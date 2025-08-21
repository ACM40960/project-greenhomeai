# ===============================================================
# Part 4 — Build Mini-Universe (Stratified Sample, Strict Align)
#
# Purpose:
#   Create a smaller “mini-universe” dataset representing the full BER data,
#   ensuring upgrade analysis and scenarios can run efficiently but still
#   cover all strata of dwellings and CO₂ levels.
#
# Workflow:
#   1. Load BER dataset, trained model, metadata, and one-hot feature names.
#   2. Engineer physics proxy features (wall, roof, floor, window heat loss).
#   3. Encode features with strict one-hot alignment to Part 3 training schema.
#   4. Predict baseline CO₂ emissions for all dwellings.
#   5. Create CO₂ deciles and stratify by county, dwelling type, age band, and CO₂ bin.
#   6. Sample ~80 dwellings per cell (min 20, cap 20k overall), with fallback rules.
#   7. Save:
#        - mini_universe_baseline.csv : sampled dwellings + key features
#        - mini_universe_coverage.csv : sample counts by strata
#        - mini_universe_row_index.csv : indices used from full dataset
#
# Outputs:
#   - outputs/mini_universe_baseline.csv
#   - outputs/mini_universe_coverage.csv
#   - outputs/mini_universe_row_index.csv
# ===============================================================




suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(lightgbm)
})

ber_path   <- "data/BERmaster2x.csv"
model_txt  <- "models/lightgbm_upgrade_friendly.txt"
meta_rds   <- "models/lightgbm_upgrade_friendly_meta.rds"
onehot_rds <- "models/p3_onehot_feature_names.rds"
out_dir    <- "outputs"

N_PER_CELL    <- 80
GLOBAL_CAP    <- 20000
MIN_PER_CELL  <- 20
BACKOFF_MIN_N <- 5000

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(ber_path)
setnames(dt, old = names(dt), new = make.names(names(dt), unique = TRUE))

county_col <- if ("County" %in% names(dt)) "County" else if ("CountyName" %in% names(dt)) "CountyName" else NA_character_
if (is.na(county_col)) stop("Neither 'County' nor 'CountyName' in data.")
dwtype_col <- if ("DwellingTypeDescr" %in% names(dt)) "DwellingTypeDescr" else stop("'DwellingTypeDescr' not found.")
age_col    <- if ("AgeBand" %in% names(dt)) "AgeBand" else if ("ThermalEra" %in% names(dt)) "ThermalEra" else stop("Neither 'AgeBand' nor 'ThermalEra' found.")

if (!file.exists(meta_rds)) stop("Meta RDS not found: models/lightgbm_upgrade_friendly_meta.rds")
meta <- readRDS(meta_rds)
if (is.null(meta$features_used)) stop("meta$features_used missing.")
feature_cols <- intersect(meta$features_used, names(dt))
if (!length(feature_cols)) stop("None of meta$features_used exist in current data.")

has <- function(cols) all(cols %in% names(dt))
if (has(c("WallArea","UValueWall")))               dt[, WallHeatLossProxy   := WallArea              * UValueWall]
if (has(c("RoofArea","UValueRoof")))               dt[, RoofHeatLossProxy   := RoofArea              * UValueRoof]
if (has(c("GroundFloorArea.sq.m.","UValueFloor"))) dt[, FloorHeatLossProxy  := GroundFloorArea.sq.m. * UValueFloor]
if (has(c("WindowArea","UValueWindow")))           dt[, WindowHeatLossProxy := WindowArea            * UValueWindow]
engineered <- intersect(c("WallHeatLossProxy","RoofHeatLossProxy","FloorHeatLossProxy","WindowHeatLossProxy"), names(dt))
feature_cols <- unique(c(feature_cols, engineered))

if (!file.exists(model_txt)) stop(sprintf("Model file not found: %s", model_txt))
booster_p3 <- lgb.load(model_txt)

if (!file.exists(onehot_rds)) stop("Missing models/p3_onehot_feature_names.rds (save it in Part 3).")
onehot_names <- readRDS(onehot_rds)
if (!length(onehot_names)) stop("p3_onehot_feature_names.rds is empty.")

Xdf <- dt[, ..feature_cols]
char_cols <- names(Xdf)[vapply(Xdf, is.character, logical(1))]
for (cc in char_cols) Xdf[[cc]] <- factor(Xdf[[cc]])
Xm <- Matrix::sparse.model.matrix(~ . - 1, data = as.data.frame(Xdf))
cn <- colnames(Xm); cn <- gsub("[^A-Za-z0-9_]", "_", cn); cn <- make.unique(cn, sep = "_"); colnames(Xm) <- cn

have <- intersect(onehot_names, colnames(Xm))
if (length(have) == 0) stop("No overlap between encoded columns and training one‑hot names.")
Xm <- Xm[, onehot_names[onehot_names %in% have], drop = FALSE]
missing <- setdiff(onehot_names, colnames(Xm))
if (length(missing)) {
  add <- Matrix(0, nrow = nrow(Xm), ncol = length(missing), sparse = TRUE)
  colnames(add) <- missing
  Xm <- Matrix::cbind2(Xm, add)
}
Xm <- Xm[, onehot_names, drop = FALSE]
if (!inherits(Xm, "dgCMatrix")) Xm <- as(Xm, "dgCMatrix")

pred_log <- predict(booster_p3, Xm)  # shape & order match training
dt[, CO2_baseline := pmax(0, expm1(as.numeric(pred_log)))]
if (!any(is.finite(dt$CO2_baseline))) stop("All baseline predictions NA/Inf.")

qs <- unique(quantile(dt$CO2_baseline[is.finite(dt$CO2_baseline)], probs = seq(0,1,0.1), na.rm = TRUE, type = 7))
if (length(qs) < 3) qs <- unique(quantile(dt$CO2_baseline, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))
dt[, CO2_decile := cut(CO2_baseline, breaks = qs, include.lowest = TRUE, right = TRUE)]

strata_full <- c(county_col, dwtype_col, age_col, "CO2_decile")
present_strata <- strata_full[strata_full %in% names(dt)]
if (length(present_strata) < 2) stop("Not enough strata columns.")

strat_sample <- function(DT, strata, n_per_cell = 80L, min_per_cell = 20L, global_cap = 20000L) {
  DT <- copy(DT); setkeyv(DT, strata); DT[, rn__ := .I]
  sampled_idx <- DT[, { n <- .N; if (n <= min_per_cell) .SD$rn__ else .SD$rn__[sample.int(n, size = min(n, n_per_cell))] }, by = strata]
  total <- nrow(sampled_idx)
  if (total > global_cap) {
    sampled_idx <- sampled_idx[, { n <- .N; keep <- max(1L, floor(n * global_cap / total)); .SD[sample.int(n, size = keep)] }, by = strata]
  }
  sort(sampled_idx$V1)
}

idx <- strat_sample(dt, present_strata, n_per_cell = N_PER_CELL, min_per_cell = MIN_PER_CELL, global_cap = GLOBAL_CAP)
used_strata <- present_strata
if (length(idx) < BACKOFF_MIN_N) {
  present_strata2 <- setdiff(present_strata, "CO2_decile")
  if (length(present_strata2) < 2) stop("Back‑off failed: too few strata columns.")
  idx <- strat_sample(dt, present_strata2, n_per_cell = N_PER_CELL, min_per_cell = MIN_PER_CELL, global_cap = GLOBAL_CAP)
  used_strata <- present_strata2
}

mini <- dt[idx]

covg <- mini[, .N, by = used_strata][order(-N)]
physics_cols <- intersect(c(
  "UValueWall","UValueRoof","UValueFloor","UValueWindow",
  "WallArea","RoofArea","GroundFloorArea","WindowArea",
  "HSMainSystemEfficiency","TempAdjustment","FloorArea",
  "WallHeatLossProxy","RoofHeatLossProxy","FloorHeatLossProxy","WindowHeatLossProxy"
), names(mini))
keep_cols_out <- unique(c(feature_cols, used_strata, "CO2_decile", "CO2_baseline", physics_cols))
mini_out <- mini[, ..keep_cols_out]

fwrite(covg,     file.path(out_dir, "mini_universe_coverage.csv"))
fwrite(mini_out, file.path(out_dir, "mini_universe_baseline.csv"))
fwrite(data.table(row_index_in_full = idx), file.path(out_dir, "mini_universe_row_index.csv"))

cat("Saved:\n - outputs/mini_universe_baseline.csv\n - outputs/mini_universe_coverage.csv\n - outputs/mini_universe_row_index.csv\nDone.\n")
