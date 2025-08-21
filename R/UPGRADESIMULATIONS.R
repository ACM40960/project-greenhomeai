# ===============================================================
# Part 4 — Synthetic Upgrade Scenarios (ΔCO₂) — Strict One-Hot Align
#
# Purpose:
#   Simulate the effect of individual retrofit upgrades (walls, roof,
#   windows, heating efficiency) on household CO₂ emissions. Each
#   upgrade modifies specific proxy features, re-runs the model, and
#   computes the emission difference (ΔCO₂).
#
# Workflow:
#   1. Load mini-universe baseline sample and trained LightGBM model.
#   2. Force-include critical physics proxy features (heat-loss, efficiency).
#   3. Encode features with strict one-hot alignment (matching training).
#   4. For each upgrade:
#        - Apply a rule-based change (e.g., reduce heat loss proxy).
#        - Predict CO₂ emissions after upgrade.
#        - Calculate ΔCO₂ = before − after.
#   5. Collect results:
#        - Long format: one row per dwelling × upgrade
#        - Wide format: one row per dwelling with ΔCO₂ by upgrade
#   6. Save outputs and summary stats.
#   7. (Optional) Generate histogram plots of ΔCO₂ distributions
#        for inclusion in posters/reports.
#
# Outputs:
#   - outputs/part4_scenarios_deltaCO2_long.csv
#   - outputs/part4_scenarios_deltaCO2_wide.csv
#   - poster_plots/synthetic/*.png (upgrade distribution histograms)
# ===============================================================



suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(lightgbm)
})

mini_path  <- "outputs/mini_universe_baseline.csv"
model_txt  <- "models/lightgbm_upgrade_friendly.txt"
meta_rds   <- "models/lightgbm_upgrade_friendly_meta.rds"
onehot_rds <- "models/p3_onehot_feature_names.rds"
out_dir    <- "outputs"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dt <- fread(mini_path)
setnames(dt, old = names(dt), new = make.names(names(dt), unique = TRUE))

stopifnot(file.exists(model_txt))
booster_p3 <- lgb.load(model_txt)
stopifnot(file.exists(meta_rds))
meta <- readRDS(meta_rds); stopifnot(!is.null(meta$features_used))
stopifnot(file.exists(onehot_rds))
onehot_names <- readRDS(onehot_rds); stopifnot(length(onehot_names) > 0)

forced_cols <- c("HSMainSystemEfficiency",
                 "WallHeatLossProxy","RoofHeatLossProxy","WindowHeatLossProxy","FloorHeatLossProxy")
feature_cols <- unique(c(intersect(meta$features_used, names(dt)), intersect(forced_cols, names(dt))))
if (!length(feature_cols)) stop("No overlap between meta$features_used/forced_cols and baseline.")

encode_align <- function(DT_raw, feat_cols, onehot_names) {
  Xdf <- DT_raw[, ..feat_cols]
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
  Xm
}

predict_p3 <- function(DT_raw) {
  Xm <- encode_align(DT_raw, feature_cols, onehot_names)
  pred_log <- predict(booster_p3, Xm)  # names/order match training, no shape hacks
  pmax(0, expm1(as.numeric(pred_log)))
}

if (!("CO2_baseline" %in% names(dt))) {
  dt[, CO2_baseline := predict_p3(dt)]
}

run_walls   <- TRUE
run_roof    <- TRUE
run_windows <- TRUE
run_heating <- TRUE

apply_walls <- function(D)  { if ("WallHeatLossProxy"   %in% names(D)) D[, WallHeatLossProxy   := pmax(0, WallHeatLossProxy   * 0.70)]; D }
apply_roof  <- function(D)  { if ("RoofHeatLossProxy"   %in% names(D)) D[, RoofHeatLossProxy   := pmax(0, RoofHeatLossProxy   * 0.80)]; D }
apply_win   <- function(D)  { if ("WindowHeatLossProxy" %in% names(D)) D[, WindowHeatLossProxy := pmax(0, WindowHeatLossProxy * 0.75)]; D }
apply_heat  <- function(D)  { if ("HSMainSystemEfficiency" %in% names(D)) D[, HSMainSystemEfficiency := pmin(0.95, HSMainSystemEfficiency * 1.10)]; D }

scen_list <- list()

if (run_walls) {
  after <- apply_walls(copy(dt))
  co2_after <- predict_p3(after)
  scen_list[["walls"]] <- data.table(row_id = seq_len(nrow(dt)), upgrade = "insulation_wall",
                                     CO2_before = dt$CO2_baseline, CO2_after = co2_after,
                                     delta_CO2 = pmax(0, dt$CO2_baseline - co2_after))
}
if (run_roof) {
  after <- apply_roof(copy(dt))
  co2_after <- predict_p3(after)
  scen_list[["roof"]] <- data.table(row_id = seq_len(nrow(dt)), upgrade = "insulation_roof",
                                    CO2_before = dt$CO2_baseline, CO2_after = co2_after,
                                    delta_CO2 = pmax(0, dt$CO2_baseline - co2_after))
}
if (run_windows) {
  after <- apply_win(copy(dt))
  co2_after <- predict_p3(after)
  scen_list[["windows"]] <- data.table(row_id = seq_len(nrow(dt)), upgrade = "windows",
                                       CO2_before = dt$CO2_baseline, CO2_after = co2_after,
                                       delta_CO2 = pmax(0, dt$CO2_baseline - co2_after))
}
if (run_heating) {
  after <- apply_heat(copy(dt))
  co2_after <- predict_p3(after)
  scen_list[["heating"]] <- data.table(row_id = seq_len(nrow(dt)), upgrade = "heating_efficiency",
                                       CO2_before = dt$CO2_baseline, CO2_after = co2_after,
                                       delta_CO2 = pmax(0, dt$CO2_baseline - co2_after))
}

scen_long <- rbindlist(scen_list, use.names = TRUE, fill = TRUE)
scen_long[is.na(delta_CO2), delta_CO2 := 0]
scen_long[delta_CO2 < 0,   delta_CO2 := 0]

scen_wide <- dcast(scen_long, row_id ~ upgrade, value.var = "delta_CO2", fun.aggregate = sum)

fwrite(scen_long, file.path(out_dir, "part4_scenarios_deltaCO2_long.csv"))
fwrite(scen_wide, file.path(out_dir, "part4_scenarios_deltaCO2_wide.csv"))

print(scen_long[, .(
  n = .N,
  mean_delta = mean(delta_CO2),
  median_delta = median(delta_CO2),
  p90_delta = quantile(delta_CO2, 0.9)
), by = upgrade][order(-mean_delta)])

cat("Saved:\n - outputs/part4_scenarios_deltaCO2_long.csv\n - outputs/part4_scenarios_deltaCO2_wide.csv\n")
















suppressPackageStartupMessages({ library(ggplot2); library(scales) })
dir.create("poster_plots/synthetic", recursive = TRUE, showWarnings = FALSE)

for (u in unique(scen_long$upgrade)) {
  dd <- scen_long[upgrade == u & is.finite(delta_CO2)]
  if (!nrow(dd)) next
  p_hist <- ggplot(dd, aes(x = delta_CO2)) +
    geom_histogram(bins = 40) +
    scale_x_continuous(labels = comma) +
    labs(title = paste0(u, ": Predicted ΔCO₂ Distribution"),
         x = "Predicted ΔCO₂ (kg/yr)", y = "Count") +
    theme_minimal(base_size = 18)
  ggsave(file.path("poster_plots/synthetic", paste0(u, "_distribution.png")),
         p_hist, width = 10, height = 7, dpi = 300, bg = "white")
}
