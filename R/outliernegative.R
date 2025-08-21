# ===============================================================
# Data Cleaning & Quality Control for BER Master Dataset
#
# This script prepares the raw BER dataset for modeling:
#   1. Loads input CSV and identifies CO₂, energy, and categorical fields.
#   2. Generates a missingness report and fills selected categorical NAs as "Unknown".
#   3. Detects negative numeric values, replacing invalid energy/CO₂ with 0.
#   4. Caps extreme outliers (winsorisation at 99.9%) for energy-related variables.
#   5. Rebuilds total CO₂ from component parts and replaces inconsistent totals.
#   6. Creates a cleaned CO₂ target (`target_CO2_kg`) capped at the 99.9th percentile.
#   7. Summarises key variables (min, median, 99.9th percentile, max) for diagnostics.
#   8. Drops rows with >30% missing in key features.
#   9. Outputs a QC report (missingness, negatives) and saves the cleaned dataset.
#
# Outputs:
#   - "BERmaster2x.csv" : cleaned dataset ready for modeling
#   - "BERmaster2xreport.csv" : quality control report
# ===============================================================





library(data.table)

in_path  <- "data\\BERmaster1x.csv"         
out_clean_path <- "data\\BERmaster2x.csv"
out_report_path <- "data\\BERmaster2xreport.csv"

DT <- fread(in_path)
cat("Loaded:", nrow(DT), "rows and", ncol(DT), "columns\n")

co2_cols <- intersect(
  c("CO2Lighting","CO2PumpsFans","CO2MainWater","CO2MainSpace","CO2SecondarySpace"),
  names(DT)
)
total_co2_col <- intersect("Total_CO2_kg", names(DT))
energy_cols <- grep("^DeliveredEnergy|^PrimaryEnergy", names(DT), value = TRUE)

cat_cols_to_fill <- intersect(
  c("MainSpaceHeatingFuel","MainWaterHeatingFuel","VentilationMethod"),
  names(DT)
)

num_cols <- names(DT)[sapply(DT, is.numeric)]
num_cols_energy_like <- unique(c(co2_cols, energy_cols))

miss_dt <- data.table(
  column = names(DT),
  missing_count = sapply(DT, function(x) sum(is.na(x))),
  missing_pct = sapply(DT, function(x) mean(is.na(x)) * 100)
)[order(-missing_count)]

cat("\nTop 15 columns by missing values:\n")
print(head(miss_dt, 15))

# Fill selected categoricals with "Unknown"
if (length(cat_cols_to_fill)) {
  for (cc in cat_cols_to_fill) {
    set(DT, which(is.na(DT[[cc]])), cc, "Unknown")
  }
}

neg_report <- data.table(
  column = num_cols,
  negative_count = sapply(DT[, ..num_cols], function(x) sum(x < 0, na.rm = TRUE))
)[negative_count > 0][order(-negative_count)]

cat("\nNumeric columns with negatives (top 20):\n")
print(head(neg_report, 20))

if (length(num_cols_energy_like)) {
  for (cc in num_cols_energy_like) {
    bad_idx <- which(!is.na(DT[[cc]]) & DT[[cc]] < 0)
    if (length(bad_idx)) set(DT, bad_idx, cc, 0)
  }
}

winsor_cap <- function(x, p = 0.999) {
  if (!is.numeric(x)) return(x)
  if (all(is.na(x))) return(x)
  cap <- as.numeric(quantile(x, probs = p, na.rm = TRUE, names = FALSE))
  x[x > cap] <- cap
  x
}

if (length(num_cols_energy_like)) {
  DT[, (num_cols_energy_like) := lapply(.SD, winsor_cap, p = 0.999), .SDcols = num_cols_energy_like]
}

if (length(co2_cols) == 5) {
  DT[, Total_CO2_kg_rebuilt := rowSums(.SD, na.rm = TRUE), .SDcols = co2_cols]
  
  if (length(total_co2_col)) {
    orig <- DT[[total_co2_col]]
    rebuilt <- DT[["Total_CO2_kg_rebuilt"]]
    
    rel_diff <- rep(NA_real_, length(orig))
    idx_pos <- which(!is.na(orig) & orig > 0)
    rel_diff[idx_pos] <- abs(rebuilt[idx_pos] - orig[idx_pos]) / orig[idx_pos]
    
    replace_idx <- which(is.na(orig) | (rel_diff > 0.10))  # >10% deviation
    DT[, Total_CO2_kg_clean := fifelse(.I %in% replace_idx, rebuilt, orig)]
  } else {
    DT[, Total_CO2_kg_clean := Total_CO2_kg_rebuilt]
  }
  
  DT[, co2_total_replaced_flag := as.integer(Total_CO2_kg_clean != get(total_co2_col))]
} else {
  warning("Expected 5 CO2 breakdown columns; skipping total rebuild.")
}

table(DT$co2_total_replaced_flag)  # 1 = replaced with rebuilt, 0 = kept original

orig_vs_clean <- data.table(
  metric = c("min","p50","p999","max"),
  original = c(min(DT$Total_CO2_kg, na.rm=TRUE),
               quantile(DT$Total_CO2_kg, 0.5,   na.rm=TRUE),
               quantile(DT$Total_CO2_kg, 0.999, na.rm=TRUE),
               max(DT$Total_CO2_kg, na.rm=TRUE)),
  clean = c(min(DT$Total_CO2_kg_clean, na.rm=TRUE),
            quantile(DT$Total_CO2_kg_clean, 0.5,   na.rm=TRUE),
            quantile(DT$Total_CO2_kg_clean, 0.999, na.rm=TRUE),
            max(DT$Total_CO2_kg_clean, na.rm=TRUE))
)
print(orig_vs_clean)

cap99 <- as.numeric(quantile(DT$Total_CO2_kg_clean, 0.999, na.rm = TRUE))
DT[, target_CO2_kg := fifelse(Total_CO2_kg_clean > cap99, cap99, Total_CO2_kg_clean)]

data.table(
  min = min(DT$target_CO2_kg, na.rm=TRUE),
  p50 = quantile(DT$target_CO2_kg, 0.5, na.rm=TRUE),
  p999 = quantile(DT$target_CO2_kg, 0.999, na.rm=TRUE),
  max = max(DT$target_CO2_kg, na.rm=TRUE)
)

sel_parts  <- intersect(co2_cols, names(DT))
sel_energy <- intersect(energy_cols, names(DT))
sel_totals <- intersect(c("Total_CO2_kg","Total_CO2_kg_clean","target_CO2_kg"), names(DT))

summ_vec <- function(x) {
  mn   <- suppressWarnings(as.numeric(min(x, na.rm = TRUE)))
  q50  <- suppressWarnings(as.numeric(stats::quantile(x, 0.5,   na.rm = TRUE)))
  q999 <- suppressWarnings(as.numeric(stats::quantile(x, 0.999, na.rm = TRUE)))
  mx   <- suppressWarnings(as.numeric(max(x, na.rm = TRUE)))
  c(min = mn, p50 = q50, p999 = q999, max = mx)
}

summarise_cols <- function(cols) {
  rbindlist(lapply(cols, function(cc) {
    vals <- summ_vec(DT[[cc]])
    data.table(column = cc, min = vals["min"], p50 = vals["p50"],
               p999 = vals["p999"], max = vals["max"])
  }))
}

summ_parts  <- if (length(sel_parts))  summarise_cols(sel_parts)  else data.table()
summ_totals <- if (length(sel_totals)) summarise_cols(sel_totals) else data.table()
summ_energy <- if (length(sel_energy)) summarise_cols(sel_energy) else data.table()

summary_after <- rbind(summ_parts, summ_totals, summ_energy, fill = TRUE)

print(summary_after[column %in% c("Total_CO2_kg","Total_CO2_kg_clean","target_CO2_kg")])

candidate_feats <- unique(c(
  co2_cols, total_co2_col, energy_cols,
  intersect(c("TotalFloorArea","UValueWall","UValueRoof","UValueWindow","BerRating"), names(DT))
))
candidate_feats <- candidate_feats[candidate_feats %in% names(DT)]
row_na_pct <- DT[, rowMeans(is.na(.SD)), .SDcols = candidate_feats]
drop_idx <- which(row_na_pct > 0.30)
cat("\nRows to drop by >30% NA in key features:", length(drop_idx), "\n")
if (length(drop_idx)) DT <- DT[-drop_idx]

qc_report <- rbind(
  data.table(type = "missingness", miss_dt),
  data.table(type = "negatives", column = neg_report$column, missing_count = NA_integer_,
             missing_pct = NA_real_)[, negative_count := neg_report$negative_count],
  fill = TRUE
)

fwrite(qc_report, out_report_path)
fwrite(DT, out_clean_path)

cat("\n==== Done ====\n")
cat("Saved QC report ->", out_report_path, "\n")
cat("Saved cleaned dataset ->", out_clean_path, "\n")
