# ===============================================================
# Part 4 — Upgrade Recommendations (Top-N)
#
# This script generates the top recommended upgrades per dwelling
# based on predicted CO₂ reductions from the scenario results.
#
# Workflow:
#   1. Load wide-format ΔCO₂ scenarios (per row_id, per upgrade).
#   2. Reshape to long format: (row_id, upgrade, ΔCO₂).
#   3. Rank upgrades per dwelling by CO₂ savings (descending).
#   4. Select top-N upgrades (default: N=3).
#   5. Create:
#        - Long format table with all ranked upgrades
#        - Wide format table for convenience (top_1, top_2, top_3)
#   6. Save both outputs to disk.
#
# Outputs:
#   - outputs/part4_recommendations_long.csv
#   - outputs/part4_recommendations_top3.csv
# ===============================================================



suppressPackageStartupMessages({
  library(data.table)
})

wide_path <- "outputs/part4_scenarios_deltaCO2_wide.csv"
stopifnot(file.exists(wide_path))

dtw <- fread(wide_path)
upgrade_cols <- intersect(
  c("insulation_wall","insulation_roof","windows","heating_efficiency"),
  names(dtw)
)
if (length(upgrade_cols) == 0) stop("No upgrade columns found in the wide file.")

long <- melt(dtw, id.vars = "row_id", measure.vars = upgrade_cols,
             variable.name = "upgrade", value.name = "delta_CO2")
long[is.na(delta_CO2), delta_CO2 := 0]
setorder(long, row_id, -delta_CO2)
long[, rank := seq_len(.N), by = row_id]

TOP_N <- 3L
topn <- long[rank <= TOP_N]

topn[, score := delta_CO2]

rec_wide <- dcast(topn, row_id ~ rank, value.var = c("upgrade","delta_CO2","score"))
setnames(rec_wide,
         old = names(rec_wide),
         new = sub("^upgrade_", "top", names(rec_wide)))  # e.g., top_1, top_2, top_3

fwrite(topn,    "outputs/part4_recommendations_long.csv")
fwrite(rec_wide,"outputs/part4_recommendations_top3.csv")

cat("Saved:\n",
    " - outputs/part4_recommendations_long.csv\n",
    " - outputs/part4_recommendations_top3.csv\n", sep = "")
