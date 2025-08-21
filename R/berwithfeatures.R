# =========================================
# GreenHomeAI — Feature Engineering Script
#
# This script processes the raw BER dataset (bermaster1.csv)
# and creates engineered features for modeling CO₂ emissions.
# Key steps:
#  - Define a safe ratio helper to avoid divide-by-zero/NA
#  - Create ratios (glazing, living area) and total floor area
#  - Compute area-weighted U-values (walls, roof, windows)
#  - Derive heating system efficiency and renewables flags
#  - Handle missing values with median imputation by dwelling type
#  - Remove invalid rows (e.g., zero floor area)
#  - Save the cleaned dataset as ber_with_features.csv
# =========================================


library(data.table)

per <- fread("data\\bermaster1.csv")

safe_ratio <- function(num, den) ifelse(is.na(num) | is.na(den) | den == 0, NA_real_, num/den)

if (all(c("WindowArea","FloorArea") %in% names(per)))
  per[, GlazingRatio := safe_ratio(WindowArea, FloorArea)]

if ("LivingAreaPercent" %in% names(per))
  per[, LivingAreaRatio := LivingAreaPercent / 100]

sum_cols <- intersect(c("GroundFloorArea","FirstFloorArea","SecondFloorArea","ThirdFloorArea"), names(per))
if (length(sum_cols))
  per[, TotalFloorArea := rowSums(.SD, na.rm = TRUE), .SDcols = sum_cols]

need <- c("UValueWall","WallArea","UValueRoof","RoofArea","UValueWindow","WindowArea")
if (all(need %in% names(per))) {
  per[, AreaWeightedUValue :=
        safe_ratio(UValueWall*WallArea + UValueRoof*RoofArea + UValueWindow*WindowArea,
                   WallArea + RoofArea + WindowArea)]
}

hs <- intersect(c("HSMainSystemEfficiency","WHMainSystemEff"), names(per))
if (length(hs) == 2)
  per[, HeatingSystemScore := (HSMainSystemEfficiency + WHMainSystemEff) / 2]

ren_cols <- intersect(c("SHRenewableResources","WHRenewableResources"), names(per))

if (length(ren_cols)) {
  per[, HasRenewables := as.integer(
    Reduce(`|`, lapply(.SD, function(x) {
      x <- suppressWarnings(as.numeric(x))  # Convert to numeric safely
      x[is.na(x)] <- 0                       # Replace NA with 0
      x > 0                                  # TRUE if > 0
    }))
  ), .SDcols = ren_cols]
}


if ("HasRenewables" %in% names(per)) per[, HasRenewables := NULL]

names(per)


per[, GlazingRatio_missing := as.integer(is.na(GlazingRatio))]

per[, GlazingRatio := fifelse(is.na(GlazingRatio),
                              median(GlazingRatio, na.rm = TRUE),
                              GlazingRatio),
    by = DwellingTypeDescr]


per[, HeatingScore_zero := as.integer(HeatingSystemScore == 0)]
per[HeatingSystemScore == 0, HeatingSystemScore := NA]
per[, HeatingSystemScore := fifelse(is.na(HeatingSystemScore),
                                    median(HeatingSystemScore, na.rm = TRUE),
                                    HeatingSystemScore),
    by = DwellingTypeDescr]


before_drop <- nrow(per)
per <- per[TotalFloorArea != 0]
after_drop <- nrow(per)

cat("Dropped", before_drop - after_drop, "rows with zero TotalFloorArea\n")


fwrite(per, "data\\ber_with_features.csv")










