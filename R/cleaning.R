# =========================================
# GreenHomeAI — Data Cleaning & Preprocessing
#
# This script cleans the raw BER dataset (BERdata.csv)
# and prepares it for feature engineering and modeling.
# Key steps:
#  - Standardize missing values and trim whitespace
#  - Drop empty / irrelevant columns and rows
#  - Remove duplicates and convert numeric-like text to numbers
#  - Handle missing values (median for numeric, mode for categorical)
#  - Drop or impute columns with high NA percentages
#  - Detect and flag outliers using IQR rules
#  - Save cleaned datasets (bermaster.csv, bermaster1.csv)
# =========================================



library(data.table)

f <- "data\\BERdata.csv"


per <- fread(f, na.strings = c("", "NA", "NaN", "NULL", " ", "  "))
cat("Raw dim:", dim(per), "\n")

char_cols <- names(per)[sapply(per, is.character)]
if (length(char_cols))
  per[, (char_cols) := lapply(.SD, trimws), .SDcols = char_cols]


miss_col <- colMeans(is.na(per))
keep_cols <- miss_col < 0.95
per <- per[, ..keep_cols]

miss_row <- rowMeans(is.na(per))
per <- per[miss_row < 0.95]
cat("After empty col/row drop:", dim(per), "\n")




safe_to_drop <- c(
  "FirstEnergyTypeId", "FirstEnergyType_Description", "FirstEnerProdComment",
  "FirstEnerProdDelivered", "FirstPartLTotalContribution", "FirstEnerProdConvFactor",
  "FirstEnerProdCO2EmissionFactor", "FirstEnerConsumedDelivered", "FirstEnerConsumedConvFactor",
  "FirstEnerConsumedCO2EmissionFactor",
  "SecondEnergyTypeId", "SecondEnergyType_Description", "SecondEnerProdDelivered",
  "SecondPartLTotalContribution", "SecondEnerProdConvFactor", "SecondEnerProdCO2EmissionFactor",
  "SecondEnerConsumedDelivered", "SecondEnerConsumedConvFactor", "SecondEnerConsumedCO2EmissionFactor",
  "ThirdEnergyTypeId", "ThirdEnergyType_Description", "ThirdEnerProdDelivered",
  "ThirdPartLTotalContribution", "ThirdEnerProdConvFactor", "ThirdEnerProdCO2EmissionFactor",
  "ThirdEnerConsumedDelivered", "ThirdEnerConsumedConvFactor", "ThirdEnerConsumedCO2EmissionFactor",
  
  "FirstWallType_Description", "FirstWallArea", "FirstWallUValue", "FirstWallIsSemiExposed",
  "FirstWallAgeBandId", "FirstWallTypeId", "SecondWallType_Description", "SecondWallArea",
  "SecondWallUValue", "SecondWallIsSemiExposed", "SecondWallAgeBandId", "SecondWallTypeId",
  "ThirdWallType_Description", "ThirdWallArea", "ThirdWallUValue", "ThirdWallIsSemiExposed",
  "ThirdWallAgeBandId", "ThirdWallTypeId",
  
  "SA_Code", "UIDHex", "prob_smarea_error_0corr", "prob_smarea_error_100corr"
)


drop_cols <- intersect(safe_to_drop, names(per))
if (length(drop_cols) > 0) {
  per[, (drop_cols) := NULL]
  cat("Dropped", length(drop_cols), "columns not needed for project scope.\n")
} else {
  cat("No safe-to-drop columns found in dataset.\n")
}



missing_summary <- data.table(
  Column = names(per),
  MissingCount = sapply(per, function(x) sum(is.na(x))),
  MissingPercent = round(sapply(per, function(x) mean(is.na(x)) * 100), 2)
)

missing_summary <- missing_summary[order(-MissingPercent)]

head(missing_summary, 20)











cols_to_drop <- c("EPC", "CPC", "RenewEPren","RenewEPnren","RER")
per[, (cols_to_drop) := NULL]

names(per)

missing_summary2 <- data.table(
  Column = names(per),
  MissingCount = sapply(per, function(x) sum(is.na(x))),
  MissingPercent = round(sapply(per, function(x) mean(is.na(x)) * 100), 2)
)
missing_summary2 <- missing_summary2[order(-MissingPercent)]
cat("Rows/Cols:", nrow(per), ncol(per), "\n")
print(head(missing_summary2, 20))


id_cols <- intersect(c("BERNo","BERNumber","MPRN","UPRN"), names(per))
if (length(id_cols)) {
  dup <- per[, .N, by = id_cols][N > 1]
  cat("Duplicate ID groups:", nrow(dup), "\n")
} else {
  cat("No known ID columns found; checking full-row dups.\n")
  cat("Exact duplicate rows:", per[duplicated(per), .N], "\n")
}



per <- unique(per)

cat("After duplicate removal:", nrow(per), "rows\n")


num_like <- names(per)[sapply(per, function(x)
  is.character(x) && mean(grepl("^ *-?\\d+(\\.\\d+)? *$", x %>% trimws), na.rm=TRUE) > 0.8)]
if (length(num_like))
  per[, (num_like) := lapply(.SD, function(x) as.numeric(trimws(x))), .SDcols = num_like]


names(per)


safe_to_drop2 <- c(


  "StorageLosses", "ManuLossFactorAvail", "SolarHotWaterHeating", "ElecImmersionInSummer",
  "CombiBoiler", "KeepHotFacility", "WaterStorageVolume", "DeclaredLossFactor",
  "TempFactorUnadj", "TempFactorMultiplier", "InsulationType", "InsulationThickness",
  "PrimaryCircuitLoss", "CombiBoilerAddLoss", "ElecConsumpKeepHot",

  "PurposeOfRating", "DateOfAssessment", "MultiDwellingMPRN",
  
  "PredominantRoofTypeArea", "PredominantRoofType",
  "RoomInRoofArea", "GlazingPercent", "Volume"
)

drop_cols <- intersect(safe_to_drop2, names(per))
if (length(drop_cols) > 0) {
  per[, (drop_cols) := NULL]
  cat("Dropped", length(drop_cols), "columns from dataset.\n")
} else {
  cat("No matching safe-to-drop columns found in dataset.\n")
}


names(per)



fwrite(per, "data\\bermaster.csv")






library(data.table)
library(ggplot2)
library(corrplot)

per <- fread("data\\bermaster.csv")



missing_summary <- data.table(
  Column = names(per),
  MissingCount = sapply(per, \(x) sum(is.na(x))),
  MissingPercent = round(sapply(per, \(x) mean(is.na(x)) * 100), 2)
)[order(-MissingPercent)]
print(missing_summary[MissingCount > 0])  

drop50 <- c("TotalDeliveredEnergy",
            "DeliveredEnergySupplementaryWater",
            "CO2SupplementaryWater")

exists50 <- intersect(drop50, names(per))
per[, (exists50) := NULL]
cat("Dropped:", paste(exists50, collapse=", "), "\n")




cols15 <- c("CO2SecondarySpace", "DeliveredEnergySecondarySpace")

for (col in cols15) {
  if (col %in% names(per)) {
    med_val <- median(per[[col]], na.rm = TRUE)
    per[is.na(get(col)), (col) := med_val]
    cat("Imputed", sum(is.na(per[[col]])), "NAs in", col, "with median =", med_val, "\n")
  }
}

cols3 <- c(
  "DeliveredLightingEnergy", "DeliveredEnergyPumpsFans",
  "CO2Lighting", "CO2PumpsFans",
  "CO2MainWater", "CO2MainSpace",
  "DeliveredEnergyMainWater", "DeliveredEnergyMainSpace"
)

for (col in cols3) {
  if (col %in% names(per)) {
    med_val <- median(per[[col]], na.rm = TRUE)
    per[is.na(get(col)), (col) := med_val]
    cat("Imputed", sum(is.na(per[[col]])), "NAs in", col, "with median =", med_val, "\n")
  }
}

mode_type_safe <- function(x) {
  ux <- x[!is.na(x)]
  if (!length(ux)) return(NA)
  m <- names(which.max(table(ux)))
  # return as same type as input
  if (is.integer(x)) return(as.integer(m))
  if (is.numeric(x)) return(as.numeric(m))
  return(m)  # character/factor
}



cols15_num <- c("HSEffAdjFactor", "HSSupplHeatFraction", "HSSupplSystemEff",
                "WHEffAdjFactor", "PrimaryEnergyLighting",
                "PrimaryEnergyPumpsFans", "PrimaryEnergySupplementaryWater")

cols15_cat <- c("SupplSHFuel", "SupplWHFuel",
                "SHRenewableResources", "WHRenewableResources")

for (col in cols15_num) {
  if (col %in% names(per)) {
    med_val <- median(per[[col]], na.rm = TRUE)
    per[is.na(get(col)), (col) := med_val]
    cat("Imputed", sum(is.na(per[[col]])), "NAs in", col, "with median =", med_val, "\n")
  }
}

for (col in cols15_cat) {
  if (col %in% names(per)) {
    mode_val <- names(sort(table(per[[col]]), decreasing = TRUE))[1]
    per[is.na(get(col)), (col) := mode_val]
    cat("Imputed", sum(is.na(per[[col]])), "NAs in", col, "with mode =", mode_val, "\n")
  }
}

tiny_num <- c("PrimaryEnergyMainWater","PrimaryEnergyMainSpace","PrimaryEnergySecondarySpace",
              "FanPowerManuDeclaredValue","HeatExchangerEff","PercentageDraughtStripped")
tiny_int_counts <- c("NoOfChimneys","NoOfOpenFlues","NoOfFansAndVents","NoOfFluelessGasFires",
                     "NoOfSidesSheltered")
tiny_cat <- c("PermeabilityTestResult","TempAdjustment")

for (col in intersect(tiny_num, names(per))) {
  med <- median(per[[col]], na.rm=TRUE)
  per[is.na(get(col)), (col) := med]
}

for (col in intersect(tiny_int_counts, names(per))) {
  if (is.integer(per[[col]])) {
    per[is.na(get(col)), (col) := 0L]
  } else {
    per[is.na(get(col)), (col) := 0]
  }
}

for (col in intersect(tiny_cat, names(per))) {
  mv <- mode_type_safe(per[[col]])
  per[is.na(get(col)), (col) := mv]
}

leftover <- sapply(per, function(x) sum(is.na(x)))
cat("Total NAs after cleanup:", sum(leftover), "\n")
sort(leftover[leftover>0], decreasing=TRUE)


















missing_summary <- data.table(
  Column = names(per),
  MissingCount = sapply(per, \(x) sum(is.na(x))),
  MissingPercent = round(sapply(per, \(x) mean(is.na(x))*100), 2)
)[order(-MissingPercent)]
head(missing_summary, 10)

fwrite(per, "data\\bermaster1.csv")




















num_cols <- names(per)[sapply(per, is.numeric)]

outlier_report <- lapply(num_cols, function(col) {
  vals <- per[[col]]
  q1 <- quantile(vals, 0.25, na.rm=TRUE)
  q3 <- quantile(vals, 0.75, na.rm=TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(vals < lower | vals > upper, na.rm=TRUE)
})

outlier_df <- data.table(Column=num_cols, OutlierCount=unlist(outlier_report))
outlier_df <- outlier_df[order(-OutlierCount)]
print(outlier_df[OutlierCount > 0])


fwrite(per, "data\\bermaster1.csv")

















flag_outlier <- function(x) {
  q1 <- quantile(x, 0.25, na.rm=TRUE)
  q3 <- quantile(x, 0.75, na.rm=TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  as.integer(x < lower | x > upper)
}

top_outlier_cols <- outlier_df$Column[1:20]  
for (col in top_outlier_cols) {
  if (col %in% names(per) && is.numeric(per[[col]])) {
    per[[paste0(col, "_OutlierFlag")]] <- flag_outlier(per[[col]])
  }
}


per[UValueWall < 0 | UValueWall > 5, UValueWall := median(UValueWall, na.rm=TRUE)]




