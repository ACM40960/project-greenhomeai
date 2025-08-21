# =========================================
# GreenHomeAI — Energy & CO₂ Totals Script
#
# This script computes total delivered energy,
# total primary energy, and total CO₂ emissions
# from their component columns in the BER dataset.
#
# Workflow:
#  - Load cleaned BER dataset (BERmaster1.csv)
#  - Convert energy & CO₂ columns to numeric
#  - Calculate row-wise totals for Delivered, Primary, CO₂
#  - Save augmented dataset as BERmaster1x.csv
# =========================================




infile  <- "data\\BERmaster1.csv"
outfile <- "data\\BERmaster1x.csv"

df <- read.csv(infile, check.names = FALSE, stringsAsFactors = FALSE)

delivered_cols <- c(
  "DeliveredEnergyMainSpace","DeliveredEnergyMainWater",
  "DeliveredLightingEnergy","DeliveredEnergyPumpsFans",
  "DeliveredEnergySecondarySpace"
)

primary_cols <- c(
  "PrimaryEnergyMainSpace","PrimaryEnergyMainWater",
  "PrimaryEnergyLighting","PrimaryEnergyPumpsFans",
  "PrimaryEnergySecondarySpace","PrimaryEnergySupplementaryWater"
)

co2_cols <- c(
  "CO2MainSpace","CO2MainWater","CO2Lighting",
  "CO2PumpsFans","CO2SecondarySpace"
)

need <- c(delivered_cols, primary_cols, co2_cols)
missing <- setdiff(need, names(df))
if(length(missing)) stop("Missing columns: ", paste(missing, collapse=", "))

to_num <- function(x) suppressWarnings(as.numeric(x))
for (v in need) df[[v]] <- to_num(df[[v]])

df$Total_Delivered_kWh <- rowSums(df[delivered_cols], na.rm = TRUE)
df$Total_Primary_kWh   <- rowSums(df[primary_cols],   na.rm = TRUE)
df$Total_CO2_kg        <- rowSums(df[co2_cols],       na.rm = TRUE)

print(head(df[c("Total_Delivered_kWh","Total_Primary_kWh","Total_CO2_kg")]))

write.csv(df, outfile, row.names = FALSE)
cat("Saved:", outfile, "\n")
