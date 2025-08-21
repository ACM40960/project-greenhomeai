# ===============================================================
# Part 5 — Upgrade Economics (ΔCO₂ → kWh → € Saved)
#
# This script links predicted CO₂ savings from upgrade scenarios
# to economic impacts (energy use and € savings).
#
# Workflow:
#   1. Load baseline, wide-format ΔCO₂ scenarios, emission factors, and tariffs.
#   2. Reshape scenarios to long format (upgrade, delta_CO₂).
#   3. Normalize upgrade names and clean negative values.
#   4. Map household fuels to emission factor (kgCO₂/kWh) and tariff (€ per kWh).
#   5. Compute:
#        - ΔkWh avoided = ΔCO₂ / emission factor
#        - € saved/year = ΔkWh * tariff
#   6. Save detailed per-row results and summary by upgrade type.
#
# Outputs:
#   - outputs/tables/part5_long_with_economics.csv
#   - outputs/tables/part5_economics_by_upgrade.csv
# ===============================================================





suppressPackageStartupMessages({
  library(data.table)
})

clean <- function(x) gsub("[^a-z0-9]+","_", tolower(trimws(x)))

dir.create("outputs/tables", showWarnings = FALSE, recursive = TRUE)

baseline_path <- "outputs/mini_universe_baseline.csv"
wide_path     <- "outputs/part4_scenarios_deltaCO2_wide.csv"

emf_path <- "data/Emission_factors.csv" 
trf_path <- "data/Tariffs.csv"            

stopifnot(file.exists(baseline_path), file.exists(wide_path),
          file.exists(emf_path), file.exists(trf_path))

base <- fread(baseline_path)
wide <- fread(wide_path)

if ("row_id" %in% names(wide) && !("row_id" %in% names(base))) {
  if (nrow(base) != nrow(wide)) {
    stop("wide has row_id but baseline does not, and row counts differ. Provide a shared ID.")
  }
  base[, row_id := .I]
}
if (!("row_id" %in% names(base) && "row_id" %in% names(wide))) {
  stop("Need 'row_id' in both mini_universe_baseline.csv and part4_scenarios_deltaCO2_wide.csv")
}
id_col <- "row_id"
setkeyv(base, id_col); setkeyv(wide, id_col)

delta_cols <- setdiff(names(wide), id_col)
delta_cols <- delta_cols[grepl("insulation|window|heating", delta_cols, ignore.case = TRUE)]
stopifnot(length(delta_cols) >= 2)

long <- melt(
  wide[, c(id_col, delta_cols), with = FALSE],
  id.vars = id_col,
  variable.name = "upgrade_raw",
  value.name   = "delta_co2"
)

normalize_upgrade <- function(s){
  s <- tolower(gsub("[^a-z0-9]+","_", s))
  s <- sub("^delta_", "", s)
  s <- sub("_kg$", "", s)
  s
}
long[, upgrade := vapply(upgrade_raw, normalize_upgrade, character(1))]
long[, delta_co2 := pmax(delta_co2, 0)]

emf <- fread(emf_path)    
trf <- fread(trf_path)    

if ("euro_per_kWh" %in% names(trf)) setnames(trf, "euro_per_kWh", "euro_per_kwh")

stopifnot(all(c("fuel","kgco2_per_kwh") %in% names(emf)))
stopifnot(all(c("fuel","euro_per_kwh")   %in% names(trf)))

if (!("MainSpaceHeatingFuel" %in% names(base))) {
  stop("'MainSpaceHeatingFuel' column not found in baseline.")
}

trf[, fuel := clean(fuel)]
trf_plain <- unique(trf[fuel %in% c("electricity","gas","heating_oil"),
                        .(fuel_tariff = fuel, euro_per_kwh)], by = "fuel_tariff")

emf[, fuel := clean(fuel)]
setnames(emf, "fuel", "fuel_emf")

to_tariff_key <- function(v){
  v <- tolower(trimws(v)); v <- gsub("\\s+"," ", v)
  out <- v
  out[out %in% c("mains gas","gas","natural gas")] <- "gas"
  out[out %in% c("heating oil","kerosene")]        <- "heating_oil"
  out[out %in% c("electricity","electric")]        <- "electricity"
  clean(out)
}
to_emf_key <- function(v){
  v <- tolower(trimws(v)); v <- gsub("\\s+"," ", v)
  out <- v
  out[out %in% c("mains gas","gas","natural gas")] <- "natural_gas_ncv_"
  out[out %in% c("heating oil","kerosene")]        <- "heating_oil"
  out[out %in% c("electricity","electric")]        <- "electricity"
  clean(out)
}

fuel_dt <- base[, .(id = get(id_col),
                    fuel_tariff = to_tariff_key(MainSpaceHeatingFuel),
                    fuel_emf    = to_emf_key(MainSpaceHeatingFuel))]

econ <- merge(long[, .(id = get(id_col), upgrade, delta_co2)],
              fuel_dt, by = "id", all.x = TRUE)
econ <- merge(econ, emf[, .(fuel_emf, kgco2_per_kwh)],    by = "fuel_emf",    all.x = TRUE)
econ <- merge(econ, trf_plain[, .(fuel_tariff, euro_per_kwh)],
              by = "fuel_tariff", all.x = TRUE)

econ[, delta_kwh := fifelse(kgco2_per_kwh > 0, delta_co2 / kgco2_per_kwh, NA_real_)]
econ[, euro_saved_per_year := delta_kwh * euro_per_kwh]

fwrite(econ, "outputs/tables/part5_long_with_economics.csv")

econ_by <- econ[, .(
  n = .N,
  mean_delta_co2  = mean(delta_co2, na.rm = TRUE),
  mean_euro_saved = mean(euro_saved_per_year, na.rm = TRUE)
), by = upgrade][order(-mean_delta_co2)]

fwrite(econ_by, "outputs/tables/part5_economics_by_upgrade.csv")

message("Part 5 done. Wrote:",
        "\n - outputs/tables/part5_long_with_economics.csv",
        "\n - outputs/tables/part5_economics_by_upgrade.csv")
