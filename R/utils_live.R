# ============================================================
# GreenHomeAI — Live Utilities
#
# This script provides helper functions for running the app in
# live mode. It handles:
#   - Loading trained LightGBM models (baseline + deltas)
#   - Encoding user inputs into model-ready features
#   - Selecting a closest "seed" dwelling from the baseline pool
#   - Making baseline CO₂ predictions and upgrade savings (deltas)
#   - Converting CO₂ savings into kWh and € saved using tariffs
#   - Diagnostic checks to validate feature alignment
#   - Visualization helpers (waterfall, bubble, gauge plots)
#
# In short: this file is the runtime engine that connects user
# input → model predictions → economic/CO₂ impacts → visuals.
# ============================================================




suppressWarnings({
  library(data.table); library(lightgbm); library(Matrix)
  library(readr); library(readxl); library(dplyr)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b
clean  <- function(x) gsub("[^a-z0-9]+","_", tolower(trimws(as.character(x))))

resolve_file <- function(base){
  cands <- c(base, paste0(base,".txt"), paste0(base,".model"), paste0(base,".bin"))
  hit <- cands[file.exists(cands)]
  if (!length(hit)) stop(sprintf("Model file not found. Tried:\n - %s",
                                 paste(normalizePath(cands, winslash="/", mustWork=FALSE), collapse="\n - ")))
  hit[1]
}
load_lgb_any <- function(base){
  fn <- resolve_file(base)
  message("Loaded LightGBM model: ", fn)
  lightgbm::lgb.load(fn)
}
lgb_predict <- function(booster, X) {
  fun <- tryCatch(getS3method("predict", "lgb.Booster"),
                  error = function(e) get("predict.lgb.Booster", envir = asNamespace("lightgbm")))
  if (inherits(X, "ngCMatrix")) X <- methods::as(X, "dgCMatrix")
  as.numeric(fun(booster, newdata = X))
}

read_any <- function(csv, xlsx, label){
  if (file.exists(csv)) readr::read_csv(csv, show_col_types = FALSE)
  else if (file.exists(xlsx)) readxl::read_xlsx(xlsx)
  else stop(sprintf("%s missing. Looked for: %s / %s", label, csv, xlsx))
}

.flatten_charvecs <- function(x, bag=list()){
  if (is.character(x)) bag[[length(bag)+1]] <- x
  else if (is.list(x)) for (el in x) bag <- .flatten_charvecs(el, bag)
  bag
}
extract_feature_names <- function(meta, booster=NULL){
  need <- tryCatch(lightgbm::lgb.num_feature(booster), error=function(e) NA_integer_)
  keys <- c("feature_names","onehot_feature_names","train_feature_names",
            "p3_onehot_feature_names","p4_onehot_feature_names","col_names",
            "colnames","X_names","input_names","feature_names_order")
  for (k in keys) {
    v <- meta[[k]]
    if (is.character(v) && (is.na(need) || length(v)==need)) return(v)
  }
  cands <- .flatten_charvecs(meta)
  if (!is.na(need)) {
    hit <- Filter(function(v) length(v)==need, cands)
    if (length(hit)) return(hit[[1]])
  }
  if (length(cands)) return(cands[[which.max(vapply(cands, length, 1L))]])
  NULL
}

encode_row_to_p3 <- function(seed_row, inputs, feats){
  v <- setNames(numeric(length(feats)), feats)
  set1 <- function(nm, val){ if (nm %in% names(v)) v[[nm]] <<- as.numeric(val) }
  
  num_map <- c("Year_of_Construction"="year_built",
               "FloorArea"="floor_area_m2",
               "TotalFloorArea"="floor_area_m2",
               "GroundFloorArea.sq.m."="floor_area_m2",
               "NoStoreys"="no_storeys")
  for (k in names(num_map)){
    if (k %in% names(v)) {
      val <- inputs[[ num_map[[k]] ]]
      if (is.null(val) && k %in% names(seed_row)) val <- seed_row[[k]]
      set1(k, val %||% 0)
    }
  }
  for (col in c("CountyName","DwellingTypeDescr","MainSpaceHeatingFuel","VentilationMethod","StructureType")){
    if (col %in% names(seed_row)){
      key <- paste0(clean(col), "_", clean(seed_row[[col]]))
      if (key %in% names(v)) set1(key, 1)
    }
  }
  for (kv in list(
    paste0("mainspaceheatingfuel_", clean(inputs$main_fuel)),
    paste0("dwellingtypedescr_",   clean(inputs$dwelling_type)),
    paste0("countyname_",          clean(inputs$county))
  )) if (kv %in% names(v)) set1(kv, 1)
  
  if ("wall_insulation_present" %in% names(v)) set1("wall_insulation_present", as.numeric(inputs$has_wall))
  if ("roof_insulation_present" %in% names(v)) set1("roof_insulation_present", as.numeric(inputs$has_roof))
  if ("upgraded_windows"       %in% names(v)) set1("upgraded_windows",       as.numeric(inputs$has_windows))
  if ("efficient_heating"      %in% names(v)) set1("efficient_heating",      as.numeric(inputs$has_heating))
  
  extra <- intersect(names(v), names(seed_row))
  for (nm in extra) if (is.numeric(seed_row[[nm]])) set1(nm, seed_row[[nm]])
  
  nz <- which(v != 0)
  X  <- Matrix::sparseMatrix(i = if (length(nz)) rep(1L, length(nz)) else integer(0),
                             j = nz,
                             x = if (length(nz)) v[nz] else numeric(0),
                             dims = c(1L, length(v)))
  colnames(X) <- names(v)
  X
}

pick_seed_row <- function(baseline, inputs){
  dwell_col  <- if ("DwellingTypeDescr" %in% names(baseline)) "DwellingTypeDescr" else "dwelling_type"
  fuel_col   <- if ("MainSpaceHeatingFuel" %in% names(baseline)) "MainSpaceHeatingFuel" else "main_fuel"
  county_col <- if ("CountyName" %in% names(baseline)) "CountyName" else NULL
  
  dt <- data.table::as.data.table(baseline)
  if (!is.null(dwell_col) && dwell_col %in% names(dt)) dt <- dt[get(dwell_col) == inputs$dwelling_type]
  if (!is.null(fuel_col)  &&  fuel_col %in% names(dt))  dt <- dt[get(fuel_col)  == inputs$main_fuel]
  
  if (!is.null(county_col) && county_col %in% names(dt)) {
    sub <- dt[get(county_col) == inputs$county]
    if (nrow(sub) > 0) dt <- sub
  }
  area_cands <- c("FloorArea","TotalFloorArea","GroundFloorArea.sq.m.","FloorArea_m2")
  area_col <- area_cands[area_cands %in% names(dt)][1]
  if (!is.null(area_col)) {
    dt[, .dist := abs(as.numeric(get(area_col)) - as.numeric(inputs$floor_area_m2))]
    data.table::setorder(dt, .dist)
  }
  year_cands <- c("Year_of_Construction","year_built")
  year_col <- year_cands[year_cands %in% names(dt)][1]
  if (!is.null(year_col)) {
    dt[, .ydist := abs(as.numeric(get(year_col)) - as.numeric(inputs$year_built))]
    data.table::setorder(dt, .dist, .ydist)
  }
  if (nrow(dt) == 0) dt <- data.table::as.data.table(baseline)
  dt[1]
}

decode_baseline <- function(p, meta = NULL){
  tr <- tryCatch(
    tolower(as.character(meta$target_transform %||% meta$transform %||%
                           meta$response %||% meta$target %||% "")),
    error = function(e) ""
  )
  if (nzchar(tr)) {
    if (grepl("log1p|log\\(1\\+x\\)|log\\+1", tr)) return(pmax(expm1(p), 0))
    if (grepl("log", tr))                           return(pmax(expm1(p), 0))
  }
  v <- suppressWarnings(as.numeric(p))
  if (length(v) && all(is.finite(v)) && stats::median(v, na.rm = TRUE) < 50) {
    return(pmax(expm1(v), 0))
  }
  as.numeric(p)
}

predict_baseline <- function(booster3, X, meta=NULL){
  p <- lgb_predict(booster3, X)
  decode_baseline(p, meta)
}

predict_deltas <- function(boosters4, seed_row, inputs, names_list){
  sapply(names(boosters4), function(u){
    Xu <- encode_row_to_p3(seed_row, inputs, names_list[[u]])
    lgb_predict(boosters4[[u]], Xu)
  })
}

to_tariff_key <- function(fuel){
  f <- clean(fuel)
  if (f %in% c("electricity")) "electricity"
  else if (f %in% c("mains_gas","natural_gas","gas")) "gas"
  else if (f %in% c("heating_oil","oil","kerosene")) "heating_oil"
  else NA_character_
}
to_emf_key <- function(fuel){
  f <- clean(fuel)
  if (f %in% c("mains_gas","natural_gas","gas")) "natural_gas_ncv_"
  else if (f %in% c("heating_oil","oil","kerosene")) "heating_oil"
  else if (f %in% c("electricity")) "electricity"
  else if (f %in% c("lpg")) "lpg"
  else if (f %in% c("wood","biomass","wood_logs","wood_pellets")) "wood_pellets"
  else NA_character_
}
econ_layer <- function(delta_co2, fuel, emfs, tariffs){
  k_emf <- to_emf_key(fuel); k_trf <- to_tariff_key(fuel)
  kgco2_per_kwh <- emfs$kgco2_per_kwh[match(k_emf, clean(emfs$fuel))]
  euro_per_kwh  <- tariffs$euro_per_kwh[match(k_trf, clean(tariffs$fuel))]
  delta_kwh  <- if (!is.na(kgco2_per_kwh) && kgco2_per_kwh > 0) delta_co2 / kgco2_per_kwh else NA_real_
  euro_saved <- if (!is.na(delta_kwh) && !is.na(euro_per_kwh)) delta_kwh * euro_per_kwh else NA_real_
  list(delta_kwh = delta_kwh, euro_saved = euro_saved,
       kgco2_per_kwh = kgco2_per_kwh, euro_per_kwh = euro_per_kwh)
}

load_live_assets <- function(){

  p3_names <- readRDS("models/p3_onehot_feature_names.rds")
  b_meta   <- if (file.exists("models/lightgbm_upgrade_friendly_meta.rds")) readRDS("models/lightgbm_upgrade_friendly_meta.rds") else NULL
  booster3 <- load_lgb_any("models/lightgbm_upgrade_friendly")
  n3 <- tryCatch(lightgbm::lgb.num_feature(booster3), error=function(e) NA_integer_)
  if (!is.na(n3) && length(p3_names)!=n3) {
    cand <- if (!is.null(b_meta)) extract_feature_names(b_meta, booster3) else NULL
    if (is.null(cand) || length(cand)!=n3)
      stop(sprintf("Baseline model expects %d features; p3_onehot_feature_names.rds has %d.", n3, length(p3_names)))
    message("utils: using baseline names from meta"); p3_names <- cand
  }
  
  delta_paths <- list(
    heating_efficiency = "models/delta_models/lgb_delta_heating_efficiency",
    insulation_roof    = "models/delta_models/lgb_delta_insulation_roof",
    insulation_wall    = "models/delta_models/lgb_delta_insulation_wall",
    windows            = "models/delta_models/lgb_delta_windows"
  )
  boosters4 <- list(); p4_names <- list()
  for (u in names(delta_paths)) {
    base <- delta_paths[[u]]
    b <- load_lgb_any(base)
    need <- tryCatch(lightgbm::lgb.num_feature(b), error=function(e) NA_integer_)
    meta_path <- paste0(base, "_meta.rds")
    nm <- if (file.exists(meta_path)) extract_feature_names(readRDS(meta_path), b) else NULL
    if (is.null(nm)) stop(sprintf("Delta '%s': missing/invalid feature-name vector in %s", u, basename(meta_path)))
    if (!is.na(need) && length(nm)!=need)
      stop(sprintf("Delta '%s': model expects %d features but meta has %d.", u, need, length(nm)))
    boosters4[[u]] <- b
    p4_names[[u]]  <- nm
    message(sprintf("utils: '%s' names loaded (%d)", u, length(nm)))
  }
  
  baseline <- read_any("outputs/mini_universe_baseline.csv", "outputs/mini_universe_baseline.xlsx", "mini_universe_baseline")
  if (!"row_id" %in% names(baseline)) baseline$row_id <- seq_len(nrow(baseline))
  
  tariffs <- read_any("data/Tariffs.csv","data/Tariffs.xlsx","Tariffs"); names(tariffs) <- clean(names(tariffs))
  if (!"euro_per_kwh" %in% names(tariffs)) {
    cand <- grep("euro.*kwh|eur.*kwh|price.*kwh", names(tariffs), value=TRUE)
    if (length(cand)) names(tariffs)[match(cand[1], names(tariffs))] <- "euro_per_kwh"
  }
  if (!"fuel" %in% names(tariffs)) {
    cand <- grep("fuel", names(tariffs), value=TRUE)
    if (length(cand)) names(tariffs)[match(cand[1], names(tariffs))] <- "fuel"
  }
  emfs <- read_any("data/Emission_factors.csv","data/Emission_factors.xlsx","Emission factors"); names(emfs) <- clean(names(emfs))
  if (!"kgco2_per_kwh" %in% names(emfs)) {
    cand <- grep("kg.?co2.*kwh", names(emfs), value=TRUE)
    if (length(cand)) names(emfs)[match(cand[1], names(emfs))] <- "kgco2_per_kwh"
  }
  if (!"fuel" %in% names(emfs)) {
    cand <- grep("fuel", names(emfs), value=TRUE)
    if (length(cand)) names(emfs)[match(cand[1], names(emfs))] <- "fuel"
  }
  
  list(
    booster3  = booster3,
    b_meta    = b_meta,
    p3_names  = p3_names,
    boosters4 = boosters4,
    p4_names  = p4_names,
    baseline  = baseline,
    tariffs   = tariffs,
    emfs      = emfs
  )
}

probe_booster <- function(booster, feats){
  X <- matrix(0, nrow = 1, ncol = length(feats))
  colnames(X) <- feats
  err <- tryCatch({ lgb_predict(booster, X); NULL }, error = function(e) e$message)
  if (is.null(err)) TRUE else err
}
probe_diag <- function(A){
  cat("== DIAG ==\n")
  cat("Baseline names:", length(A$p3_names), "\n")
  r <- probe_booster(A$booster3, A$p3_names)
  cat("Baseline probe:", if (isTRUE(r)) "OK" else paste("ERR:", r), "\n")
  for (u in names(A$boosters4)){
    nm <- A$p4_names[[u]]
    rr <- probe_booster(A$boosters4[[u]], nm)
    cat(sprintf("Delta %-18s names=%-4d probe=%s\n",
                u, length(nm), if (isTRUE(rr)) "OK" else paste("ERR:", rr)))
  }
}

.pretty_upgrade <- function(x){
  nm <- c(
    insulation_wall = "Wall insulation",
    insulation_roof = "Roof insulation",
    windows         = "Window upgrades",
    heating_efficiency = "Heating efficiency"
  )
  unname(ifelse(x %in% names(nm), nm[x], x))
}

waterfall_co2 <- function(baseline, df, selected) {
  library(plotly)
  sel   <- intersect(selected %||% character(0), df$upgrade)
  labs  <- .pretty_upgrade(sel)
  saves <- pmax(0, df$delta_co2[match(sel, df$upgrade)])
  total_save <- sum(saves, na.rm = TRUE)
  after <- max(baseline - total_save, 0)
  
  x       <- c("Baseline", labs, "After")
  y       <- c(baseline, -saves, after)
  measure <- c("absolute", rep("relative", length(labs)), "total")
  txt     <- c(
    paste0(round(baseline), " kg"),
    paste0(round(saves),    " kg"),
    paste0(round(after),    " kg")
  )
  
  plot_ly(type = "waterfall", x = x, y = y, measure = measure,
          text = txt, textposition = "outside", showlegend = FALSE) %>%
    layout(
      title = list(text = "<b>CO\u2082: Baseline → Reductions → After</b>", x = 0, xanchor = "left"),
      yaxis = list(title = "CO\u2082 (kg/year)"),
      xaxis = list(type = "category", categoryorder = "array", categoryarray = x),
      margin = list(t = 50, b = 20),
      annotations = list(
        list(x = "Baseline", y = baseline, xref = "x", yref = "y",
             text = "Start", showarrow = TRUE, ax = 40, ay = -30),
        list(x = "After", y = after, xref = "x", yref = "y",
             text = "After scenario", showarrow = TRUE, ax = 40, ay = -30)
      )
    )
}

bubble_impact <- function(df) {
  library(plotly)
  df2 <- subset(df, is.finite(delta_co2) & is.finite(euro_saved) & euro_saved > 0)
  if (!nrow(df2)) return(NULL)
  df2$label <- .pretty_upgrade(df2$upgrade)
  xr <- range(df2$delta_co2, na.rm = TRUE)
  yr <- range(df2$euro_saved, na.rm = TRUE)
  xpad <- if (diff(xr) > 0) 0.05 * diff(xr) else 1
  ypad <- if (diff(yr) > 0) 0.08 * diff(yr) else 20
  
  plot_ly(
    df2,
    x = ~delta_co2, y = ~euro_saved,
    type = "scatter", mode = "markers+text",
    text = ~label, textposition = "top center",
    textfont = list(size = 12),
    cliponaxis = FALSE,
    marker = list(
      sizemode = "area",
      sizeref  = 2.0 * max(abs(df2$delta_kwh), na.rm=TRUE) / 4000,
      size     = ~abs(delta_kwh),
      line = list(width = 0.5, color = "rgba(0,0,0,0.2)")
    ),
    showlegend = FALSE
  ) %>%
    layout(
      title = list(text = "<b>Upgrade impact: CO\u2082 saved vs € saved (size = kWh)</b>", x = 0, xanchor = "left"),
      xaxis = list(
        title = "CO\u2082 saved (kg/yr)",
        range = c(xr[1] - xpad, xr[2] + xpad),
        zeroline = FALSE
      ),
      yaxis = list(
        title = "\u20ac saved / year",
        range = c(max(0, yr[1] - ypad), yr[2] + ypad),
        zeroline = FALSE
      ),
      margin = list(t = 50, b = 40, l = 60, r = 20),
      annotations = list(
        list(x = xr[2] + xpad*0.9, y = yr[1], text = "More CO\u2082 saved \u2192",
             showarrow = FALSE, xref = "x", yref = "y", xanchor = "right", yanchor = "bottom"),
        list(x = xr[1], y = yr[2] + ypad*0.8, text = "\u2191 More € / year",
             showarrow = FALSE, xref = "x", yref = "y", xanchor = "left", yanchor = "top")
      )
    )
}

gauge_pct_saved <- function(baseline, df, selected) {
  library(plotly)
  if (!is.finite(baseline) || baseline <= 0) return(NULL)
  sel <- intersect(selected %||% character(0), df$upgrade)
  pct <- 100 * sum(df$delta_co2[df$upgrade %in% sel], na.rm = TRUE) / baseline
  pct <- max(min(pct, 100), 0)
  plot_ly(type = "indicator", mode = "gauge+number",
          value = round(pct, 1),
          number = list(suffix = "%"),
          gauge = list(axis = list(range = list(NULL, 100)))) %>%
    layout(title = list(text = "<b>% CO\u2082 reduction from selected scenario</b>", x = 0, xanchor = "left"),
           margin = list(t = 50, b = 10))
}
