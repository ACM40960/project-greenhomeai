# =========================================
# GreenHomeAI — Delta-CO₂ Models (per upgrade)
#
# This script trains a separate LightGBM model for each
# retrofit upgrade to predict the expected CO₂ reduction (ΔCO₂).
#
# Workflow:
#  - Load baseline and scenario delta-CO₂ datasets
#  - Align with features used in the main upgrade-friendly model
#  - Train per-upgrade LightGBM regression models (log-target not needed)
#  - Evaluate with RMSE, MAE, R², MAPE + diagnostic plots
#  - Save model files, metadata, and feature importance
#  - Export summary of all upgrade models
#
# Outputs:
#   - models/delta_models/lgb_delta_<upgrade>.txt
#   - models/delta_models/lgb_delta_<upgrade>_meta.rds
#   - outputs/delta_importance_<upgrade>.csv
#   - outputs/delta_models_summary.csv
# =========================================


suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(lightgbm)
})

set.seed(42)

mini_path   <- "outputs/mini_universe_baseline.csv"
scen_long   <- "outputs/part4_scenarios_deltaCO2_long.csv"
meta_rds    <- "models/lightgbm_upgrade_friendly_meta.rds"   
onehot_rds  <- "models/p3_onehot_feature_names.rds"          
out_dir     <- "outputs"
model_dir   <- "models/delta_models"

if (!dir.exists(out_dir))  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

dt_base <- fread(mini_path)
dt_scen <- fread(scen_long)

setnames(dt_base, old = names(dt_base), new = make.names(names(dt_base), unique = TRUE))
setnames(dt_scen, old = names(dt_scen), new = make.names(names(dt_scen), unique = TRUE))

stopifnot(file.exists(meta_rds))
meta <- readRDS(meta_rds)
stopifnot(!is.null(meta$features_used))
feature_cols <- intersect(meta$features_used, names(dt_base))

forced_cols <- intersect(
  c("WallHeatLossProxy","RoofHeatLossProxy","FloorHeatLossProxy","WindowHeatLossProxy",
    "HSMainSystemEfficiency","TempAdjustment"),
  names(dt_base)
)
feature_cols <- unique(c(feature_cols, forced_cols))
stopifnot(length(feature_cols) > 0)

dt_scen <- merge(
  dt_scen,
  dt_base[, c("CO2_baseline", feature_cols), with = FALSE][, row_id := seq_len(.N)],
  by = "row_id",
  all.x = TRUE
)

dt_scen[is.na(delta_CO2), delta_CO2 := 0]
dt_scen[delta_CO2 < 0,    delta_CO2 := 0]

lgb_params <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.05,
  num_leaves = 48,
  feature_fraction = 0.85,
  bagging_fraction = 0.8,
  bagging_freq = 1,
  min_data_in_leaf = 100,
  lambda_l2 = 1.0,
  max_depth = -1,
  verbosity = -1
)

to_sparse <- function(df) {
  mm <- model.matrix(~ . - 1, data = df)         
  sp <- Matrix(mm, sparse = TRUE)                 
  cn <- colnames(sp)
  cn <- gsub("[^A-Za-z0-9_]", "_", cn)
  cn <- make.unique(cn, sep = "_")
  colnames(sp) <- cn
  sp
}

upgrades <- sort(unique(dt_scen$upgrade))
summary_rows <- list()

for (u in upgrades) {
  cat("\n[ΔCO2] Training for upgrade:", u, "\n")
  

  D <- dt_scen[upgrade == u]
  

  Xdf <- copy(D[, ..feature_cols])
  char_cols <- names(Xdf)[vapply(Xdf, is.character, logical(1))]
  for (cc in char_cols) Xdf[[cc]] <- factor(Xdf[[cc]])
  Xm <- to_sparse(Xdf)
  
  y <- D$delta_CO2
  
  n <- nrow(Xm)
  idx_tr <- sample.int(n, floor(0.8 * n))
  idx_va <- setdiff(seq_len(n), idx_tr)
  
  dtrain <- lgb.Dataset(data = Xm[idx_tr, ], label = y[idx_tr])
  dvalid <- lgb.Dataset(data = Xm[idx_va, ], label = y[idx_va])
  
  model <- lgb.train(
    params = lgb_params,
    data = dtrain,
    nrounds = 4000,
    valids = list(valid = dvalid),
    early_stopping_rounds = 100,
    verbose = 1
  )
  
 
  pred_val <- predict(model, Xm[idx_va, ])
  rmse <- sqrt(mean((pred_val - y[idx_va])^2))
  mae  <- mean(abs(pred_val - y[idx_va]))
  
  
  
  
  cat(sprintf("[ΔCO2 %s] RMSE: %.3f  |  MAE: %.3f  |  Best iter: %d\n", u, rmse, mae, model$best_iter))
  
  
  suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(scales) })
  
  theme_poster <- theme_minimal(base_size = 18) +
    theme(
      plot.title = element_text(face="bold", size=22, hjust=0.5),
      axis.title = element_text(face="bold", size=18),
      axis.text  = element_text(size=16),
      panel.grid.minor = element_blank()
    )
  
  save_plot <- function(p, file, w=10, h=7, dpi=320) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    ggsave(filename = file, plot = p, width = w, height = h, dpi = dpi, bg = "white")
  }
  
  eval_df <- data.table(
    actual = y[idx_va],
    pred   = as.numeric(pred_val)
  )[is.finite(actual) & is.finite(pred)]
  
  rmse_v <- sqrt(mean((eval_df$pred - eval_df$actual)^2))
  mae_v  <- mean(abs(eval_df$pred - eval_df$actual))
  denom  <- sum((eval_df$actual - mean(eval_df$actual))^2)
  r2_v   <- if (denom > 0) 1 - sum((eval_df$actual - eval_df$pred)^2) / denom else NA_real_
  mape_v <- mean(abs((eval_df$pred - eval_df$actual) / pmax(1e-9, abs(eval_df$actual)))) * 100
  
  annot <- paste0("RMSE=", comma(rmse_v, accuracy=1),
                  " | MAE=", comma(mae_v, accuracy=1),
                  " | R²=", ifelse(is.na(r2_v),"NA", sprintf("%.3f", r2_v)),
                  " | MAPE=", sprintf('%.2f%%', mape_v))
  
  qx <- quantile(eval_df$pred,   c(.005, .995), na.rm = TRUE)
  qy <- quantile(eval_df$actual, c(.005, .995), na.rm = TRUE)
  
  p_parity <- ggplot(eval_df, aes(x = pred, y = actual)) +
    geom_abline(slope=1, intercept=0, linetype="dashed", color="red", linewidth=1) +
    geom_point(alpha=0.35, size=1.2) +
    geom_smooth(method="loess", se=FALSE, linewidth=1) +
    annotate("text", x=qx[1], y=qy[2], hjust=0, vjust=1, label=annot, size=5) +
    scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
    coord_cartesian(xlim=qx, ylim=qy) +
    labs(title = paste0(u, ": Actual vs Predicted ΔCO₂"),
         x="Predicted ΔCO₂ (kg/yr)", y="Actual ΔCO₂ (kg/yr)") +
    theme_poster
  
  calib <- eval_df %>%
    mutate(bin = ntile(pred, 10)) %>%
    group_by(bin) %>%
    summarise(pred_mean = mean(pred), actual_mean = mean(actual), .groups = "drop")
  
  p_calib <- ggplot(calib, aes(x = pred_mean, y = actual_mean)) +
    geom_abline(slope=1, intercept=0, linetype="dashed", color="red", linewidth=1) +
    geom_point(size=3) + geom_line(linewidth=1) +
    scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
    coord_equal() +
    labs(title = paste0(u, ": Calibration Curve"),
         x="Mean predicted ΔCO₂ (kg/yr)", y="Mean actual ΔCO₂ (kg/yr)") +
    theme_poster
  
  plot_segment_mae <- function(df_full, idx_va, seg) {
    if (!seg %in% names(df_full)) return(NULL)
    tmp <- data.table(
      seg = df_full[[seg]][idx_va],
      abs_err = abs(y[idx_va] - pred_val)
    )
    tmp <- tmp[is.finite(abs_err) & !is.na(seg),
               .(n=.N, MAE = mean(abs_err)), by = seg][order(-MAE)]
    ggplot(tmp, aes(x = reorder(seg, MAE), y = MAE)) +
      geom_col() + coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(title = paste0(u, ": Error by ", seg),
           x = seg, y = "Mean Absolute Error (kg/yr)") +
      theme_poster
  }
  
  segments_try <- c("MainSpaceHeatingFuel","DwellingTypeDescr","CountyName","YearBuilt")
  seg_plots <- lapply(segments_try, function(s) plot_segment_mae(D, idx_va, s))
  seg_plots <- Filter(Negate(is.null), seg_plots)
  
  od <- file.path("poster_plots", paste0("delta_", u))
  save_plot(p_parity, file.path(od, "01_parity.png"))
  save_plot(p_calib, file.path(od, "02_calibration.png"))
  if (length(seg_plots)) {
    for (i in seq_along(seg_plots)) save_plot(seg_plots[[i]], file.path(od, sprintf("03_error_by_%02d.png", i)))
  }
  

  
  
  
  
  mpath <- file.path(model_dir, sprintf("lgb_delta_%s.txt", u))
  lgb.save(model, mpath)
  
  meta_u <- list(
    date_trained = Sys.time(),
    upgrade = u,
    rmse_valid = rmse,
    mae_valid = mae,
    best_iter = model$best_iter,
    features_used = feature_cols,
    onehot_names = colnames(Xm)     
  )
  saveRDS(meta_u, file.path(model_dir, sprintf("lgb_delta_%s_meta.rds", u)))
  
  imp <- lgb.importance(model, percentage = TRUE)
  fwrite(imp, file.path(out_dir, sprintf("delta_importance_%s.csv", u)))
  
  summary_rows[[u]] <- data.table(
    upgrade = u, n_train = length(idx_tr), n_valid = length(idx_va),
    rmse_valid = rmse, mae_valid = mae, best_iter = model$best_iter
  )
  
  
  
}

delta_summary <- rbindlist(summary_rows, use.names = TRUE)
fwrite(delta_summary, file.path(out_dir, "delta_models_summary.csv"))
print(delta_summary)

cat("\nSaved per-upgrade models to:", normalizePath(model_dir), "\n")
cat("Saved importances to outputs/delta_importance_<upgrade>.csv and summary to outputs/delta_models_summary.csv\n")





