# =========================================
# GreenHomeAI — Upgrade-Friendly CO₂ Model
#
# This script builds a LightGBM regression model
# to predict household CO₂ emissions while ensuring
# features are upgrade-friendly (no leakage).
#
# Main workflow:
#  - Load and clean BER dataset
#  - Drop outcome-like / leakage-prone features
#  - Engineer physics-based proxy features
#  - Train/test split and one-hot encoding
#  - Train LightGBM with early stopping (log-target)
#  - Evaluate model with RMSE, MAE, R², MAPE
#  - Run "what-if" scenarios for retrofits (walls, windows)
#  - Save model, metadata, and evaluation plots
# =========================================





suppressPackageStartupMessages({
  library(data.table)
  library(Matrix)
  library(lightgbm)
})

set.seed(42)

path <- "data/BERmaster2x.csv"
DT <- fread(path)
stopifnot("target_CO2_kg" %in% names(DT))
cat("Loaded:", nrow(DT), "rows,", ncol(DT), "cols\n")

setnames(DT, old = names(DT), new = make.names(names(DT), unique = TRUE))

drop_cols <- c(
  "Total_CO2_kg","Total_CO2_kg_rebuilt","Total_CO2_kg_clean","target_CO2_kg","co2_total_replaced_flag",
  grep("^CO2", names(DT), value = TRUE),
  
  grep("^Delivered", names(DT), value = TRUE),
  grep("^Primary",   names(DT), value = TRUE),
  "Total_Delivered_kWh","Total_Primary_kWh",
  
  "BerRating","EnergyRating","EnergyRatingG","TypeofRating","MPCDERValue"
)
drop_cols <- unique(intersect(drop_cols, names(DT)))
keep_cols <- setdiff(names(DT), drop_cols)

id_like <- intersect(c("BERNumber","MPRN","Eircode","Address","Postcode","UPRN","ID"), keep_cols)
keep_cols <- setdiff(keep_cols, id_like)

cat("\nDropping (upgrade-unfriendly):\n"); print(sort(drop_cols))

has <- function(cols) all(cols %in% names(DT))

if (has(c("WallArea","UValueWall")))                 DT[, WallHeatLossProxy   := WallArea              * UValueWall]
if (has(c("RoofArea","UValueRoof")))                 DT[, RoofHeatLossProxy   := RoofArea              * UValueRoof]
if (has(c("GroundFloorArea.sq.m.","UValueFloor")))   DT[, FloorHeatLossProxy  := GroundFloorArea.sq.m. * UValueFloor]
if (has(c("WindowArea","UValueWindow")))             DT[, WindowHeatLossProxy := WindowArea            * UValueWindow]

engineered <- intersect(c("WallHeatLossProxy","RoofHeatLossProxy","FloorHeatLossProxy","WindowHeatLossProxy"),
                        names(DT))
keep_cols <- unique(c(keep_cols, engineered))

cat("\nFinal kept feature columns (first 40):\n"); print(head(keep_cols, 40))
cat(sprintf("Total kept features: %d\n", length(keep_cols)))

X <- DT[, ..keep_cols]
y <- DT[["target_CO2_kg"]]

n <- nrow(X)
idx_train <- sample.int(n, size = floor(0.8 * n))
idx_test  <- setdiff(seq_len(n), idx_train)

X_train <- X[idx_train]
X_test  <- X[idx_test]
y_train <- y[idx_train]
y_test  <- y[idx_test]

char_cols <- names(X_train)[vapply(X_train, is.character, logical(1))]
for (cc in char_cols) {
  X_train[[cc]] <- factor(X_train[[cc]])
  X_test[[cc]]  <- factor(X_test[[cc]], levels = levels(X_train[[cc]]))
}

to_sparse <- function(df) {
  mm <- model.matrix(~ . - 1, data = df)
  as(mm, "dgCMatrix")
}
Xm_train <- to_sparse(X_train)
Xm_test  <- to_sparse(X_test)

orig_names <- colnames(Xm_train)
safe_names <- gsub("[^A-Za-z0-9_]", "_", orig_names)
safe_names <- make.unique(safe_names, sep = "_")
colnames(Xm_train) <- safe_names
colnames(Xm_test)  <- safe_names

saveRDS(colnames(Xm_train), "models/p3_onehot_feature_names.rds")
cat("Saved one-hot feature names to models/p3_onehot_feature_names.rds\n")



y_train_log <- log1p(y_train)
y_test_log  <- log1p(y_test)

lgb_train <- lgb.Dataset(data = Xm_train, label = y_train_log)
lgb_valid <- lgb.Dataset(data = Xm_test,  label = y_test_log)

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

cat("\n[LightGBM] Training (upgrade-friendly, log-target, early stopping)...\n")
lgb_model <- lgb.train(
  params = lgb_params,
  data = lgb_train,
  nrounds = 4000,                 # more room — will stop early
  valids = list(valid = lgb_valid),
  early_stopping_rounds = 100,
  verbose = 1
)

pred_log <- predict(lgb_model, Xm_test)
pred <- pmax(0, expm1(pred_log))  # guard tiny negatives
rmse <- sqrt(mean((pred - y_test)^2))
mae  <- mean(abs(pred - y_test))
cat(sprintf("\n[LightGBM Upgrade-Friendly] RMSE: %.2f | MAE: %.2f | Best iter: %d\n",
            rmse, mae, lgb_model$best_iter))


j <- sample.int(nrow(X_test), 1)

base_row <- copy(X_test[j])   # 1-row data.table
upg_row  <- copy(base_row)

if ("UValueWall" %in% names(upg_row)) {
  upg_row[, UValueWall := pmax(0.15, UValueWall - 0.30)]
}
if (all(c("WallArea","UValueWall") %in% names(upg_row))) {
  upg_row[, WallHeatLossProxy := WallArea * UValueWall]
}

train_levels <- lapply(X_train[, sapply(X_train, is.factor), with = FALSE], levels)

encode_one <- function(df_row, train_levels, safe_names_ref) {
  df_copy <- as.data.frame(df_row)  # ensure data.frame, not vector
  
  for (cc in names(train_levels)) {
    if (cc %in% names(df_copy)) {
      df_copy[[cc]] <- factor(df_copy[[cc]], levels = train_levels[[cc]])
      df_copy[[cc]][is.na(df_copy[[cc]])] <- levels(train_levels[[cc]])[1]
    }
  }
  
  mm <- model.matrix(~ . - 1, data = df_copy)
  sp <- Matrix::Matrix(mm, sparse = TRUE)
  
  cn <- colnames(sp)
  cn <- gsub("[^A-Za-z0-9_]", "_", cn)
  cn <- make.unique(cn, sep = "_")
  colnames(sp) <- cn
  
  missing <- setdiff(safe_names_ref, colnames(sp))
  if (length(missing)) {
    add <- Matrix::Matrix(0, nrow = nrow(sp), ncol = length(missing), sparse = TRUE)
    colnames(add) <- missing
    sp <- cbind(sp, add)
  }
  sp <- sp[, safe_names_ref, drop = FALSE]  
  sp
}

base_sp <- encode_one(base_row, train_levels, safe_names)
upg_sp  <- encode_one(upg_row,  train_levels, safe_names)

pred_base <- pmax(0, expm1(predict(lgb_model, base_sp)))
pred_upg  <- pmax(0, expm1(predict(lgb_model, upg_sp)))

cat("\nWhat‑if sanity check (row ", j, " within test set):\n", sep = "")
cat(sprintf("Baseline CO2: %.1f kg  ->  After better wall insulation: %.1f kg  (Δ=%.1f kg)\n",
            as.numeric(pred_base), as.numeric(pred_upg), as.numeric(pred_upg - pred_base)))

cat(sprintf("Observed CO2 (target) for this row: %.1f kg\n", y_test[j]))








train_levels <- lapply(X_train[, sapply(X_train, is.factor), with = FALSE], levels)

encode_one <- function(df_row, train_levels, safe_names_ref) {
  df_copy <- as.data.frame(df_row)
  for (cc in names(train_levels)) {
    if (cc %in% names(df_copy)) {
      df_copy[[cc]] <- factor(df_copy[[cc]], levels = train_levels[[cc]])
      df_copy[[cc]][is.na(df_copy[[cc]])] <- levels(train_levels[[cc]])[1]
    }
  }
  mm <- model.matrix(~ . - 1, data = df_copy)
  sp <- Matrix::Matrix(mm, sparse = TRUE)
  cn <- colnames(sp); cn <- gsub("[^A-Za-z0-9_]", "_", cn); cn <- make.unique(cn, sep = "_")
  colnames(sp) <- cn
  missing <- setdiff(safe_names_ref, colnames(sp))
  if (length(missing)) {
    add <- Matrix::Matrix(0, nrow = nrow(sp), ncol = length(missing), sparse = TRUE)
    colnames(add) <- missing
    sp <- cbind(sp, add)
  }
  sp <- sp[, safe_names_ref, drop = FALSE]
  sp
}

j <- sample.int(nrow(X_test), 1)

base_row <- copy(X_test[j])
upg_row  <- copy(base_row)

if ("UValueWindow" %in% names(upg_row)) {
  upg_row[, UValueWindow := pmax(0.80, UValueWindow - 0.60)]
}
if (all(c("WindowArea","UValueWindow") %in% names(upg_row))) {
  upg_row[, WindowHeatLossProxy := WindowArea * UValueWindow]
}

base_sp <- encode_one(base_row, train_levels, safe_names)
upg_sp  <- encode_one(upg_row,  train_levels, safe_names)

pred_base <- pmax(0, expm1(predict(lgb_model, base_sp)))
pred_upg  <- pmax(0, expm1(predict(lgb_model, upg_sp)))

cat("\nWhat‑if Scenario 2 (row ", j, "): Better windows\n", sep = "")
cat(sprintf("Baseline CO2: %.1f kg  ->  After window upgrade: %.1f kg  (Δ=%.1f kg)\n",
            as.numeric(pred_base), as.numeric(pred_upg), as.numeric(pred_upg - pred_base)))
cat(sprintf("Observed CO2 (target) for this row: %.1f kg\n", y_test[j]))






model_path <- "models/lightgbm_upgrade_friendly.txt"
lgb.save(lgb_model, model_path)
cat(sprintf("Model saved to: %s\n", model_path))

meta <- list(
  date_trained = Sys.time(),
  rmse = rmse,
  mae = mae,
  best_iter = lgb_model$best_iter,
  features_used = keep_cols
)
meta_path <- "models/lightgbm_upgrade_friendly_meta.rds"
saveRDS(meta, meta_path)
cat(sprintf("Metadata saved to: %s\n", meta_path))


suppressWarnings({
  library(ggplot2)
  library(dplyr)
  library(scales)
})

eval_df <- data.frame(
  actual = y_test,
  pred   = pred
)

maybe_cols <- intersect(
  c("CountyName", "DwellingTypeDescr", "MainSpaceHeatingFuel",
    "YearBuilt", "FloorArea", "FloorArea..m2."),
  names(X_test)
)
if (length(maybe_cols)) {
  eval_df <- cbind(eval_df, as.data.frame(X_test[, ..maybe_cols]))
}

rmse <- sqrt(mean((eval_df$pred - eval_df$actual)^2))
mae  <- mean(abs(eval_df$pred - eval_df$actual))
r2   <- 1 - sum((eval_df$actual - eval_df$pred)^2) /
  sum((eval_df$actual - mean(eval_df$actual))^2)
mape <- mean(abs((eval_df$pred - eval_df$actual) / pmax(1e-9, eval_df$actual))) * 100

cat(sprintf("\nEvaluation Metrics\n------------------\nRMSE : %.2f\nMAE  : %.2f\nR²   : %.3f\nMAPE : %.2f%%\n\n",
            rmse, mae, r2, mape))



















suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(scales)
})

theme_poster <- theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face="bold", size=22, hjust=0.5),
    axis.title = element_text(face="bold", size=18),
    axis.text  = element_text(size=16),
    panel.grid.minor = element_blank()
  )

save_plot <- function(p, file, w=10, h=7, dpi=320) {
  ggsave(filename = file, plot = p, width = w, height = h, dpi = dpi, bg = "white")
  message("Saved: ", normalizePath(file))
}

eval_df <- eval_df %>%
  filter(is.finite(actual), is.finite(pred)) %>%
  mutate(resid = actual - pred)

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))
mae  <- function(y, yhat) mean(abs(y - yhat))
r2   <- function(y, yhat) 1 - sum((y - yhat)^2)/sum((y - mean(y))^2)

m_rmse <- rmse(eval_df$actual, eval_df$pred)
m_mae  <- mae (eval_df$actual, eval_df$pred)
m_r2   <- r2  (eval_df$actual, eval_df$pred)

annot_text <- paste0("RMSE = ", comma(m_rmse, accuracy = 1),
                     "   |   MAE = ", comma(m_mae, accuracy = 1),
                     "   |   R² = ", round(m_r2, 3))

qx <- quantile(eval_df$pred,   probs = c(.005, .995))
qy <- quantile(eval_df$actual, probs = c(.005, .995))

p1_post <- ggplot(eval_df, aes(x = pred, y = actual)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  annotate("text", x = qx[1], y = qy[2], hjust = 0, vjust = 1,
           label = annot_text, size = 5) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(xlim = qx, ylim = qy) +
  labs(title = "Actual vs Predicted CO₂",
       x = "Predicted CO₂ (kg/yr)",
       y = "Actual CO₂ (kg/yr)") +
  theme_poster

calib_df <- eval_df %>%
  mutate(bin = ntile(pred, 10)) %>%
  group_by(bin) %>%
  summarise(pred_mean = mean(pred),
            actual_mean = mean(actual),
            .groups = "drop")

p4_post <- ggplot(calib_df, aes(x = pred_mean, y = actual_mean)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  coord_equal() +
  labs(title = "Calibration Curve",
       x = "Mean predicted CO₂ (kg/yr)",
       y = "Mean actual CO₂ (kg/yr)") +
  theme_poster

plot_segment_mae_poster <- function(df, segment_col, title_prefix = "Model Error by ") {
  stopifnot(segment_col %in% names(df))
  tmp <- df %>%
    mutate(abs_err = abs(actual - pred)) %>%
    group_by(.data[[segment_col]]) %>%
    summarise(n = dplyr::n(),
              MAE = mean(abs_err),
              .groups = "drop") %>%
    arrange(desc(MAE))
  
  ggplot(tmp, aes(x = reorder(.data[[segment_col]], MAE), y = MAE)) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(title = paste0(title_prefix, segment_col),
         x = segment_col,
         y = "Mean Absolute Error (kg/yr)") +
    theme_poster
}

p_fuel_post <- NULL
if ("MainSpaceHeatingFuel" %in% names(eval_df)) {
  p_fuel_post <- plot_segment_mae_poster(eval_df, "MainSpaceHeatingFuel")
}

print(p1_post)
print(p4_post)
if (!is.null(p_fuel_post)) print(p_fuel_post)

dir.create("poster_plots", showWarnings = FALSE)
save_plot(p1_post,      "poster_plots/01_actual_vs_predicted.png")
save_plot(p4_post,      "poster_plots/02_calibration_curve.png")
if (!is.null(p_fuel_post)) save_plot(p_fuel_post, "poster_plots/03_mae_by_fuel.png")
