# =========================================
# GreenHomeAI — Delta-CO₂ Prediction Script
#
# This script loads a trained per-upgrade LightGBM ΔCO₂
# model (e.g., for windows) and generates predictions for
# selected rows in the mini-universe baseline dataset.
#
# Workflow:
#  - Load baseline data and the chosen upgrade model/meta
#  - Rebuild factor level maps for consistent encoding
#  - One-hot encode rows using saved training feature set
#  - Run LightGBM model to predict ΔCO₂ values
#  - Print predictions for inspection
# =========================================




library(data.table); library(Matrix); library(lightgbm)

mini <- fread("outputs/mini_universe_baseline.csv")
m_txt  <- "models/delta_models/lgb_delta_windows.txt"              
m_meta <- readRDS("models/delta_models/lgb_delta_windows_meta.rds")

feat_cols   <- m_meta$features_used
onehot_cols <- m_meta$onehot_names

build_level_map <- function(D, cols) {
  levs <- list()
  for (cc in cols) {
    if (cc %in% names(D)) {
      v <- D[[cc]]
      if (is.character(v) || is.factor(v)) {
        levs[[cc]] <- levels(factor(v))    
      }
    }
  }
  levs
}
level_map <- build_level_map(mini, feat_cols)

encode_rows <- function(Dsub, feat_cols, level_map, onehot_cols) {
  X <- copy(Dsub[, ..feat_cols])
  
  for (cc in names(level_map)) {
    if (cc %in% names(X)) {
      X[[cc]] <- factor(as.character(X[[cc]]), levels = level_map[[cc]])
      # still NA? force to first level
      if (any(is.na(X[[cc]]))) {
        X[[cc]][is.na(X[[cc]])] <- level_map[[cc]][1]
      }
    }
  }
  
  for (cc in names(X)) {
    if (is.character(X[[cc]])) X[[cc]] <- factor(X[[cc]])
  }
  
  Xm <- Matrix::sparse.model.matrix(~ . - 1, data = as.data.frame(X))
  cn <- colnames(Xm)
  cn <- gsub("[^A-Za-z0-9_]", "_", cn)
  cn <- make.unique(cn, sep = "_")
  colnames(Xm) <- cn
  
  have <- intersect(onehot_cols, colnames(Xm))
  missing <- setdiff(onehot_cols, have)
  Xm <- Xm[, onehot_cols[onehot_cols %in% have], drop = FALSE]
  if (length(missing)) {
    add <- Matrix(0, nrow = nrow(Xm), ncol = length(missing), sparse = TRUE)
    colnames(add) <- missing
    Xm <- Matrix::cbind2(Xm, add)
  }
  Xm <- Xm[, onehot_cols, drop = FALSE]
  if (!inherits(Xm, "dgCMatrix")) Xm <- as(Xm, "dgCMatrix")
  Xm
}

rows_to_test <- 1:5
Xm <- encode_rows(mini[rows_to_test], feat_cols, level_map, onehot_cols)

mdl <- lgb.load(m_txt)
pred_delta <- pmax(0, as.numeric(predict(mdl, Xm)))
print(pred_delta)






















