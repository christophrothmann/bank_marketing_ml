library(here)
library(xgboost)
library(caret)

# Load the XGBoost models
model_unweighted <- readRDS(here::here("source", "models_output", "xgboost_model_unweighted.rds"))
model_weighted <- readRDS(here::here("source", "models_output", "xgboost_model_weighted.rds"))

# Load the data
source(here::here("source", "models", "ipw_data.R"))

# Prepare matrices (as in model_2_XGBoost.R)
train_labels <- ifelse(train_data$y == "yes", 1, 0)
test_labels <- ifelse(test_data$y == "yes", 1, 0)

train_features <- train_data[, !names(train_data) %in% c("y", "ipw_weight")]
test_features <- test_data[, names(test_data) != "y"]

dummy_model <- dummyVars(~., data = train_features)
test_matrix <- predict(dummy_model, newdata = test_features)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# Predictions (probabilities)
preds_unweighted <- predict(model_unweighted, dtest)
preds_weighted <- predict(model_weighted, dtest)

# Function to calculate PR-AUC (Average Precision)
calc_pr_auc <- function(probs, labels_factor, positive_class = "yes") {
  labels <- ifelse(labels_factor == positive_class, 1, 0)
  ord <- order(probs, decreasing = TRUE)
  probs <- probs[ord]
  labels <- labels[ord]
  
  tp <- cumsum(labels)
  fp <- cumsum(1 - labels)
  
  precision <- tp / (tp + fp)
  recall <- tp / sum(labels)
  
  precision <- c(1, precision)
  recall <- c(0, recall)
  
  ap <- 0
  for (i in 2:length(recall)) {
    dr <- recall[i] - recall[i - 1]
    if (dr > 0) {
      ap <- ap + dr * precision[i]
    }
  }
  return(ap)
}

# Calculate PR-AUC
pr_auc_unweighted <- calc_pr_auc(preds_unweighted, test_data$y)
pr_auc_weighted <- calc_pr_auc(preds_weighted, test_data$y)

# Let's print out the results
cat("\n==================================================\n")
cat("XGBOOST: METRIKEN & KONFUSIONSNATRIX\n")
cat("==================================================\n")

# A. UNGEWICHTET (Threshold = 0.05)
cat("\n--- UNGEWICHTETES XGBOOST (Threshold = 0.05) ---\n")
preds_class_unweighted <- factor(ifelse(preds_unweighted > 0.05, "yes", "no"), levels = c("no", "yes"))
cm_unweighted <- confusionMatrix(preds_class_unweighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_unweighted$table)
cat(sprintf("Precision: %.2f%%\n", cm_unweighted$byClass["Precision"] * 100))
cat(sprintf("Recall (Sensitivity): %.2f%%\n", cm_unweighted$byClass["Recall"] * 100))
cat(sprintf("F1-Score: %.2f%%\n", cm_unweighted$byClass["F1"] * 100))
cat(sprintf("PR-AUC (Precision-Recall AUC): %.4f\n", pr_auc_unweighted))

# B. GEWICHTET (Threshold = 0.05)
cat("\n--- GEWICHTETES XGBOOST (IPW, Threshold = 0.05) ---\n")
preds_class_weighted <- factor(ifelse(preds_weighted > 0.05, "yes", "no"), levels = c("no", "yes"))
cm_weighted <- confusionMatrix(preds_class_weighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_weighted$table)
cat(sprintf("Precision: %.2f%%\n", cm_weighted$byClass["Precision"] * 100))
cat(sprintf("Recall (Sensitivity): %.2f%%\n", cm_weighted$byClass["Recall"] * 100))
cat(sprintf("F1-Score: %.2f%%\n", cm_weighted$byClass["F1"] * 100))
cat(sprintf("PR-AUC (Precision-Recall AUC): %.4f\n", pr_auc_weighted))
cat("==================================================\n")
