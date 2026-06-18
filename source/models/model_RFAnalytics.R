library(here)
library(randomForest)
library(caret)

# Load the Random Forest models
model_unweighted <- readRDS(here::here("source", "models_output", "rf_tuned_model_unweighted.rds"))
model_weighted <- readRDS(here::here("source", "models_output", "rf_tuned_model_weighted.rds"))

# Load the data
source(here::here("source", "models", "ipw_data.R"))

# Prepare data (as in model_1_RF_Tuned.R)
train_features_names <- names(train_data)[!names(train_data) %in% c("ipw_weight")]
train_clean <- train_data[, train_features_names]

# Predictions (probabilities)
preds_unweighted <- predict(model_unweighted, newdata = test_data, type = "prob")[, "yes"]
preds_weighted <- predict(model_weighted, newdata = test_data, type = "prob")[, "yes"]

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
cat("RANDOM FOREST: METRIKEN & KONFUSIONSNATRIX\n")
cat("==================================================\n")

# A. UNGEWICHTET (Threshold = 0.60)
cat("\n--- UNGEWICHTETES RANDOM FOREST (Threshold = 0.60) ---\n")
preds_class_unweighted <- factor(ifelse(preds_unweighted > 0.60, "yes", "no"), levels = c("no", "yes"))
cm_unweighted <- confusionMatrix(preds_class_unweighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_unweighted$table)
cat(sprintf("Precision: %.2f%%\n", cm_unweighted$byClass["Precision"] * 100))
cat(sprintf("Recall (Sensitivity): %.2f%%\n", cm_unweighted$byClass["Recall"] * 100))
cat(sprintf("F1-Score: %.2f%%\n", cm_unweighted$byClass["F1"] * 100))
cat(sprintf("PR-AUC (Precision-Recall AUC): %.4f\n", pr_auc_unweighted))

# B. GEWICHTET (Threshold = 0.36)
cat("\n--- GEWICHTETES RANDOM FOREST (IPW, Threshold = 0.36) ---\n")
preds_class_weighted <- factor(ifelse(preds_weighted > 0.36, "yes", "no"), levels = c("no", "yes"))
cm_weighted <- confusionMatrix(preds_class_weighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_weighted$table)
cat(sprintf("Precision: %.2f%%\n", cm_weighted$byClass["Precision"] * 100))
cat(sprintf("Recall (Sensitivity): %.2f%%\n", cm_weighted$byClass["Recall"] * 100))
cat(sprintf("F1-Score: %.2f%%\n", cm_weighted$byClass["F1"] * 100))
cat(sprintf("PR-AUC (Precision-Recall AUC): %.4f\n", pr_auc_weighted))
cat("==================================================\n")
