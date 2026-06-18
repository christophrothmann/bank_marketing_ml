library(here)
library(caret)

# Load the logistic regression models
model_unweighted <- readRDS(here::here("source", "models_output", "logistic_model_unweighted.rds"))
model_weighted <- readRDS(here::here("source", "models_output", "logistic_model_weighted.rds"))

# Load the data
source(here::here("source", "models", "ipw_data.R"))

# Prepare matrices (as in model_3_LogReg.R)
train_features <- train_data[, !names(train_data) %in% c("y", "month", "ipw_weight")]
test_features <- test_data[, !names(test_data) %in% c("y", "month")]

dummy_model <- dummyVars(~., data = train_features)
test_matrix <- as.data.frame(predict(dummy_model, newdata = test_features))

# Predictions
preds_unweighted <- predict(model_unweighted, newdata = test_matrix, type = "response")
preds_weighted <- predict(model_weighted, newdata = test_matrix, type = "response")

# Function to calculate PR-AUC (Average Precision)
# AP is defined as sum_n (Recall_n - Recall_{n-1}) * Precision_n
calc_pr_auc <- function(probs, labels_factor, positive_class = "yes") {
    # Convert labels to 0 and 1
    labels <- ifelse(labels_factor == positive_class, 1, 0)

    # Sort probabilities and corresponding labels in descending order
    ord <- order(probs, decreasing = TRUE)
    probs <- probs[ord]
    labels <- labels[ord]

    # Calculate cumulative true positives and false positives
    tp <- cumsum(labels)
    fp <- cumsum(1 - labels)

    # Calculate precision and recall
    precision <- tp / (tp + fp)
    recall <- tp / sum(labels)

    # Add starting point (precision = 1, recall = 0)
    precision <- c(1, precision)
    recall <- c(0, recall)

    # Calculate area using trapezoidal integration or average precision
    # OLS average precision formula:
    ap <- 0
    for (i in 2:length(recall)) {
        # Increment in recall
        dr <- recall[i] - recall[i - 1]
        if (dr > 0) {
            # Use trapezoid or step height
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
cat("LOGISTISCHE REGRESSION: METRIKEN & KONFUSIONSNATRIX\n")
cat("==================================================\n")

# A. UNGEWICHTET (Threshold = 0.25)
cat("\n--- UNGEWICHTETES MODELL (Threshold = 0.25) ---\n")
preds_class_unweighted <- factor(ifelse(preds_unweighted > 0.25, "yes", "no"), levels = c("no", "yes"))
cm_unweighted <- confusionMatrix(preds_class_unweighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_unweighted$table)
cat(sprintf("Precision: %.2f%%\n", cm_unweighted$byClass["Precision"] * 100))
cat(sprintf("Recall (Sensitivity): %.2f%%\n", cm_unweighted$byClass["Recall"] * 100))
cat(sprintf("F1-Score: %.2f%%\n", cm_unweighted$byClass["F1"] * 100))
cat(sprintf("PR-AUC (Precision-Recall AUC): %.4f\n", pr_auc_unweighted))

# B. GEWICHTET (Threshold = 0.21)
cat("\n--- GEWICHTETES MODELL (IPW, Threshold = 0.21) ---\n")
preds_class_weighted <- factor(ifelse(preds_weighted > 0.21, "yes", "no"), levels = c("no", "yes"))
cm_weighted <- confusionMatrix(preds_class_weighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_weighted$table)
cat(sprintf("Precision: %.2f%%\n", cm_weighted$byClass["Precision"] * 100))
cat(sprintf("Recall (Sensitivity): %.2f%%\n", cm_weighted$byClass["Recall"] * 100))
cat(sprintf("F1-Score: %.2f%%\n", cm_weighted$byClass["F1"] * 100))
cat(sprintf("PR-AUC (Precision-Recall AUC): %.4f\n", pr_auc_weighted))
cat("==================================================\n")
