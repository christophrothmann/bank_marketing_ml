# ==============================================================================
# MODELL 3: LOGISTISCHE REGRESSION (UNGEWICHTET VS. GEWICHTET)
# ==============================================================================

# --- 1. DATEN & PAKETE LADEN ---
library(here)
library(caret)

# IPW-Vorbereitungsskript laden (enthält ModelData.R und ipw_weights)
source(here::here("source", "models", "ipw_data.R"))

cat("Bereite Daten für die Logistische Regression vor...\n")

# --- 2. DUMMY-KODIERUNG & GEWICHTE EXTRAHIEREN ---
# Extrahieren der Gewichte
ipw_weights <- train_data$ipw_weight

# month entfernen (Multikollinearität) und ipw_weight (damit es nicht als Feature dient)
train_features <- train_data[, !names(train_data) %in% c("y", "month", "ipw_weight")]
test_features <- test_data[, !names(test_data) %in% c("y", "month")]

# One-Hot-Encoding
dummy_model <- dummyVars(~., data = train_features)
train_matrix <- as.data.frame(predict(dummy_model, newdata = train_features))
test_matrix <- as.data.frame(predict(dummy_model, newdata = test_features))

# Target-Variable y hinzufügen
train_matrix$y <- train_data$y

# --- Helper Funktion zur Schwellenwert-Optimierung ---
optimize_threshold <- function(preds_prob, actual_labels) {
  thresholds <- seq(0.05, 0.95, by = 0.01)
  results <- data.frame(threshold = thresholds, precision = NA, recall = NA, f1 = NA)
  
  for (i in seq_along(thresholds)) {
    t <- thresholds[i]
    preds_class <- factor(ifelse(preds_prob > t, "yes", "no"), levels = c("no", "yes"))
    cm <- confusionMatrix(preds_class, actual_labels, positive = "yes", mode = "prec_recall")
    
    results$precision[i] <- cm$byClass["Precision"]
    results$recall[i] <- cm$byClass["Recall"]
    results$f1[i] <- cm$byClass["F1"]
  }
  
  best_idx <- which.max(results$f1)
  return(results[best_idx, ])
}

# ==============================================================================
# MODELL A: UNGEWICHTET (PRIMÄR - FÜR ALLGEMEINEN TARGETING USE CASE)
# ==============================================================================
cat("\n--- STARTE TRAINING: UNGEWICHTETES MODELL (PRIMÄR) ---\n")
log_model_unweighted <- glm(y ~ ., data = train_matrix, family = binomial)
cat("Training ungewichtet abgeschlossen!\n")

# Koeffizienten prüfen
coefs_unweighted <- summary(log_model_unweighted)$coefficients
if ("campaign_month" %in% rownames(coefs_unweighted)) {
  cat("\nSchätzung campaign_month (ungewichtet):\n")
  print(coefs_unweighted["campaign_month", , drop = FALSE])
}

# Evaluation
preds_prob_unweighted <- predict(log_model_unweighted, newdata = test_matrix, type = "response")
best_res_unweighted <- optimize_threshold(preds_prob_unweighted, test_data$y)

cat(sprintf("\nOptimaler Schwellenwert (ungewichtet): %f (Max F1: %f)\n", 
            best_res_unweighted$threshold, best_res_unweighted$f1))

preds_class_unweighted <- factor(ifelse(preds_prob_unweighted > best_res_unweighted$threshold, "yes", "no"), 
                                 levels = c("no", "yes"))
cm_unweighted <- confusionMatrix(preds_class_unweighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_unweighted)


# ==============================================================================
# MODELL B: GEWICHTET (IPW VERGLEICHSMODELL)
# ==============================================================================
cat("\n--- STARTE TRAINING: GEWICHTETES MODELL (IPW BEREINIGT) ---\n")
log_model_weighted <- glm(y ~ ., data = train_matrix, family = binomial, weights = ipw_weights)
cat("Training gewichtet abgeschlossen!\n")

# Koeffizienten prüfen
coefs_weighted <- summary(log_model_weighted)$coefficients
if ("campaign_month" %in% rownames(coefs_weighted)) {
  cat("\nSchätzung campaign_month (gewichtet):\n")
  print(coefs_weighted["campaign_month", , drop = FALSE])
}

# Evaluation
preds_prob_weighted <- predict(log_model_weighted, newdata = test_matrix, type = "response")
best_res_weighted <- optimize_threshold(preds_prob_weighted, test_data$y)

cat(sprintf("\nOptimaler Schwellenwert (gewichtet): %f (Max F1: %f)\n", 
            best_res_weighted$threshold, best_res_weighted$f1))

preds_class_weighted <- factor(ifelse(preds_prob_weighted > best_res_weighted$threshold, "yes", "no"), 
                               levels = c("no", "yes"))
cm_weighted <- confusionMatrix(preds_class_weighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_weighted)


# --- 5. MODELLE SPEICHERN ---
if (!dir.exists(here::here("source", "models_output"))) {
  dir.create(here::here("source", "models_output"))
}
speicher_pfad_unweighted <- here::here("source", "models_output", "logistic_model_unweighted.rds")
speicher_pfad_weighted <- here::here("source", "models_output", "logistic_model_weighted.rds")

saveRDS(log_model_unweighted, file = speicher_pfad_unweighted)
saveRDS(log_model_weighted, file = speicher_pfad_weighted)

cat("\nModelle erfolgreich in source/models_output/ gespeichert.\n")
