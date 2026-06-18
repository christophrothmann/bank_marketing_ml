# ==============================================================================
# MODELL 1b: RANDOM FOREST (UNGEWICHTET VS. GEWICHTET)
# ==============================================================================

# --- 1. DATEN & PAKETE LADEN ---
library(here)
library(randomForest)
library(caret)

# IPW-Vorbereitungsskript laden (enthält ModelData.R und ipw_weights)
source(here::here("source", "models", "ipw_data.R"))

cat("Bereite Daten für Random Forest vor...\n")

# ipw_weight und y aus Features entfernen
train_features_names <- names(train_data)[!names(train_data) %in% c("ipw_weight")]
train_clean <- train_data[, train_features_names]

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
# MODELL A: UNGEWICHTET (PRIMÄR)
# ==============================================================================
cat("\n--- STARTE TRAINING: UNGEWICHTETES RANDOM FOREST (PRIMÄR) ---\n")
anzahl_yes_unweighted <- sum(train_clean$y == "yes")

set.seed(42)
rf_model_unweighted <- randomForest(
  y ~ .,
  data = train_clean,
  ntree = 500,
  strata = train_clean$y,
  sampsize = c(anzahl_yes_unweighted, anzahl_yes_unweighted),
  importance = TRUE
)
cat("Training ungewichtetes Random Forest abgeschlossen!\n")

# Vorhersage (Probabilities) & Evaluation
preds_prob_unweighted <- predict(rf_model_unweighted, newdata = test_data, type = "prob")[, "yes"]
best_res_unweighted <- optimize_threshold(preds_prob_unweighted, test_data$y)

cat(sprintf("\nOptimaler Schwellenwert (ungewichtet): %f (Max F1: %f)\n", 
            best_res_unweighted$threshold, best_res_unweighted$f1))

preds_class_unweighted <- factor(ifelse(preds_prob_unweighted > best_res_unweighted$threshold, "yes", "no"), 
                                 levels = c("no", "yes"))
cm_unweighted <- confusionMatrix(preds_class_unweighted, test_data$y, positive = "yes", mode = "prec_recall")
print(cm_unweighted)


# ==============================================================================
# MODELL B: GEWICHTET (IPW BEREINIGT DURCH GEWICHTETES BOOTSTRAP RESAMPLING)
# ==============================================================================
cat("\n--- STARTE TRAINING: GEWICHTETES RANDOM FOREST (IPW BEREINIGT) ---\n")

# Gewichtetes Resampling der Trainingsdaten proportional zu den IPW-Gewichten
set.seed(42)
weighted_indices <- sample(1:nrow(train_clean), size = nrow(train_clean), replace = TRUE, prob = train_data$ipw_weight)
train_clean_weighted <- train_clean[weighted_indices, ]

anzahl_yes_weighted <- sum(train_clean_weighted$y == "yes")

set.seed(42)
rf_model_weighted <- randomForest(
  y ~ .,
  data = train_clean_weighted,
  ntree = 500,
  strata = train_clean_weighted$y,
  sampsize = c(anzahl_yes_weighted, anzahl_yes_weighted),
  importance = TRUE
)
cat("Training gewichtetes Random Forest abgeschlossen!\n")

# Vorhersage (Probabilities) & Evaluation
preds_prob_weighted <- predict(rf_model_weighted, newdata = test_data, type = "prob")[, "yes"]
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
speicher_pfad_unweighted <- here::here("source", "models_output", "rf_tuned_model_unweighted.rds")
speicher_pfad_weighted <- here::here("source", "models_output", "rf_tuned_model_weighted.rds")

saveRDS(rf_model_unweighted, file = speicher_pfad_unweighted)
saveRDS(rf_model_weighted, file = speicher_pfad_weighted)

cat("\nRandom Forest-Modelle erfolgreich in source/models_output/ gespeichert.\n")
