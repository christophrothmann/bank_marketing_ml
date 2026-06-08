# ==============================================================================
# MODELL 1a: RANDOM FOREST (BASELINE MIT CLASS WEIGHTS)
# Ziel: Zeigen, dass einfache Class Weights bei diesen Daten nicht ausreichen.
# ==============================================================================

# --- 1. DATEN & PAKETE LADEN ---
library(here)
library(randomForest)
library(caret)

# Daten aus dem Vorbereitungsskript laden
source(here::here("source", "models", "ModelData.R"))

cat("Starte Training: Random Forest (Baseline)...\n")

# --- 2. MODELL TRAINIEREN ---
set.seed(42) # Wichtig für exakt gleiche Ergebnisse bei jedem Durchlauf

rf_model_baseline <- randomForest(
  y ~ ., 
  data = train_data, 
  ntree = 500,
  # Wir gewichten 'yes' 8-mal stärker, aber das reicht hier noch nicht
  classwt = c("no" = 1, "yes" = 50), 
  importance = TRUE
)

cat("Training der Baseline abgeschlossen!\n")

# --- 3. VORHERSAGE & EVALUATION ---
rf_predictions_baseline <- predict(rf_model_baseline, newdata = test_data)

cat("\nKonfusionsmatrix - BASELINE Random Forest:\n")
rf_matrix_baseline <- confusionMatrix(rf_predictions_baseline, test_data$y, positive = "yes")
print(rf_matrix_baseline)

# --- 4. MODELL SPEICHERN ---
if (!dir.exists(here::here("models_output"))) { dir.create(here::here("models_output")) }

speicher_pfad_baseline <- here::here("models_output", "rf_baseline_model_mit50Gewicht.rds")
saveRDS(rf_model_baseline, file = speicher_pfad_baseline)

cat("Baseline-Modell gespeichert unter:\n", speicher_pfad_baseline, "\n")