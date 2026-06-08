# ==============================================================================
# MODELL 1c: RANDOM FOREST (SWEET SPOT)
# Ziel: Balance zwischen Sensitivity und Specificity durch In-Tree Downsampling
# ohne den Cutoff zu extrem zu verschieben.
# ==============================================================================

# --- 1. DATEN & PAKETE LADEN ---
library(here)
library(randomForest)
library(caret)

# Daten aus dem Vorbereitungsskript laden
source(here::here("source", "models", "ModelData.R"))

cat("Starte Training: Random Forest (Sweet Spot)...\n")

# --- 2. VORBEREITUNG FÜR DOWNSAMPLING ---
# Wie viele 'yes'-Kunden haben wir exakt im Training?
anzahl_yes <- sum(train_data$y == "yes")

# --- 3. MODELL TRAINIEREN ---
set.seed(42)

rf_model_sweetspot <- randomForest(
  y ~ ., 
  data = train_data, 
  ntree = 500,
  
  # Der goldene Mittelweg: Cutoff bleibt standardmäßig bei 50:50,
  # aber jeder Baum lernt mit einem perfekt balancierten Datensatz (50:50)
  strata = train_data$y,
  sampsize = c(anzahl_yes, anzahl_yes), 
  
  importance = TRUE
)

cat("Training des Sweet-Spot-Modells abgeschlossen!\n")

# --- 4. VORHERSAGE & EVALUATION ---
rf_predictions_sweetspot <- predict(rf_model_sweetspot, newdata = test_data)

cat("\nKonfusionsmatrix - SWEET SPOT Random Forest:\n")
rf_matrix_sweetspot <- confusionMatrix(rf_predictions_sweetspot, test_data$y, positive = "yes")
print(rf_matrix_sweetspot)

# --- 5. MODELL SPEICHERN ---
if (!dir.exists(here::here("models_output"))) { 
  dir.create(here::here("models_output")) 
}

speicher_pfad_sweetspot <- here::here("models_output", "rf_sweetspot_model.rds")
saveRDS(rf_model_sweetspot, file = speicher_pfad_sweetspot)

cat("Sweet-Spot-Modell gespeichert unter:\n", speicher_pfad_sweetspot, "\n")


cat("\nKonfusionsmatrix (mit F1-Score):\n")
rf_matrix_sweetspot <- confusionMatrix(rf_predictions_sweetspot, 
                                       test_data$y, 
                                       positive = "yes", 
                                       mode = "prec_recall") # Das ist der Zauber-Befehl!
print(rf_matrix_sweetspot)
