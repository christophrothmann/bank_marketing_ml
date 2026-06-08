# ==============================================================================
# MODELL 1b: RANDOM FOREST (GETUNT)
# Ziel: Sensitivity drastisch erhöhen durch Cutoff & Downsampling (sampsize).
# ==============================================================================

# --- 1. DATEN & PAKETE LADEN ---
library(here)
library(randomForest)
library(caret)

source(here::here("source", "models", "ModelData.R"))

cat("Starte Training: Random Forest (Getunt)...\n")

# --- 2. VORBEREITUNG FÜR DOWNSAMPLING ---
# Wie viele 'yes'-Kunden haben wir exakt im Training?
anzahl_yes <- sum(train_data$y == "yes")

# --- 3. MODELL TRAINIEREN ---
set.seed(42)

rf_model_tuned <- randomForest(
  y ~ ., 
  data = train_data, 
  ntree = 500,
  
  # TRICK 1: Abstimmungsregel anpassen (Schon ab 20% 'yes'-Stimmen wird angerufen)
  cutoff = c(0.8, 0.2), 
  
  # TRICK 2: Jeder Baum bekommt exakt gleich viele 'yes' wie 'no' (faire Chance)
  strata = train_data$y,
  sampsize = c(anzahl_yes, anzahl_yes), 
  
  importance = TRUE
)

cat("Training des getunten Modells abgeschlossen!\n")

# --- 4. VORHERSAGE & EVALUATION ---
# WICHTIG: Hier nutzen wir strikt die Variablen mit '_tuned' am Ende!
rf_predictions_tuned <- predict(rf_model_tuned, newdata = test_data)

cat("\nKonfusionsmatrix - GETUNTER Random Forest:\n")
rf_matrix_tuned <- confusionMatrix(rf_predictions_tuned, test_data$y, positive = "yes")
print(rf_matrix_tuned)

# --- 5. MODELL SPEICHERN ---
if (!dir.exists(here::here("models_output"))) { dir.create(here::here("models_output")) }

speicher_pfad_tuned <- here::here("models_output", "rf_tuned_model.rds")
saveRDS(rf_model_tuned, file = speicher_pfad_tuned)

cat("Getuntes Modell gespeichert unter:\n", speicher_pfad_tuned, "\n")