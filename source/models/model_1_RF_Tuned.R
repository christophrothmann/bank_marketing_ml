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

  # TRICK 1: Abstimmungsregel anpassen
  # Da jeder Baum durch Downsampling (sampsize) mit einem perfekt balancierten
  # 50:50-Datensatz lernt, neigt das Modell dazu, zu viele 'yes' vorherzusagen.
  # Um dies an die reale Verteilung anzupassen (ca. 11.7%), verlangen wir einen
  # Konsens von mindestens 60% der Bäume für ein 'yes'. Dies verhindert Überkorrektur
  # und liefert das beste Ergebnis (F1: 47.2%, Kappa: 0.395, Accuracy: 86.5%).
  cutoff = c(0.4, 0.6),

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
if (!dir.exists(here::here("source", "models_output"))) {
  dir.create(here::here("source", "models_output"))
}

speicher_pfad_tuned <- here::here("source", "models_output", "rf_tuned_model.rds")
saveRDS(rf_model_tuned, file = speicher_pfad_tuned)

cat("Getuntes Modell gespeichert unter:\n", speicher_pfad_tuned, "\n")
