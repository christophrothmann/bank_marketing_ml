# ==============================================================================
# MODELL 1d: RANDOM FOREST (SMOTE)
# Ziel: Klassenimbalance vor dem Training durch künstliche Daten (SMOTE) lösen.
# ==============================================================================

# --- 1. DATEN & PAKETE LADEN ---
library(here)
library(randomForest)
library(caret)
library(performanceEstimation)

# Daten aus dem Vorbereitungsskript laden (Relativer Pfad über 'here'!)
source(here::here("source", "models", "ModelData.R"))

# --- 2. SMOTE ANWENDEN ---
cat("\n--- STARTE SMOTE DATEN-VORBEREITUNG ---\n")
cat("Klassenverteilung VOR SMOTE:\n")
print(table(train_data$y))

set.seed(42) # Wichtig, damit SMOTE immer die gleichen künstlichen Kunden erfindet
train_data_smote <- smote(
    y ~ .,
    data = train_data,
    perc.over = 2,
    perc.under = 2,
    k = 5
)

cat("\nKlassenverteilung NACH SMOTE:\n")
print(table(train_data_smote$y))

# --- 3. MODELL TRAINIEREN ---
cat("\nStarte Training: Random Forest auf SMOTE-Daten...\n")
set.seed(42)

# FUSION: Da SMOTE die Daten jetzt schon ausbalanciert hat, 
# brauchen wir den Sweet-Spot (sampsize) hier nicht mehr. 
# Wir lassen den Standard-Wald einfach auf die neuen SMOTE-Daten los!
rf_model_smote <- randomForest(
  y ~ ., 
  data = train_data_smote, 
  ntree = 500,
  importance = TRUE
)

cat("Training abgeschlossen!\n")

# --- 4. VORHERSAGE & EVALUATION (MIT F1-SCORE) ---
rf_predictions_smote <- predict(rf_model_smote, newdata = test_data)

cat("\nKonfusionsmatrix - SMOTE Random Forest:\n")
# Hier ist unsere neue Boss-Metrik (mode = 'prec_recall') direkt eingebaut!
rf_matrix_smote <- confusionMatrix(rf_predictions_smote, test_data$y, positive = "yes", mode = "prec_recall")
print(rf_matrix_smote)

# --- 5. MODELL SPEICHERN ---
if (!dir.exists(here::here("models_output"))) { dir.create(here::here("models_output")) }

speicher_pfad_smote <- here::here("models_output", "rf_smote_model.rds")
saveRDS(rf_model_smote, file = speicher_pfad_smote)

cat("\nSMOTE-Modell gespeichert unter:\n", speicher_pfad_smote, "\n")
