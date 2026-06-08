# ==============================================================================
# MODELL 2: XGBoost (Das Performance-Biest) - CLEAN VERSION
# ==============================================================================

# --- 1. DATEN & PAKETE LADEN ---
library(here)
library(xgboost)
library(caret)

source(here::here("source", "models", "ModelData.R"))

cat("Bereite Daten für XGBoost vor (One-Hot-Encoding)...\n")

# --- 2. DATEN FÜR XGBOOST ÜBERSETZEN ---
# 2a. Labels extrahieren und in 0 und 1 umwandeln
train_labels <- ifelse(train_data$y == "yes", 1, 0)
test_labels <- ifelse(test_data$y == "yes", 1, 0)

# 2b. Die 'y'-Spalte aus den Daten entfernen, damit 'dummyVars' nicht meckert (Löst Warnung 1 & 2)
train_features <- train_data[, names(train_data) != "y"]
test_features <- test_data[, names(test_data) != "y"]

# Alle Text-Spalten in numerische Matrizen umwandeln (One-Hot-Encoding)
dummy_model <- dummyVars(~ ., data = train_features)
train_matrix <- predict(dummy_model, newdata = train_features)
test_matrix <- predict(dummy_model, newdata = test_features)

# 2c. Das spezielle XGBoost-Format erstellen
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# --- 3. DIE WUNDERWAFFE GEGEN IMBALANCE ---
# Wir berechnen, wie viele 'no' auf ein 'yes' kommen (ca. 8.8)
anzahl_no <- sum(train_labels == 0)
anzahl_yes <- sum(train_labels == 1)
gewichtung <- anzahl_no / anzahl_yes

# --- 4. MODELL TRAINIEREN ---
cat("Starte Training: XGBoost...\n")
set.seed(42)

# Löst Warnung 3 & 4: Moderne XGBoost-Versionen wollen Parameter als Liste haben!
xgb_model <- xgb.train(
  data = dtrain,
  nrounds = 100,                     # 100 Bäume bauen
  params = list(
    objective = "binary:logistic",   # Wir wollen ja/nein (0/1) vorhersagen
    eval_metric = "auc",             # Boss-Metrik für das interne Lernen
    scale_pos_weight = gewichtung,   # Ersetzt SMOTE und Sweet-Spot!
    max_depth = 6,                   # Maximale Baumtiefe
    eta = 0.3                        # Lerngeschwindigkeit
  )
)
cat("Training abgeschlossen!\n")

# --- 5. VORHERSAGE & EVALUATION ---
xgb_preds_prob <- predict(xgb_model, dtest)

# Alles über 50% Wahrscheinlichkeit nennen wir "yes"
xgb_preds_class <- ifelse(xgb_preds_prob > 0.5, "yes", "no")
# Umwandeln in Factor für die Matrix
xgb_preds_factor <- factor(xgb_preds_class, levels = c("no", "yes"))

cat("\nKonfusionsmatrix - XGBoost:\n")
xgb_matrix <- confusionMatrix(xgb_preds_factor, test_data$y, positive = "yes", mode = "prec_recall")
print(xgb_matrix)

# --- 6. MODELL SPEICHERN ---
if (!dir.exists(here::here("models_output"))) { dir.create(here::here("models_output")) }
speicher_pfad_xgb <- here::here("models_output", "xgboost_model.rds")
saveRDS(xgb_model, file = speicher_pfad_xgb)
cat("\nXGBoost-Modell gespeichert unter:\n", speicher_pfad_xgb, "\n")
