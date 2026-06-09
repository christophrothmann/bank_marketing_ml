## Kommentar von Gemini warum das so schlecht Funktioniert:
# Da der Datensatz chronologisch sortiert ist (von Mai 2008 bis November 2010), führt diese Aufteilung dazu, dass das Modell auf den Jahren 2008 und Anfang 2009 trainiert wird, aber auf Ende 2009 und 2010 getestet wird. In dieser Zeit hat sich das Kundenverhalten und die Kampagnenstrategie der Bank grundlegend verändert:

# A. Massive Verschiebung der Zielvariable ($y$)
# Im Trainingsset: Nur 5,81 % der Kunden haben ein Festgeld abgeschlossen (yes).
# Im Testset: Ganze 25,43 % der Kunden haben abgeschlossen (yes) – das ist ein Anstieg um das Vierfache!
# Das Modell lernt im Training, dass ein Abschluss (yes) ein extrem seltenes Ereignis ist, und prognostiziert daher viel zu häufig ein "no".

# B. Fehlendes Lernen des wichtigsten Features (poutcome == "success")
# Im Trainingsset (Anfangsphase der Kampagne): Fast alle Kunden wurden zum ersten Mal angerufen. Die Variable poutcome (Ergebnis der vorherigen Kampagne) steht bei 93,5 % der Fälle auf "unknown". Es gab im gesamten Trainingsset von 31.648 Zeilen nur 81 Fälle, bei denen poutcome auf "success" stand (wovon sogar nur 15 Kunden wieder mit yes geantwortet haben). Das Modell konnte also überhaupt nicht lernen, dass ein vorheriger Erfolg ein extrem starker Prädiktor für einen erneuten Abschluss ist!
# Im Testset (Spätphase der Kampagne): Hier wurden gezielt Kunden wiederholt kontaktiert. Es gibt 1.430 Fälle mit poutcome == "success". Die tatsächliche Abschlussrate bei diesen Kunden im Testset lag bei 67,3 % (963 von 1.430 Kunden sagten yes).
# Die Folge: Weil das Modell die Bedeutung von poutcome == "success" im Training nicht erlernen konnte, stuft es diese warmen Leads im Testset falsch ein. Es prognostiziert für 75 % dieser eigentlich sehr erfolgreichen Zielgruppe ein "no". Das führt zu 717 False Negatives (FN) allein in dieser Gruppe und zieht deinen Recall drastisch nach unten.
# C. Saisonaler Shift der Monate
# Da im Datensatz keine Spalte für das year (Jahr) existiert, sieht das Modell nur die nominale Variable month (z. B. "may", "jul"). Weil die Daten chronologisch getrennt sind, repräsentieren dieselben Monate in Training und Test völlig andere Phasen der Kampagne:
# Der Monat jul hatte im Training (2008) eine Erfolgsquote von 6,0 %. Im Testset (2009/2010) lag sie bei 47,2 %!
# Der Monat nov hatte im Training eine Erfolgsquote von 6,1 %, im Testset jedoch 49,2 %!
# Das Modell lernt im Training fälschlicherweise, dass diese Monate "schlechte" Monate sind, obwohl sie im Testzeitraum extrem hohe Abschlussquoten aufweisen.


# ==============================================================================
# MODELL 2: XGBoost (Das Performance-Biest) - CLEAN VERSION
# ==============================================================================
required_packages <- c("xgboost", "caret", "here")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cran.rstudio.com/")
}
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
dummy_model <- dummyVars(~., data = train_features)
train_matrix <- predict(dummy_model, newdata = train_features)
test_matrix <- predict(dummy_model, newdata = test_features)

# 2c. Das spezielle XGBoost-Format erstellen
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# --- 3. IMBALANCE-BEHANDLUNG & SCHWELLENWERT ---
# Option A: Modell normal trainieren (unweighted) und den Klassifikations-Schwellenwert
# auf 0.2 anpassen (da die Abschlussquote im Datensatz bei ca. 11.7% liegt).
# Dies liefert die besten Gesamtergebnisse (F1-Score: 46.5%, Kappa: 0.387, Accuracy: 86.3%).
use_scale_pos_weight <- FALSE # Auf TRUE setzen für Option B

# Option B (Gewichtung): Wie viele 'no' auf ein 'yes' kommen (ca. 7.55 im zufälligen Split)
anzahl_no <- sum(train_labels == 0)
anzahl_yes <- sum(train_labels == 1)
gewichtung <- anzahl_no / anzahl_yes

# --- 4. MODELL TRAINIEREN ---
cat("Starte Training: XGBoost...\n")
set.seed(42)

xgb_model <- xgb.train(
  data = dtrain,
  nrounds = 100, # 100 Bäume bauen
  params = list(
    objective = "binary:logistic", # Wir wollen ja/nein (0/1) vorhersagen
    eval_metric = "auc", # Boss-Metrik für das interne Lernen
    scale_pos_weight = if (use_scale_pos_weight) gewichtung else 1.0,
    max_depth = 6, # Maximale Baumtiefe
    eta = 0.3 # Lerngeschwindigkeit
  )
)
cat("Training abgeschlossen!\n")

# --- 5. VORHERSAGE & EVALUATION ---
xgb_preds_prob <- predict(xgb_model, dtest)

# Schwellenwert (Threshold) festlegen
# Wenn scale_pos_weight aktiv ist, nutzen wir 0.5. Ohne Gewichtung ist 0.2 optimal.
threshold <- if (use_scale_pos_weight) 0.5 else 0.2
cat(sprintf("Nutze Klassifikations-Schwellenwert: %f\n", threshold))

xgb_preds_class <- ifelse(xgb_preds_prob > threshold, "yes", "no")
# Umwandeln in Factor für die Matrix
xgb_preds_factor <- factor(xgb_preds_class, levels = c("no", "yes"))


cat("\nKonfusionsmatrix - XGBoost:\n")
xgb_matrix <- confusionMatrix(xgb_preds_factor, test_data$y, positive = "yes", mode = "prec_recall")
print(xgb_matrix)

# --- 6. MODELL SPEICHERN ---
if (!dir.exists(here::here("source", "models_output"))) {
  dir.create(here::here("source", "models_output"))
}
speicher_pfad_xgb <- here::here("source", "models_output", "xgboost_model.rds")
saveRDS(xgb_model, file = speicher_pfad_xgb)
cat("\nXGBoost-Modell gespeichert unter:\n", speicher_pfad_xgb, "\n")
