# ==============================================================================
# INVERSE PROBABILITY WEIGHTING (IPW) - DATA PREPARATION
# ==============================================================================
# Dieses Skript dient als Single Source of Truth für die Berechnung der IPW-Gewichte.
# Es korrigiert die Selektionsverzerrung (Selection Bias) zwischen der Frühphase
# (Training) und der Spätphase (Test) der Kampagne.

library(here)
library(caret)

# 1. Daten und Basis-Splits laden
source(here::here("source", "models", "ModelData.R"))

cat("\nBerechne IPW-Gewichte zur Korrektur des Selection Bias...\n")

# 2. Relevante Variablen für das Selektionsmodell festlegen
# Wir nutzen demografische Merkmale sowie die historische Kontakthistorie.
# Wir schließen y (Zielvariable) und zeitliche Variablen (month, year, campaign_month) aus,
# da wir das Zielgruppenprofil der Bank modellieren wollen.
selection_features <- c("age", "job", "marital", "education", "default", "balance", 
                        "housing", "loan", "contact", "campaign", "pdays", "previous", "poutcome")

# 3. Datensätze kombinieren und Selektionsindikator erstellen
train_sel <- train_data[, selection_features]
train_sel$is_test <- 0

test_sel <- test_data[, selection_features]
test_sel$is_test <- 1

combined_sel <- rbind(train_sel, test_sel)
combined_sel$is_test <- factor(combined_sel$is_test, levels = c(0, 1), labels = c("train", "test"))

# 4. Propensity-Score-Modell trainieren (Logistische Regression)
set.seed(42)
propensity_model <- glm(is_test ~ ., data = combined_sel, family = binomial)

# 5. Gewichte berechnen: w = P(test | X) / P(train | X)
p_test <- predict(propensity_model, newdata = train_data, type = "response")
p_train <- 1 - p_test
raw_weights <- p_test / p_train

# 6. Weight Trimming (beim 99. Perzentil abschneiden, um extreme Varianz zu vermeiden)
cap_val <- quantile(raw_weights, 0.99)
trimmed_weights <- ifelse(raw_weights > cap_val, cap_val, raw_weights)

# 7. Gewichte standardisieren (Mittelwert = 1.0 zur Beibehaltung der effektiven Stichprobengröße)
ipw_weights <- trimmed_weights / mean(trimmed_weights)

# 8. Gewichte an train_data anhängen
train_data$ipw_weight <- ipw_weights

cat("IPW-Gewichte erfolgreich berechnet!\n")
cat(sprintf("  Min. Gewicht: %f\n", min(ipw_weights)))
cat(sprintf("  Max. Gewicht: %f\n", max(ipw_weights)))
cat(sprintf("  Mittelwert  : %f\n\n", mean(ipw_weights)))
