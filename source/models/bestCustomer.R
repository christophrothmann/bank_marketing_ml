library(here)
library(caret)
library(dplyr)

# 1. Daten und logistisches Modell laden
model <- readRDS(here::here("source", "models_output", "logistic_model_unweighted.rds"))
source(here::here("source", "models", "ModelData.R"))

# Features aufbereiten
train_features <- train_data[, !names(train_data) %in% c("y", "month", "ipw_weight")]
test_features <- test_data[, !names(test_data) %in% c("y", "month")]

dummy_model <- dummyVars(~., data = train_features)
test_matrix <- as.data.frame(predict(dummy_model, newdata = test_features))

# 2. Wahrscheinlichkeiten für das Testset vorhersagen
test_data$prob <- predict(model, newdata = test_matrix, type = "response")

# 3. Die Top 10% der Kunden mit der höchsten Kaufwahrscheinlichkeit filtern
cutoff_prob <- quantile(test_data$prob, 0.90)
top_customers <- test_data %>% filter(prob >= cutoff_prob)
average_customers <- test_data

cat("\n==================================================\n")
cat("TEIL A: KUNDEN-PROFILING (BUYER PERSONA)\n")
cat("==================================================\n")
cat(sprintf("Schwellenwert für die Top 10%%: Wahrscheinlichkeit >= %.2f%%\n\n", cutoff_prob * 100))

# A. Vergleich des Alters (Durchschnitt)
cat(sprintf("Durchschnittliches Alter:\n  Top 10%% Kunden: %.1f Jahre\n  Gesamte Kunden : %.1f Jahre\n\n", 
            mean(top_customers$age), mean(average_customers$age)))

# B. Vergleich des Kontostands (Median)
cat(sprintf("Kontostand (Median):\n  Top 10%% Kunden: %.2f EUR\n  Gesamte Kunden : %.2f EUR\n\n", 
            median(top_customers$balance), median(average_customers$balance)))

# C. Top-Berufe (in Prozent)
cat("Top-Berufe im Vergleich:\n")
top_jobs <- prop.table(table(top_customers$job)) * 100
avg_jobs <- prop.table(table(average_customers$job)) * 100
jobs_df <- data.frame(
  Job = names(top_jobs),
  Top_10_Percent = as.numeric(top_jobs),
  All_Customers = as.numeric(avg_jobs[names(top_jobs)])
) %>% arrange(desc(Top_10_Percent))
print(head(jobs_df, 5))

# D. Kredit- und Hausbesitz (in Prozent)
cat("\nFinanzielle Verpflichtungen:\n")
cat(sprintf("  Immobilienkredit (housing = 'yes'):\n    Top 10%% Kunden: %.1f%%\n    Gesamte Kunden : %.1f%%\n",
            mean(top_customers$housing == "yes") * 100, mean(average_customers$housing == "yes") * 100))
cat(sprintf("  Privatkredit (loan = 'yes'):\n    Top 10%% Kunden: %.1f%%\n    Gesamte Kunden : %.1f%%\n",
            mean(top_customers$loan == "yes") * 100, mean(average_customers$loan == "yes") * 100))

# E. Kontakthistorie (poutcome)
cat("\nErgebnis der vorherigen Kampagne (poutcome):\n")
top_poutcome <- prop.table(table(top_customers$poutcome)) * 100
avg_poutcome <- prop.table(table(average_customers$poutcome)) * 100
poutcome_df <- data.frame(
  Outcome = names(top_poutcome),
  Top_10_Percent = as.numeric(top_poutcome),
  All_Customers = as.numeric(avg_poutcome[names(top_poutcome)])
) %>% arrange(desc(Top_10_Percent))
print(poutcome_df)


cat("\n==================================================\n")
cat("TEIL B: REGRESSIONSKOEFFIZIENTEN & ODDS RATIOS\n")
cat("==================================================\n")

# Koeffizienten auslesen und Odds Ratios berechnen
coefs <- as.data.frame(summary(model)$coefficients)
coefs$Odds_Ratio <- exp(coefs$Estimate)
colnames(coefs) <- c("Estimate", "StdError", "zValue", "pValue", "OddsRatio")

# Signifikante Koeffizienten filtern (p < 0.05)
sig_coefs <- coefs[coefs$pValue < 0.05, ]

cat("Signifikante positive Faktoren (erhöhen Abschlusswahrscheinlichkeit):\n")
print(sig_coefs[sig_coefs$Estimate > 0, ] %>% 
        select(Estimate, pValue, OddsRatio) %>% 
        arrange(desc(Estimate)))

cat("\nSignifikante negative Faktoren (senken Abschlusswahrscheinlichkeit):\n")
print(sig_coefs[sig_coefs$Estimate < 0, ] %>% 
        select(Estimate, pValue, OddsRatio) %>% 
        arrange(Estimate))
cat("==================================================\n")
