# ==============================================================================
# SHAP-WERTE BERECHNEN UND BEESWARM PLOT GENERIEREN (XGBOOST INTERPRETATION)
# ==============================================================================
# Dieses Skript nutzt die integrierte SHAP-Wert-Berechnung von XGBoost
# (predcontrib = TRUE), um den Beitrag jedes Features zu berechnen.
# Es visualisiert die Ergebnisse als Beeswarm-Plot (Jitter-Scatter-Plot).

library(here)
library(xgboost)
library(caret)
library(dplyr)
library(ggplot2)

# 1. Daten und XGBoost-Modell laden
model <- readRDS(here::here("source", "models_output", "xgboost_model_unweighted.rds"))
source(here::here("source", "models", "ipw_data.R"))

cat("Bereite Daten für SHAP-Berechnung vor...\n")

# Features aufbereiten (wie beim XGBoost Training)
train_features <- train_data[, !names(train_data) %in% c("y", "ipw_weight")]
test_features <- test_data[, names(test_data) != "y"]

dummy_model <- dummyVars(~., data = train_features)
test_matrix <- predict(dummy_model, newdata = test_features)
test_labels <- ifelse(test_data$y == "yes", 1, 0)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# 2. SHAP-Werte berechnen
cat("Berechne SHAP-Werte (Beiträge der Features zur Wahrscheinlichkeit)...\n")
shap_matrix <- predict(model, dtest, predcontrib = TRUE)

# Die letzte Spalte enthält den Basiswert (Intercept), wir schließen ihn aus
shap_features <- shap_matrix[, -ncol(shap_matrix)]

# Sortieren der Features nach globaler Wichtigkeit (mittlerer absoluter SHAP-Wert)
mean_shap <- colMeans(abs(shap_features))
top_features <- names(sort(mean_shap, decreasing = TRUE))[1:16]

# 3. Aufbau eines Data Frames für den Beeswarm-Plot
cat("Erstelle Beeswarm-Datensatz...\n")
beeswarm_list <- list()

for (f in top_features) {
  shap_val <- shap_features[, f]
  act_val <- test_matrix[, f]

  # Min-Max Skalierung für Merkmalswerte (Wertebereich [0, 1] für einheitliche Farbskala)
  min_val <- min(act_val)
  max_val <- max(act_val)
  if (max_val > min_val) {
    norm_val <- (act_val - min_val) / (max_val - min_val)
  } else {
    norm_val <- rep(0.5, length(act_val))
  }

  # Aus Performance- und Übersichtlichen Gründen wählen wir 2000 zufällige Punkte für die Grafik
  set.seed(42)
  samp_idx <- sample(1:length(shap_val), min(2000, length(shap_val)))

  beeswarm_list[[f]] <- data.frame(
    Feature = f,
    SHAP_Value = shap_val[samp_idx],
    Feature_Value_Norm = norm_val[samp_idx],
    Mean_Abs_SHAP = mean_shap[f]
  )
}

df_beeswarm <- do.call(rbind, beeswarm_list)

# 4. Beeswarm-Plot mit ggplot2 erstellen
cat("Generiere SHAP-Beeswarm-Plot...\n")
p <- ggplot(df_beeswarm, aes(x = SHAP_Value, y = reorder(Feature, Mean_Abs_SHAP), color = Feature_Value_Norm)) +
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +
  # Jittering zur Erzeugung des typischen Beeswarm-Effekts
  geom_jitter(alpha = 0.5, size = 1.0, height = 0.25, width = 0) +
  # Rot-Blau Farbverlauf für Merkmalswerte
  scale_color_gradient(
    low = "#1f77b4", # Blau (Niedrig)
    high = "#d62728", # Rot (Hoch)
    breaks = c(0, 1),
    labels = c("Niedrig", "Hoch"),
    name = "Merkmalswert"
  ) +
  labs(
    title = "XGBoost SHAP Beeswarm Plot (Top 10 Features)",
    subtitle = "Jeder Punkt steht für einen Kunden. Rot = hoher Merkmalswert, Blau = niedriger Wert.",
    x = "SHAP-Wert (Einfluss auf die Abschlusswahrscheinlichkeit)",
    y = "Feature"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 15)),
    panel.grid.minor = element_blank()
  )

# Grafik speichern
output_plot_path <- here::here("XGBoost_SHAP_Beeswarm.png")
ggsave(output_plot_path, plot = p, width = 10, height = 6, dpi = 300)

cat(sprintf("\nBeeswarm-Plot erfolgreich gespeichert unter:\n %s\n", output_plot_path))
