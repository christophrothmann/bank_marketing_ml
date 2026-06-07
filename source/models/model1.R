# 1. Die andere Datei einlesen
# den pfad noch irgendwie anpassen, damit er einen relativen und keinen absoluten pfad hier hat
source("~/Nextcloud/Maschinelles Lernen/bank_marketing/source/models/ModelData.R")

## @Lola: Hab hier schonmal versucht, mit SMOTE die class-imbalance zu taklen. Check gerne drüber ob das sinn macht.

# Zuerst die Abhängigkeiten installieren
utils::install.packages(c("parallelMap", "tidyr"), repos = "https://packagemanager.posit.co/cran/latest")

# performanceEstimation direkt aus dem CRAN-Archiv installieren
utils::install.packages(
    "https://cran.r-project.org/src/contrib/Archive/performanceEstimation/performanceEstimation_1.1.0.tar.gz",
    repos = NULL,
    type = "source"
)


# 2. Paket laden
library(performanceEstimation)
# 3. Verteilung vor SMOTE prüfen
cat("Klassenverteilung vor SMOTE:\n")
print(table(train_data$y))

train_data_smote <- smote(
    y ~ .,
    data = train_data,
    perc.over = 2,
    perc.under = 2,
    k = 5
)

# 5. Verteilung nach SMOTE prüfen
cat("\nKlassenverteilung nach SMOTE:\n")
print(table(train_data_smote$y))

# 2. Variablen hinladen in die Datei

# 3. Model auswählen und trainieren

# 4. Mit dem Testdatensatz die Prognosegüte überprüfen (Fehlerquoten etc.)

# 5. Evaluation im Konfusionsmatrix

# 6. ROC Kurve und AUC berechnen, interpretieren

# 7. Zusammenfassung und kritische Bewertung der Ergebnisse

# 8. Model abspeichern im Ordner "models_output"

# 2. Auf die Variablen zugreifen damit wir für jedes Model den selben Trainings- und Testdatensatz
print(train_data[0:5, ])
