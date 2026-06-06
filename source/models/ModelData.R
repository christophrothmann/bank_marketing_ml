library(here)

# Set global options
options(scipen = 999)

## SST Daten für das Training der verschiedenen Modelle

Daten <- read.csv(
    here::here("data", "bank-full.csv"),
    header = TRUE,
    sep = ";",
    fill = TRUE,
    stringsAsFactors = TRUE
)

# Den gesamten Datensatz mischen -> NEEEE, evtl. nicht, weil Zeitreihen vorhanden sind
# Folgendes Paket in betracht ziehen: https://cran.r-project.org/web/packages/prophet/index.html
daten_shuffled <- Daten[sample(nrow(Daten)), ]

# Daten aufteilen (erste 80% für Training, restliche 20% für Test)
train_groesse <- round(nrow(daten_shuffled) * 0.8)
train_data <- daten_shuffled[1:train_groesse, ]

# -> Kapitel 10.6 Testdatensatz bei Zeitreihen aus Vorlesungsfolie beachten!! -> aktuell noch nicht integriert
test_data <- daten_shuffled[(train_groesse + 1):nrow(daten_shuffled), ]
