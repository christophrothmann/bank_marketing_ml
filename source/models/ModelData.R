library(here)

# Set global options
options(scipen = 999)

Daten <- read.csv(
    here::here("data", "bank-full.csv"),
    header = TRUE,
    sep = ";",
    fill = TRUE,
    stringsAsFactors = TRUE
)

# Variable "duration" entfernen, da kaum aussagekraft über Ausgang von "y" hat.
Daten <- Daten[, !names(Daten) %in% "duration"]

# Chronologische Aufteilung ohne Mischen, da Zeitreihen vorliegen.
N <- nrow(Daten)

# --- 1. Standard-Aufteilung (70% Training / 30% Test) ---
train_groesse_70 <- round(N * 0.7)

train_data <- Daten[1:train_groesse_70, ]
test_data <- Daten[(train_groesse_70 + 1):N, ]

# --- 2. Dreiteilung (50% Training / 20% Validierung / 30% Test) ---
train_groesse_50 <- round(N * 0.5)
val_groesse_20 <- round(N * 0.2)
val_end <- train_groesse_50 + val_groesse_20

train_data_3way <- Daten[1:train_groesse_50, ]
val_data_3way <- Daten[(train_groesse_50 + 1):val_end, ]
test_data_3way <- Daten[(val_end + 1):N, ]
