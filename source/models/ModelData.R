library(here)
library(caret)
set.seed(42)
# Set global options
options(scipen = 999)

Daten <- read.csv(
    here::here("data", "bank-full.csv"),
    header = TRUE,
    sep = ";",
    fill = TRUE,
    stringsAsFactors = TRUE
)

# Variable "duration" entfernen, da sie Data Leakage betreibt. Man weiß erst danach wie lange insgesamt telefoniert wurde.
Daten <- Daten[, !names(Daten) %in% "duration"]

# Erstellt einen geschichteten (stratified) zufälligen Split basierend auf der Zielvariable y (70% Training / 30% Test)
train_indices <- createDataPartition(Daten$y, p = 0.7, list = FALSE)
train_data <- Daten[train_indices, ]
test_data <- Daten[-train_indices, ]

# --- 2. Dreiteilung (50% Training / 20% Validierung / 30% Test) ---
# Hier teilen wir ebenfalls geschichtet auf, um die Verteilung zu wahren.
test_data_3way <- test_data # 30% Testdaten bleiben gleich

# Die verbleibenden 70% teilen wir im Verhältnis 50:20 auf (ca. 71.4% Train, 28.6% Val)
val_indices <- createDataPartition(train_data$y, p = 2/7, list = FALSE)
val_data_3way <- train_data[val_indices, ]
train_data_3way <- train_data[-val_indices, ]

