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

# --- 1. FEATURE ENGINEERING: ZEIT-FEATURES ---
# Monate in Zahlen mappen
month_map <- c(jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6, jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12)
Daten$month_num <- month_map[as.character(Daten$month)]


# Jahr rekonstruieren über chronologische Monats-Runs
runs <- rle(as.character(Daten$month))
num_runs <- length(runs$lengths)
run_years <- numeric(num_runs)
current_year <- 2008
run_years[1] <- current_year

for (i in 2:num_runs) {
  prev_month_num <- month_map[runs$values[i - 1]]
  curr_month_num <- month_map[runs$values[i]]
  if (curr_month_num <= prev_month_num) {
    current_year <- current_year + 1
  }
  run_years[i] <- current_year
}

Daten$year <- rep(run_years, runs$lengths)

summary(Daten)
# Kontinuierlicher Zeit-Index (Monate seit Kampagnenstart)
Daten$campaign_month <- (Daten$year - 2008) * 12 + Daten$month_num - 5 + 1

# Hilfsspalte month_num wieder entfernen (wird nicht benötigt, da month als Faktor existiert)
Daten$month_num <- NULL

# Variable "duration" entfernen, da sie Data Leakage betreibt
Daten <- Daten[, !names(Daten) %in% "duration"]

# --- 2. CHRONOLOGISCHER SPLIT (70% Training / 30% Test) ---
# Da die Daten bereits chronologisch sortiert sind, teilen wir sie über den Index auf.
train_size <- floor(0.7 * nrow(Daten))
train_data <- Daten[1:train_size, ]
test_data <- Daten[(train_size + 1):nrow(Daten), ]

# --- 3. DREITEILUNG (50% Training / 20% Validierung / 30% Test) ---
val_size <- floor(0.2 * nrow(Daten))
train_data_3way <- Daten[1:(train_size - val_size), ]
val_data_3way <- Daten[(train_size - val_size + 1):train_size, ]
test_data_3way <- test_data
