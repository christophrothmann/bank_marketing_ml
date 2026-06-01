library(dplyr)
library(here)
library(ggplot2)
options(scipen = 999)

Daten <- read.csv(here::here("data", "bank-full.csv"), header = TRUE, sep = ";", fill = TRUE, stringsAsFactors = TRUE)
summary(Daten)

##########################################
# Verbindung zwischen pdays und y
##########################################
# --- pdays temporär für den Plot in eine kategoriale Variable umwandeln ---
# (Da pdays an dieser Stelle im Code noch Zahlen wie -1 enthält)
Daten$pdays_cat <- ifelse(Daten$pdays == -1 | Daten$pdays == 999, "nicht kontaktiert", "kontaktiert")

# 1. Kontingenztafel (wie in deiner Übersicht) - Ausgabe in der Konsole
print("Kontingenztafel:")
print(table(Daten$pdays_cat, Daten$y))

# 2. Balkendiagramm mit ggplot2 (zeigt prozentuale Verteilung)
ggplot(Daten, aes(x = pdays_cat, fill = y)) +
  geom_bar(position = "fill") + # 'position = "fill"' normiert die Balken auf 100%
  theme_minimal() +
  labs(
    title = "Einfluss von vorherigem Kontakt auf den Abschluss",
    x = "Vorheriger Kontakt (pdays)",
    y = "Anteil in Prozent",
    fill = "Abschluss (y)"
  ) +
  # Verwendet das scales-Paket für ein Prozent-Format auf der y-Achse
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen"))

# 3. Mosaikplot (ebenfalls aus deiner Tabelle)
mosaicplot(table(Daten$pdays_cat, Daten$y),
  main = "Mosaikplot: Kontakt vs. Abschluss",
  xlab = "Vorheriger Kontakt",
  ylab = "Abschluss (y)",
  col = c("lightcoral", "lightgreen")
)


# --- Betrachtung der TATSÄCHLICHEN Tage (nur kontaktierte Personen) ---
# Daten filtern: Nur Kunden, die vorher kontaktiert wurden
Daten_contacted <- subset(Daten, pdays != -1 & pdays != 999)

# Boxplot nach deiner Tabelle (metrisch vs. nominal = Boxplot)
ggplot(Daten_contacted, aes(x = y, y = pdays, fill = y)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Einfluss der verstrichenen Tage (pdays) auf den Abschluss",
    subtitle = "Ausschließlich Personen, die zuvor kontaktiert wurden",
    x = "Abschluss (y)",
    y = "Tage seit letztem Kontakt (pdays)"
  ) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen"))

##########################################
# Verbindung zwischen job und y
##########################################
ggplot(Daten, aes(x = job, fill = y)) +
  geom_bar(position = "fill") + # auf 100% normiert
  theme_minimal() +
  labs(
    title = "Einfluss des Berufs (job) auf den Abschluss",
    x = "Berufstyp",
    y = "Anteil in Prozent",
    fill = "Abschluss (y)"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Labels drehen für bessere Lesbarkeit

# --- 2. Kontingenztafel ---
print("Kontingenztafel für job vs y:")
print(table(Daten$job, Daten$y))

# --- 3. Statistische Messung des Einflusses in Prozent (Pseudo-R²) ---
# Wir nutzen eine Logistische Regression, um den Erklärungsgehalt zu bestimmen
modell_job <- glm(y ~ job, data = Daten, family = "binomial")
null_modell <- glm(y ~ 1, data = Daten, family = "binomial")

# McFadden Pseudo-R² berechnen
mcfadden_job <- 1 - as.numeric(logLik(modell_job) / logLik(null_modell))

print(paste("Erklärte Varianz durch den Beruf (McFadden Pseudo-R²):", round(mcfadden_job * 100, 2), "%"))



##########################################
# Verbindung zwischen poutcome und y
##########################################

# --- 1. Kontingenztafel ---
print("Kontingenztafel für poutcome vs y:")
print(table(Daten$poutcome, Daten$y))

# --- 2. Balkendiagramm (Visuelle Bestätigung) ---
ggplot(Daten, aes(x = poutcome, fill = y)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(
    title = "Einfluss des vorherigen Kampagnenergebnisses (poutcome)",
    x = "Ergebnis der vorherigen Kampagne",
    y = "Anteil in Prozent",
    fill = "Abschluss (y)"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen"))

# --- 3. Statistische Messung (Pseudo-R² zum Vergleich mit 'job') ---
modell_poutcome <- glm(y ~ poutcome, data = Daten, family = "binomial")
null_modell <- glm(y ~ 1, data = Daten, family = "binomial")

mcfadden_poutcome <- 1 - as.numeric(logLik(modell_poutcome) / logLik(null_modell))
print(paste("Erklärte Varianz durch poutcome (McFadden):", round(mcfadden_poutcome * 100, 2), "%"))


##########################################
# Analyse der Variable "day" und y
##########################################

# --- 1. Balkendiagramm: Reine Verteilung von 'day' ---
ggplot(Daten, aes(x = day)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Häufigkeit der Kontakte nach Monatstagen",
    x = "Tag des Monats (day)",
    y = "Anzahl der Kontakte"
  ) +
  scale_x_continuous(breaks = 1:31) # Jeden Tag auf der x-Achse einzeln anzeigen

# --- 2. Tabelle: Aufschlüsselung der Tage nach Variable 'y' ---
print("Tabelle: Monatstage aufgeschlüsselt nach Abschluss (y):")
print(table(Daten$day, Daten$y))

# --- 3. Statistische Messung des Einflusses von 'day' ---

# Variante A: Korrelation (für den Zusammenhang metrisch vs. binär)
y_num <- ifelse(Daten$y == "yes", 1, 0)
korrelation_day <- cor(Daten$day, y_num, method = "pearson")

print(paste("Korrelation zwischen Tag und Abschluss (Pearson):", round(korrelation_day, 4)))
print(paste("Erklärte Varianz über Korrelation (R²):", round(korrelation_day^2 * 100, 4), "%"))

# Variante B: Logistische Regression (McFadden Pseudo-R²) als Vergleichswert
modell_day <- glm(y ~ day, data = Daten, family = "binomial")
null_modell <- glm(y ~ 1, data = Daten, family = "binomial")

mcfadden_day <- 1 - as.numeric(logLik(modell_day) / logLik(null_modell))
print(paste("Erklärte Varianz durch den Monatstag (McFadden):", round(mcfadden_day * 100, 4), "%"))





job <- Daten[, "job"]
age <- Daten[, "age"]
marital <- Daten[, "marital"]
y <- Daten[, "y"]

boxplot(age)
ggplot(Daten, aes(x = job)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Häufigkeit der Berufsbezeichnungen",
    x = "Berufstyp",
    y = "Anzahl"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

housemaid <- subset(Daten, job == "unemployed")
unemployed <- subset(Daten, marital == "mousemaid")

length(housemaid)
length(unemployed)

# Zeichnen einer Punktewolke
marital_single <- subset(Daten, marital == "single")
marital_married <- subset(Daten, marital == "married")
marital_divorced <- subset(Daten, marital == "divorced") # =widowed or divorced
marital_unknown <- subset(Daten, marital == "unknown")

par(mfrow = c(1, 4))
boxplot(marital_single["age"], ylim = c(1, 100), main = "singles")
boxplot(marital_married["age"], ylim = c(1, 100), main = "married")
boxplot(marital_divorced["age"], ylim = c(1, 100), main = "divorced")
boxplot(marital_unknown["age"], ylim = c(1, 100), main = "unknown")

summary(marital_divorced)
summary(marital_unknown)
# Setzen von Grafikoptionen

plot(x, y, main = "Groesse und Gewicht", xlim = c(1.5, 2), ylim = c(50, 100), xlab = "Gr??e", ylab = "Gewicht")


## Variablen in Fakoren umwandeln (siehe NotizenZuDenDaten.docx)
# pdays: -1 (oder 999) bedeutet "Kunde wurde vorher nicht kontaktiert".
# Wir überschreiben pdays als Faktor ("not_contacted" vs "contacted"),
# damit dieser Extremwert zukünftige Modelle/Plots nicht verzerrt.
Daten$pdays <- as.factor(ifelse(Daten$pdays == -1 | Daten$pdays == 999, "not_contacted", "contacted"))

## Plots

## Jede Variable als einzelner Plot (in separaten Ansichten)

# 1. age (Numerisch: Hist + Boxplot nebeneinander im selben Fenster)
par(mfrow = c(1, 2))
hist(Daten$age, main = "Hist: age", xlab = "", col = "lightgreen", breaks = 30)
boxplot(Daten$age, main = "Box: age", col = "lightblue", outline = TRUE)

# 2. balance (Numerisch: Hist + Boxplot nebeneinander im selben Fenster)
par(mfrow = c(1, 2))
hist(Daten$balance, main = "Hist: balance", xlab = "", col = "lightgreen", breaks = 30)
boxplot(Daten$balance, main = "Box: balance", col = "lightblue", outline = FALSE)

quantile(Daten$balance, probs = c(0.25, 0.75))

IQR(Daten$balance)

summary(Daten$balance)

# Ab hier kategoriale Variablen (Balkendiagramme, einzeln und im Vollbild)
par(mfrow = c(1, 1), mar = c(8, 3, 2, 1)) # Zurück auf 1x1 Ansicht, erhöhter Rand unten

# 3. job
barplot(table(Daten$job), main = "job", col = "steelblue", las = 2)

# 4. marital
barplot(table(Daten$marital), main = "marital", col = "steelblue", las = 2)

# 5. education
barplot(table(Daten$education), main = "education", col = "steelblue", las = 2)

# 6. default
barplot(table(Daten$default), main = "default", col = "steelblue", las = 1)

# 7. housing
barplot(table(Daten$housing), main = "housing", col = "steelblue", las = 1)

# 8. loan
barplot(table(Daten$loan), main = "loan", col = "steelblue", las = 1)

# Parameter wieder auf R-Standard zurücksetzen
par(mar = c(5, 4, 4, 2) + 0.1)

summary(Daten)
