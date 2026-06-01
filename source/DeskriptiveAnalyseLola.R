setwd("C:/Users/Lollyfee/OneDrive/bank_marketing_ml/data")
Daten <- read.csv("bank-additional-full.csv", header=TRUE, sep=";", fill=TRUE, stringsAsFactors=TRUE)

table(Daten$month)

library(dplyr)

Daten %>%
  summarise(across(c(contact, pdays, poutcome), list(
    na = ~ sum(is.na(.)),
    empty = ~ sum(. == "" | . == " ", na.rm = TRUE),
    unknown = ~ sum(tolower(.) == "unknown", na.rm = TRUE)
  )))

missing_counts <- colSums(is.na(Daten[, c("contact", "pdays", "poutcome")]))
print(missing_counts)

# Einzeln für die Spalten
table(Daten$contact)
table(Daten$poutcome)
library(dplyr)

levels(Daten$contact)
levels(Daten$poutcome)

Daten %>%
  summarise(across(c(contact, pdays, poutcome), ~ sum(is.na(.))))

table(Daten$Pdays)
# Oder gezielt nach der -1 suchen:
sum(Daten$Pdays == -1, na.rm = TRUE)

table(Daten$day_of_week)

# 1. Check für die kategorischen Spalten (contact, poutcome)
table(Daten$contact)
table(Daten$poutcome)

# 2. Check für die numerische Spalte pdays
# In diesem Datensatz bedeutet 999, dass der Kunde vorher nie kontaktiert wurde
sum(Daten$pdays == 999)



# 1. Zielvariable y in 0 und 1 umwandeln
Daten$y_num <- ifelse(Daten$y == "yes", 1, 0)

# 2. poutcome in Zahlen umwandeln (z.B. failure=1, nonexistent=2, success=3)
# R macht das bei Factors automatisch mit as.numeric()
Daten$poutcome_num <- as.numeric(Daten$poutcome)

# 3. Kovarianz berechnen
cov(Daten$poutcome_num, Daten$y_num, use = "complete.obs")

library(dplyr)
library(ggplot2)

Daten %>%
  group_by(poutcome) %>%
  summarise(Erfolgsrate = mean(y == "yes")) %>%
  ggplot(aes(x = poutcome, y = Erfolgsrate)) +
  geom_col(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Einfluss von Poutcome auf den Erfolg")




summary(Daten)
# Erstellt eine Punktewolke für eine einzelne Variable
stripchart(Daten$age, 
           method = "jitter", # 'jitter' streut die Punkte leicht, damit sie nicht überlappen
           vertical = TRUE,   # Punkte vertikal anordnen (wie beim Boxplot)
           main = "Punktewolke des Alters", 
           ylab = "Alter (age)",
           pch = 16,          # Ausgefüllte Kreise als Punkte
           col = "darkblue")


# 1. Häufigkeiten berechnen (zählt, wie oft jeder Monat vorkommt)
monats_haeufigkeiten <- table(Daten$month)

# 2. Das Balkendiagramm zeichnen
barplot(monats_haeufigkeiten, 
        main = "Verteilung nach Monaten", 
        ylab = "Anzahl", 
        col = "lightgreen")

# 3. Spezielle Monate


# Einmalig installieren, falls noch nicht vorhanden: install.packages("dplyr")
library(dplyr)

daten_jan_feb <- Daten %>%
  filter(month %in% c("jan", "dec"))
library(ggplot2)

# Löscht alle Kategorien (Monate), die in den gefilterten Daten nicht mehr vorkommen
daten_jan_feb$month <- droplevels(daten_jan_feb$month)

ggplot(daten_jan_feb, aes(x = month)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Balkendiagramm: Nur Januar und Februar", x = "Monat", y = "Anzahl") +
  theme_minimal()



# Housemaid und unemployed?
library(dplyr)
library(ggplot2)

# 1. Daten filtern: Wir behalten nur die Zeilen mit housemaid oder unemployed
gefilterte_daten <- Daten %>%
  filter(job %in% c("housemaid", "unemployed"))

# 2. Den Boxplot zeichnen
# Wir nutzen denselben gefilterten Datensatz von oben

ggplot(gefilterte_daten, aes(x = age, fill = job)) +
  geom_bar(position = "dodge") + # 'dodge' stellt die Balken nebeneinander statt übereinander
  labs(title = "Anzahl nach Familienstand und Beruf", 
       x = "Alter", 
       y = "Anzahl (Häufigkeit)") +
  theme_minimal()


table(Daten$job)
sum(Daten$job == "housemaid")
sum(Daten$job == "unemployed")
sum(Daten$job == "student")


#Boxplot
library(dplyr)
library(ggplot2)

# 1. Daten filtern: Wir behalten nur die Zeilen mit housemaid oder unemployed
gefilterte_daten <- Daten %>%
  filter(job %in% c("housemaid", "unemployed"))

# 2. Den Boxplot zeichnen
ggplot(gefilterte_daten, aes(x = job, y = age, fill = job)) +
  geom_boxplot() + 
  labs(title = "Altersverteilung: Housemaid vs. Unemployed", 
       x = "Beruf", 
       y = "Alter (age)") +
  theme_minimal()


# Test
# 1. Wir erstellen eine normale Kreuztabelle für ALLE Jobs und den Familienstand
job_marital_tabelle <- table(Daten$job, Daten$marital)

# 2. Wir wandeln die absoluten Zahlen in zeilenweise Prozente um
# Die "1" bedeutet: Berechne die Prozente pro Zeile (pro Job)
prozent_tabelle <- prop.table(job_marital_tabelle, margin = 1) * 100

# 3. Runden für eine schönere Ansicht
round(prozent_tabelle, 1)



# Familienstand je nach Berufsgruppe


library(ggplot2)

ggplot(Daten, aes(x = job, fill = marital)) +
  # position = "fill" macht daraus ein 100%-Diagramm (anstatt absoluter Zahlen)
  geom_bar(position = "fill") +
  
  # coord_flip dreht das Diagramm zur Seite, damit man die Job-Namen besser lesen kann
  coord_flip() + 
  
  labs(title = "Familienstand je nach Berufsgruppe", 
       x = "Beruf", 
       y = "Anteil (in %)",
       fill = "Familienstand") +
  # Die X-Achse (die jetzt unten ist) in Prozent formatieren
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal()

# Alle Boxplots


# Wochentage in die richtige Reihenfolge bringen
Daten$day_of_week <- factor(Daten$day_of_week, 
                            levels = c("mon", "tue", "wed", "thu", "fri"))

library(ggplot2)


# Erfolg nach Kontaktart
ggplot(Daten, aes(x = contact, fill = y)) +
  geom_bar(position = "fill") + # "fill" zeigt den Anteil (Prozent)
  labs(y = "Anteil", title = "Erfolgsrate nach Kontaktart")


# Boxplot: Erfolg vs. Gesprächsdauer
ggplot(Daten, aes(x = y, y = duration, fill = y)) +
  geom_boxplot() +
  scale_y_log10() + # Log-Skala, da duration oft extreme Ausreißer hat
  labs(title = "Gesprächsdauer bei Erfolg vs. Misserfolg")

summary(Daten)
