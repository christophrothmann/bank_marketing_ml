setwd("C:/Users/Lollyfee/OneDrive/bank_marketing_ml/data")
Daten <- read.csv("bank-additional-full.csv", header=TRUE, sep=";", fill=TRUE, stringsAsFactors=TRUE)

table(Daten$month)

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
