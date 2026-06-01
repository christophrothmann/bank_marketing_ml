setwd("C:/Users/Lollyfee/OneDrive/bank_marketing_ml/data")
Daten <- read.csv("bank-full.csv", header=TRUE, sep=";", fill=TRUE, stringsAsFactors=TRUE)

summary(Daten)

table(Daten$month)



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




# 1. Wir erstellen eine normale Kreuztabelle für ALLE Jobs und den Familienstand
job_marital_tabelle <- table(Daten$job, Daten$marital)
prozent_tabelle <- prop.table(job_marital_tabelle, margin = 1) * 100
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


# Fehlende Variablen? 
missing_counts <- colSums(is.na(Daten[, c("contact", "pdays", "poutcome")]))
print(missing_counts)

library(dplyr)

Daten %>%
  summarise(across(c(contact, pdays, poutcome), ~ sum(is.na(.))))

sum(Daten$poutcome == "unknown", na.rm = TRUE)
sum(Daten$poutcome == "success", na.rm = TRUE)
sum(Daten$poutcome == "failure", na.rm = TRUE)
sum(Daten$poutcome == "other", na.rm = TRUE)
# Das zeigt dir ALLE Kategorien und die Anzahl der echten NAs
table(Daten$poutcome, useNA = "always")

# Gesamtzahl der Zeilen im Datensatz
total_rows <- nrow(Daten)

# Deine gezählten Werte (inkl. unknown)
summe_deiner_counts <- sum(table(Daten$poutcome))

# Differenz berechnen
total_rows - summe_deiner_counts


sum(36959 + 1511 + 4901 + 1840)

# Boxplots
# Zeigt alle Kategorien + echte Fehlwerte (NA)
table(Daten$contact, useNA = "always")

library(ggplot2)

ggplot(Daten, aes(x = contact, y = duration, fill = y)) +
  geom_boxplot() +
  scale_y_log10() + # Log-Skala hilft extrem bei der duration!
  theme_minimal() +
  labs(title = "Gesprächsdauer nach Kontaktart und Erfolg",
       x = "Kontaktart", y = "Dauer (log10)")


library(ggplot2)
library(dplyr)

Daten %>%
  mutate(duration_min = duration / 60) %>%
  ggplot(aes(x = as.factor(day), y = duration_min, fill = y)) +
  geom_boxplot(outlier.shape = NA) + 
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal() +
  labs(title = "Gesprächsdauer pro Kalendertag",
       x = "Tag des Monats", 
       y = "Dauer (in Minuten)",
       fill = "Erfolg (y)")
library(ggplot2)
library(dplyr)

# Wir rechnen duration in Minuten um und filtern extreme Ausreißer für die Grafik raus
Daten %>%
  mutate(duration_min = duration / 60) %>%
  ggplot(aes(x = contact, y = duration_min, fill = y)) +
  geom_boxplot(outlier.shape = NA) + # Outlier ausblenden, damit die Boxen groß sind
  coord_cartesian(ylim = c(0, 20)) + # Zoom auf die ersten 20 Minuten
  theme_minimal() +
  labs(title = "Gesprächsdauer in Minuten nach Kontaktart",
       x = "Kontaktart", 
       y = "Dauer (in Minuten)",
       fill = "Erfolg (y)")


library(ggplot2)
library(dplyr)

Daten %>%
  ggplot(aes(x = as.factor(day), fill = y)) +
  geom_bar(position = "fill") + # "fill" skaliert jeden Balken auf 100%
  theme_minimal() +
  labs(title = "Erfolgsrate (y) nach Tag des Monats",
       x = "Kalendertag", 
       y = "Anteil (Prozent)",
       fill = "Erfolg (y)") +
  scale_y_continuous(labels = scales::percent) # Macht %-Zeichen an die Achse


library(ggplot2)
library(dplyr)

Daten$month <- factor(Daten$month, 
                      levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                                 "jul", "aug", "sep", "oct", "nov", "dec"))
Daten %>%
  ggplot(aes(x = as.factor(month), fill = y)) +
  geom_bar(position = "fill") + 
  theme_minimal() +
  labs(title = "Erfolgsrate (y) nach Monat",
       x = "Monat", 
       y = "Anteil (Prozent)",
       fill = "Erfolg (y)") +
  scale_y_continuous(labels = scales::percent)


# Wann die meisten Anrufe?

library(ggplot2)
library(dplyr)

# Erst die Monate sortieren (falls noch nicht geschehen)
Daten$month <- factor(Daten$month, 
                      levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                                 "jul", "aug", "sep", "oct", "nov", "dec"))

# Plot der absoluten Häufigkeit
ggplot(Daten, aes(x = month, fill = y)) +
  geom_bar() + # Ohne position="fill", damit wir die echten Zahlen sehen
  theme_minimal() +
  labs(title = "Anzahl der Anrufe vs. Erfolg je Monat",
       x = "Monat",
       y = "Gesamtanzahl der Anrufe",
       fill = "Erfolg (y)")



# Duration Boxplot
library(ggplot2)
library(dplyr)

Daten %>%
  mutate(duration_min = duration / 60) %>%
  ggplot(aes(x = y, y = duration_min, fill = y)) +
  geom_boxplot(outlier.alpha = 0.2) + # Ausreißer leicht transparent machen
  coord_cartesian(ylim = c(0, 30)) + # Fokus auf die ersten 30 Minuten
  theme_minimal() +
  labs(title = "Einfluss der Gesprächsdauer auf den Erfolg",
       x = "Erfolg (y)", 
       y = "Dauer (in Minuten)")


# Campain Boxplot -> Erfolg vs. Anzahl Kontakte
ggplot(Daten, aes(x = y, y = campaign, fill = y)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 10)) + # Fokus auf 1-10 Kontakte (da es extreme Ausreißer gibt)
  theme_minimal() +
  labs(title = "Anzahl der Kontakte in dieser Kampagne",
       x = "Erfolg (y)", 
       y = "Anzahl der Kontakte (campaign)")


ggplot(Daten, aes(x = duration/60, fill = y)) +
  geom_density(alpha = 0.5) + # alpha macht die Farben transparent
  coord_cartesian(xlim = c(0, 20)) +
  theme_minimal() +
  labs(title = "Dichteverteilung der Gesprächsdauer",
       x = "Dauer in Minuten",
       y = "Dichte")


# 1. Sicherstellen, dass y numerisch ist (falls noch nicht geschehen)
Daten$y_num <- ifelse(Daten$y == "yes", 1, 0)

# 2. Korrelation berechnen (Point-Biserial Correlation)
korrelation <- cor(Daten$campaign, Daten$y_num)
print(paste("Korrelation:", round(korrelation, 4)))

# 3. Logistische Regression, um den Koeffizienten zu erhalten
modell <- glm(y_num ~ campaign, data = Daten, family = binomial)
summary(modell)



# Boxplot für pdays (nur für Kunden mit Vorab-Kontakt)
Daten %>%
  filter(pdays > -1) %>%
  ggplot(aes(x = y, y = pdays, fill = y)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Tage seit letztem Kontakt (nur Bestandskunden)",
       x = "Erfolg (y)", 
       y = "Tage seit letztem Kontakt")
# --> neue Kontakte eher "yes"

# Boxplot für previous
ggplot(Daten, aes(x = y, y = previous, fill = y)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 5)) + # Zoom auf 0-5 Kontakte
  theme_minimal() +
  labs(title = "Anzahl früherer Kontakte",
       x = "Erfolg (y)", 
       y = "Anzahl Kontakte (previous)")





library(dplyr)
library(ggplot2)

# Wir teilen die Kunden in zwei Gruppen: Neu vs. Bekannt
Daten_Vergleich <- Daten %>%
  mutate(Kundentyp = ifelse(previous == 0, "Neukunde", "Bestandskunde")) %>%
  group_by(Kundentyp, campaign) %>%
  summarise(Erfolgsrate = mean(y == "yes") * 100) %>%
  filter(campaign <= 10) # Wir schauen uns nur die ersten 10 Anrufe an

# Der Plot, der alles erklärt
ggplot(Daten_Vergleich, aes(x = campaign, y = Erfolgsrate, color = Kundentyp)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal() +
  labs(title = "Neu- vs. Bestandskunden",
       x = "Anzahl der Anrufe in dieser Kampagne",
       y = "Erfolgsrate (in %)",
       color = "Wer wird angerufen?")

table(Daten$previous)
table(Daten$campain)
table(Daten$pdays)


# Ungleichgewicht omg
library(ggplot2)
library(dplyr)

# 1. Daten vorbereiten
Daten_Status <- Daten %>%
  mutate(Status = ifelse(previous == 0, "Neukunde (0 Kontakte)", "Bestandskunde (>0)"))

# 2. Plot erstellen
ggplot(Daten_Status, aes(x = Status, fill = Status)) +
  geom_bar() +
  # Ergänzt die exakten Zahlen oben auf den 
  geom_text(stat='count', aes(label=..count..), vjust=-0.5, size=5) + 
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "lightgrey")) +
  labs(title = "Das Ungleichgewicht im Datensatz",
       subtitle = "Über 80% haben keine Historie",
       x = "Status vor der aktuellen Kampagne",
       y = "Anzahl der Datensätze")


# Wir schauen uns nur die Leute an, die MINDESTENS 1 Kontakt hatten
Daten %>%
  filter(previous > 0) %>%
  ggplot(aes(x = previous)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  # Log-Skala auf der Y-Achse hilft, die kleinen Balken hinten sichtbar zu machen
  scale_y_log10() + 
  theme_minimal() +
  labs(title = "Verteilung der Bestandskunden-Historie",
       x = "Anzahl früherer Kontakte (previous)",
       y = "Häufigkeit"
)

table(Daten$previous)
# Boxplot für poutcome

library(ggplot2)
library(dplyr)

# Plot: Was ist aus den Leuten geworden, die beim letzten Mal Erfolg hatten?
Daten %>%
  ggplot(aes(x = poutcome, fill = y)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("red", "#377EB8")) + # Rot für No, Blau für Yes
  labs(title = "Einfluss des letzten Kampagnen-Ergebnisses",
       x = "Ergebnis der letzten Kampagne",
       y = "Anteil in Prozent",
       fill = "Erfolg (y)")

Daten %>%
  group_by(poutcome) %>%
  summarise(
    Anzahl = n(),
    Erfolgsquote = round(mean(y == "yes") * 100, 2)
  )


# 1. Zielvariable y in 0 und 1 umwandeln
Daten$y_num <- ifelse(Daten$y == "yes", 1, 0)

# 2. Kovarianz berechnen (z.B. für die Variable 'campaign')
# Zeigt die Richtung des Zusammenhangs, hängt aber stark von der Skala ab.
kovarianz <- cov(Daten$campaign, Daten$y_num)
print(paste("Kovarianz:", kovarianz))

# 3. Korrelation berechnen (BESSER für den Vergleich des Einflusses)
# -1 = starker negativer Einfluss, 0 = kein Einfluss, 1 = starker positiver Einfluss
korrelation <- cor(Daten$campaign, Daten$y_num)
print(paste("Korrelation:", korrelation))


ggplot(Daten, aes(x = job, fill = y)) +
  geom_bar(position = "fill") + 
  coord_flip() + # Dreht es, damit man die Jobs lesen kann
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Wer schließt Verträge ab?",
       subtitle = "Erfolgsquote nach Berufsgruppe",
       x = "Beruf", y = "Anteil in %", fill = "Erfolg")


# Boxplot für y Target
table(Daten$y)


library(ggplot2)


library(ggplot2)
library(dplyr)

Daten %>%
  ggplot(aes(x = as.character(month), fill = y)) +
  geom_bar(position = "fill") + # Macht die Balken alle gleich hoch auf 100%
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) + # Prozentzeichen an der Achse
  labs(title = "Erfolgsrate nach Monaten (Alphabetisch)",
       x = "Monat", 
       y = "Anteil (Prozent)",
       fill = "Erfolg (y)")




library(ggplot2)
library(dplyr)

Daten %>%
  ggplot(aes(x = as.character(month), fill = y)) +
  geom_bar() + # Zeigt die echten, nackten Zahlen
  theme_minimal() +
  labs(title = "Anzahl der Anrufe nach Monaten (Alphabetisch)",
       x = "Monat", 
       y = "Gesamtanzahl der Anrufe",
       fill = "Erfolg (y)")




library(ggplot2)
library(dplyr)

# 1. Monate explizit von Jan bis Dez als Reihenfolge festlegen
Daten$month <- factor(Daten$month, 
                      levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                                 "jul", "aug", "sep", "oct", "nov", "dec"))

# 2. Das 100%-Balkendiagramm zeichnen
Daten %>%
  filter(!is.na(month)) %>% 
  ggplot(aes(x = month, fill = y)) +
  geom_bar(position = "fill") + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Erfolgsrate nach Monaten (Chronologisch)",
       x = "Monat", 
       y = "Anteil (Prozent)",
       fill = "Erfolg (y)")


library(ggplot2)
library(dplyr)

# 1. Monate in die richtige Reihenfolge bringen
Daten$month <- factor(Daten$month, 
                      levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                                 "jul", "aug", "sep", "oct", "nov", "dec"))

# 2. Einfaches Balkendiagramm ohne 'fill'
Daten %>%
  filter(!is.na(month)) %>%
  ggplot(aes(x = month)) +
  geom_bar(fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Reine Verteilung der Anrufe über das Jahr",
       subtitle = "Gesamtzahl der Kontakte pro Monat (ohne Erfolgskontrolle)",
       x = "Monat", 
       y = "Anzahl der Anrufe")

table(Daten$job)



library(dplyr)
library(ggplot2)

# 1. Daten filtern: Wir holen uns nur die Zeilen mit 'housemaid' und 'unemployed'
gefilterte_daten <- Daten %>%
  filter(job %in% c("housemaid", "unemployed"))

# 2. Das gruppierte Balkendiagramm zeichnen
ggplot(gefilterte_daten, aes(x = marital, fill = job)) +
  geom_bar(position = "dodge") + # 'dodge' stellt die Balken paarweise nebeneinander
  theme_minimal() +
  scale_fill_manual(values = c("housemaid" = "#F8766D", "unemployed" = "#00BFC4")) + # Nutzt die Standard-ggplot-Farben aus deinem Bild
  labs(title = "Anzahl nach Familienstand und Beruf",
       x = "Familienstand (marital)",
       y = "Anzahl (Häufigkeit)",
       fill = "job")




library(dplyr)
library(ggplot2)

# 1. Erfolgsrate pro Job berechnen
Job_Erfolg <- Daten %>%
  mutate(y_num = ifelse(y == "yes", 1, 0)) %>% 
  group_by(job) %>%
  summarise(Erfolgsrate = mean(y_num) * 100)

# 2. Plot zeichnen (mit reorder() automatisch sortiert)
ggplot(Job_Erfolg, aes(x = reorder(job, Erfolgsrate), y = Erfolgsrate, fill = job)) +
  geom_col() +
  coord_flip() + # Dreht das Diagramm für perfekte Lesbarkeit
  theme_minimal() +
  theme(legend.position = "none") + # Legende wird gelöscht, da Berufe schon links stehen
  labs(title = "Wer schließt am ehesten ab?",
       subtitle = "Erfolgsrate in % je nach Berufsgruppe",
       x = "Berufsgruppe",
       y = "Erfolgsrate (in %)")




Daten %>%
  mutate(y_num = ifelse(y == "yes", 1, 0)) %>%
  group_by(job) %>%
  summarise(
    Gesamtanzahl = n(),
    Erfolgsquote_Prozent = round(mean(y_num) * 100, 2)
  ) %>%
  arrange(desc(Erfolgsquote_Prozent)) # Höchste Quote ganz oben


# Kreuztabelle erstellen und Test rechnen
tabelle <- table(Daten$job, Daten$y)
chi_test <- chisq.test(tabelle)

print(chi_test)


library(dplyr)
library(ggplot2)

# Wir nutzen position = "fill" für die 100%-Darstellung wie im Screenshot
ggplot(Daten, aes(x = job, fill = y)) +
  geom_bar(position = "fill") + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  # Die exakten Farben aus deinem Screenshot (Lachsrot und Türkis)
  scale_fill_manual(values = c("no" = "#F8766D", "yes" = "#00BFC4")) + 
  labs(title = "Erfolgsrate nach Berufsgruppe",
       x = "Beruf (job)", 
       y = "Anteil (Prozent)",
       fill = "Erfolg (y)") +
  # Wichtig: Wir drehen die Achsen, damit die vielen Jobnamen perfekt lesbar sind!
  coord_flip()



# Falls du das Paket noch nicht hast, einmal installieren:
# install.packages("vcd")

library(vcd)

# Kreuztabelle erstellen
tabelle_poutcome <- table(Daten$poutcome, Daten$y)

# Cramer's V berechnen
cramers_v <- assocstats(tabelle_poutcome)$cramer
print(paste("Cramer's V für poutcome:", round(cramers_v, 4)))
chi_poutcome <- chisq.test(tabelle_poutcome)
print(chi_poutcome)

library(ggplot2)

ggplot(Daten, aes(x = y, y = duration / 60, fill = y)) + # Hier durch 60 teilen
  geom_boxplot() +
  scale_y_log10() + # Logarithmische Skala bleibt aktiv
  scale_fill_manual(values = c("no" = "#F8766D", "yes" = "#00BFC4")) +
  theme_minimal() + 
  labs(title = "Gesprächsdauer bei Erfolg vs. Misserfolg",
       x = "Erfolg (y)",
       y = "Gesprächsdauer (in Minuten)", # Beschriftung angepasst
       fill = "Erfolg (y)")



# 1. Check für balance (Wie viel Geld haben die Gewinner?)
ggplot(Daten, aes(x = y, y = balance, fill = y)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1000, 5000)) + # Zoom, da es extreme Bonzen-Ausreißer gibt
  theme_minimal() +
  labs(title = "Kontostand bei Erfolg vs. Misserfolg", y = "Kontostand in Euro")

# 2. Check für loan (Senkt ein Privatkredit die Chancen?)
ggplot(Daten, aes(x = loan, fill = y)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Erfolgsrate bei bestehendem Privatkredit", y = "Anteil in %")
