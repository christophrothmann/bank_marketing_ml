setwd("C:/Users/Lollyfee/OneDrive/bank_marketing_ml/data")
Daten <- read.csv("bank-full.csv", header=TRUE, sep=";", fill=TRUE, stringsAsFactors=TRUE)

summary(Daten)


# Duration aus Datensatz kicken
Daten$duration <- NULL
Daten$duration_min <- NULL
names(Daten)


## Day Analyse
Daten$day <- factor(Daten$day, levels = 1:31)
table(Daten$day)

library(ggplot2)

# Einfaches Balkendiagramm für das Volumen je Tag
ggplot(Daten, aes(x = day)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Verteilung der Kontakte nach Monatstag",
       x = "Tag des Monats",
       y = "Absolute Anzahl der Kontakte")

# Gestapeltes, normiertes Balkendiagramm für die Erfolgsquote
ggplot(Daten, aes(x = day, fill = y)) +
  geom_bar(position = "fill") + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Erfolgsquote (y) je Monatstag",
       subtitle = "Normiert auf 100%",
       x = "Tag des Monats",
       y = "Prozentualer Anteil",
       fill = "Erfolgreich (y)")



## Month Analyse
table(Daten$month)
# Monate als Factor mit allen 12 Monaten in der richtigen Reihenfolge definieren
Daten$month <- factor(Daten$month, 
                      levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

# Kurzer Check in der Konsole, ob die Reihenfolge stimmt
table(Daten$month)
library(ggplot2)
table(Daten$month)
# Diagramm 1: Absolute Kontakte (gestapelt)
ggplot(Daten, aes(x = month, fill = y)) +
  geom_bar(position = "stack") + 
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Absolute Kontakte und Erfolge nach Monat",
       subtitle = "Nicht normiert – zeigt das reine Anrufvolumen pro Monat",
       x = "Monat",
       y = "Absolute Anzahl der Kontakte",
       fill = "Erfolgreich (y)")
# Diagramm 2: Erfolgsquote pro Monat (normiert)
ggplot(Daten, aes(x = month, fill = y)) +
  geom_bar(position = "fill") + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Erfolgsquote (y) je Monat",
       subtitle = "Normiert auf 100% – zeigt die relative Abschlussrate",
       x = "Monat",
       y = "Prozentualer Anteil",
       fill = "Erfolgreich (y)")


library(ggplot2)

# 1. Wir berechnen die durchschnittliche Erfolgsquote des kompletten Datensatzes (unsere Baseline)
baseline_prozent <- (sum(Daten$y == "yes") / nrow(Daten)) * 100

# 2. Kreuztabelle erstellen und die genauen Prozentsätze (Quoten) pro Monat ausrechnen
tabelle_monat <- table(Daten$month, Daten$y)
quoten_monat <- prop.table(tabelle_monat, margin = 1) # margin = 1 rechnet zeilenweise auf 100%

# 3. Daten für das Diagramm vorbereiten
quoten_df <- as.data.frame(quoten_monat)
colnames(quoten_df) <- c("month", "y", "Rate")

# Wir filtern nur die Zeilen, in denen y = "yes" ist, und nehmen die Rate mal 100
yes_quoten <- subset(quoten_df, y == "yes")
yes_quoten$Rate <- yes_quoten$Rate * 100

# 4. Das Deep-Dive Diagramm (absteigend sortiert!)
ggplot(yes_quoten, aes(x = reorder(month, -Rate), y = Rate)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") + # Stat="identity" nimmt genau unsere berechneten Werte
  geom_hline(yintercept = baseline_prozent, color = "lightcoral", linetype = "dashed", size = 1.2) +
  theme_minimal() +
  labs(title = "Deep-Dive: Exakte Erfolgsquote (yes) pro Monat",
       subtitle = "Rote gestrichelte Linie = Gesamtdurchschnitt (Baseline)",
       x = "Monat (absteigend nach Erfolg sortiert)",
       y = "Erfolgsquote in %")

















## Duration Analyse
table(Daten$duration)
summary(Daten)

library(ggplot2)

# Boxplot: Anrufdauer vs. Zielvariable (y)
ggplot(Daten, aes(x = y, y = duration, fill = y)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Einfluss der Anrufdauer auf den Abschluss",
       subtitle = "Erfolgreiche Anrufe (yes) dauern tendenziell deutlich länger",
       x = "Erfolgreich (y)",
       y = "Dauer in Sekunden",
       fill = "Erfolgreich (y)")


library(ggplot2)

# Dichte-Plot der Anrufdauer
ggplot(Daten, aes(x = duration, fill = y)) +
  geom_density(alpha = 0.6) + # alpha macht die Farben leicht transparent, damit man Überlappungen sieht
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  coord_cartesian(xlim = c(0, 1500)) + # Zoom auf die ersten 25 Minuten
  labs(title = "Verteilung der Anrufdauer",
       subtitle = "Erfolgreiche Anrufe (grün) verschieben sich deutlich nach rechts (länger)",
       x = "Dauer in Sekunden",
       y = "Dichte",
       fill = "Erfolgreich (y)")


# 1. Neue Spalte erstellen: Dauer in Minuten (abgerundet)
Daten$duration_min <- floor(Daten$duration / 60)

# 2. Wir filtern für den Plot nur Gespräche unter 30 Minuten, danach wird die Datenlage extrem dünn
ggplot(subset(Daten, duration_min <= 30), aes(x = factor(duration_min), fill = y)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Erfolgsquote nach Gesprächsminute",
       x = "Länge des Gesprächs (in ganzen Minuten)",
       y = "Prozentualer Anteil",
       fill = "Erfolgreich (y)")


# Chi-Quadrat-Test auf Unabhängigkeit
chisq.test(table(Daten$month, Daten$y))

# Campaign Analyse

table(Daten$campaign)
summary(Daten)


library(ggplot2)

# Absolute Verteilung der Kampagnen-Anrufe (gefiltert bis max. 15)
ggplot(subset(Daten, campaign <= 15), aes(x = factor(campaign), fill = y)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Anzahl der Kontakte pro Kunde (aktuelle Kampagne)",
       subtitle = "Die allermeisten Kunden werden 1 bis 3 Mal angerufen",
       x = "Anzahl der Kontaktversuche (campaign)",
       y = "Absolute Anzahl",
       fill = "Erfolgreich (y)")



# Normierte Erfolgsquote (gefiltert bis max. 15)
ggplot(subset(Daten, campaign <= 15), aes(x = factor(campaign), fill = y)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Erfolgsquote nach Anzahl der Kontaktversuche",
       subtitle = "Normiert auf 100% - zeigt, wie die Erfolgschance abnimmt",
       x = "Anzahl der Kontaktversuche (campaign)",
       y = "Prozentualer Anteil",
       fill = "Erfolgreich (y)")



# pdays Analyse
table(Daten$pdays)

library(ggplot2)

# 1. Hilfsspalte erstellen: Unterscheidung zwischen Neu- und Bestandskontakten
Daten$kontaktart <- ifelse(Daten$pdays == -1, "Neu", "Bekannt")

# 2. Gestapeltes, normiertes Balkendiagramm
ggplot(Daten, aes(x = kontaktart, fill = y)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Erfolgsquote: Neu vs. Bekannt",
       x = "Art des Kontakts",
       y = "Prozentualer Anteil",
       fill = "Erfolgreich (y)")


# Histogramm für die bekannten Kund:innen (gefiltert: alles außer -1)
ggplot(subset(Daten, pdays != -1), aes(x = pdays, fill = y)) +
  geom_histogram(binwidth = 30, position = "stack", color = "white") + # color="white" macht feine Ränder um die Blöcke
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Wann werden Kunden wieder angerufen?",
       subtitle = "Verteilung der vergangenen Tage (in ca. 30-Tage Blöcken)",
       x = "Tage seit dem letzten Kontakt (pdays)",
       y = "Absolute Anzahl",
       fill = "Erfolgreich (y)")



# Previous Analyse
library(ggplot2)

# Absolute Verteilung der bisherigen Kontakte (gefiltert bis max. 7)
ggplot(subset(Daten, previous <= 7), aes(x = factor(previous), fill = y)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Anzahl der Kontakte VOR dieser Kampagne",
       subtitle = "Die meisten Kunden (0) wurden noch nie zuvor angerufen",
       x = "Bisherige Kontakte (previous)",
       y = "Absolute Anzahl",
       fill = "Erfolgreich (y)")

# Normierte Erfolgsquote nach bisherigen Kontakten
ggplot(subset(Daten, previous <= 7), aes(x = factor(previous), fill = y)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Erfolgsquote nach bisherigen Kontakten",
       subtitle = "Kunden mit Vorgeschichte (1+) schließen deutlich häufiger ab",
       x = "Bisherige Kontakte (previous)",
       y = "Prozentualer Anteil",
       fill = "Erfolgreich (y)")


# Boxplot gefiltert: Wir schauen uns nur Kunden an, die wir schon kannten (> 0)
ggplot(subset(Daten, previous > 0), aes(x = y, y = previous, fill = y)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Boxplot: Bisherige Kontakte (Nur Warmakquise)",
       subtitle = "Ohne die Nullen wird die echte Verteilung bei Bestandskunden sichtbar",
       x = "Erfolgreich (y)",
       y = "Anzahl bisheriger Kontakte (previous)",
       fill = "Erfolgreich (y)")



library(ggplot2)

# Boxplot mit optischem Zoom auf die relevanten Werte (1 bis 15)
ggplot(subset(Daten, previous > 0), aes(x = y, y = previous, fill = y)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  coord_cartesian(ylim = c(1, 15)) + # Die Lupe: Wir schneiden die Y-Achse optisch bei 15 ab
  labs(title = "Gezoomter Boxplot: Bisherige Kontakte",
       subtitle = "Ausreißer über 15 ausgeblendet für lesbare Quartile",
       x = "Erfolgreich (y)",
       y = "Anzahl bisheriger Kontakte",
       fill = "Erfolgreich (y)")






# Wir filtern den Datensatz nach Extremfällen (z.B. mehr als 100 Kontakte)
extreme_ausreisser <- subset(Daten, previous > 100)

# Wir lassen uns nur ein paar spannende Spalten anzeigen, um den Typen zu analysieren
extreme_ausreisser[, c("age", "job", "marital", "duration", "campaign", "previous", "poutcome", "y")]





# wir gehen auf jagd
library(ggplot2)
library(scales) # Wichtig, damit die Zahlen auf der Achse nicht wissenschaftlich (1e+04) angezeigt werden

# 1. Wir bauen uns unsere eigenen Schubladen (Bins)
Daten$previous_cluster <- cut(Daten$previous,
                              breaks = c(-1, 0, 10, 50, 100, 200, Inf),
                              labels = c("0", 
                                         "1-10", 
                                         "11-50", 
                                         "51-100", 
                                         "101-200", 
                                         "> 200"),
                              right = TRUE) # right=TRUE heißt: bis einschließlich der Zahl

# 2. Der knallharte Fakten-Check in der Konsole
print("Exakte Verteilung der Cluster:")
table(Daten$previous_cluster)

# 3. Die Visualisierung mit der geheimen Log-Skala-Waffe
ggplot(Daten, aes(x = previous_cluster)) +
  geom_bar(fill = "lightcoral", color = "black") + 
  theme_minimal() +
  # Hier passiert die Magie: Y-Achse logarithmisch machen!
  scale_y_log10(labels = comma) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Wie oft wurde Kund:in kontaktiert?",
      # subtitle = "ACHTUNG Logarithmische Skala: Jeder Hauptstrich verzehnfacht den Wert!",
       x = "Kontakt-Gruppen (previous)",
       y = "Absolute Anzahl (Log-Skala)")


# Zählt alle Kunden, die mehr als 50 Mal kontaktiert wurden
anzahl_hardcore_faelle <- sum(Daten$previous > 50)

print(anzahl_hardcore_faelle)

# 1. Wir filtern alle Kunden über 50 raus und speichern sie als neues, kleines Paket
hardcore_kunden <- subset(Daten, previous > 50)

# 2. Wir lassen uns ausgeben, wie viele Zeilen (Personen) in diesem Paket stecken
nrow(hardcore_kunden) 

# (Optional) Lass dir die ersten paar Spalten von genau diesen Leuten anzeigen:
# head(hardcore_kunden[, c("age", "job", "previous", "y")])


# poutcome Analyse
table(Daten$poutcome)

library(ggplot2)

# Absolute Verteilung des Vorkampagnen-Ergebnisses
ggplot(Daten, aes(x = poutcome, fill = y)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Absolute Verteilung: Ergebnis der Vorkampagne (poutcome)",
       subtitle = "Der Großteil der Kunden hat keine Vorgeschichte ('nonexistent' / 'unknown')",
       x = "Ergebnis der letzten Kampagne",
       y = "Absolute Anzahl",
       fill = "Erfolgreich (y)")



# Normierte Erfolgsquote nach Vorkampagnen-Ergebnis
ggplot(Daten, aes(x = poutcome, fill = y)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Erfolgsquote nach Vorkampagnen-Ergebnis",
       x = "Ergebnis der letzten Kampagne",
       y = "Prozentualer Anteil",
       fill = "Erfolgreich (y)")



# Riesen ding
# 1. Alle vier Kategorien sauber trennen
Daten$poutcome_success <- ifelse(Daten$poutcome == "success", 1, 0)
Daten$poutcome_failure <- ifelse(Daten$poutcome == "failure", 1, 0)
Daten$poutcome_other   <- ifelse(Daten$poutcome == "other", 1, 0)
Daten$poutcome_unknown <- ifelse(Daten$poutcome == "unknown", 1, 0)

# 2. Genau diese vier Spalten + Zielvariable für die Matrix auswählen
ausgewaehlte_variablen_neu <- c("poutcome_success", "poutcome_failure", "poutcome_other", "poutcome_unknown", "y_num")

# 3. Dataframe und Korrelationsmatrix erstellen
daten_fuer_matrix_neu <- Daten[, ausgewaehlte_variablen_neu]
korrelations_matrix_neu <- cor(daten_fuer_matrix_neu, use = "complete.obs")

# 4. Die Heatmap komplett und sauber
library(ggcorrplot)

ggcorrplot(korrelations_matrix_neu, 
           method = "square", 
           type = "lower",     
           lab = TRUE,         
           lab_size = 4,       
           colors = c("lightcoral", "white", "lightgreen"), 
           title = "Deep-Dive: Alle 4 Ergebnisse der letzten Kampagne",
           legend.title = "Pearson\nKorrelation")



# y Analyse
table(Daten$y)
# Absolute Zahlen
absolute_zahlen <- table(Daten$y)
print("Absolute Verteilung:")
print(absolute_zahlen)

# Prozentuale Anteile (mit 100 multipliziert für lesbare Prozente)
prozente <- prop.table(absolute_zahlen) * 100
print("Prozentuale Verteilung:")
print(round(prozente, 2))


library(ggplot2)

ggplot(Daten, aes(x = y, fill = y)) +
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  # Wir schalten die Legende hier aus, weil die x-Achse schon beschriftet ist
  theme(legend.position = "none") + 
  labs(title = "Verteilung der Zielvariable (y)",
       x = "Hat der Kunde abgeschlossen?",
       y = "Absolute Anzahl")







# Korrelation
# 1. Sicherstellen, dass unsere Zielvariable y als Zahl vorliegt (0 und 1)
Daten$y_num <- ifelse(Daten$y == "yes", 1, 0)

# 2. Wir definieren genau die Spalten, die du in der Matrix sehen willst
# (month und poutcome lassen wir weg, da sie kategorisch sind)
ausgewaehlte_variablen <- c("day","duration", "campaign", "pdays", "previous", "y_num")

# 3. Wir erstellen ein neues, kleines Dataframe nur mit diesen Spalten
daten_fuer_matrix <- Daten[, ausgewaehlte_variablen]

# 4. Korrelationsmatrix berechnen
korrelations_matrix <- cor(daten_fuer_matrix, use = "complete.obs")

# 5. Die Heatmap (fetzig und im Kollegenschema!)
library(ggcorrplot)

ggcorrplot(korrelations_matrix, 
           method = "square", 
           type = "lower",     
           lab = TRUE,         
           lab_size = 4,       # Etwas größer, da wir jetzt weniger Kacheln haben
           colors = c("lightcoral", "white", "lightgreen"), 
           title = "Feature-Korrelation: Kampagnen-Daten & Zielvariable",
           legend.title = "Pearson\nKorrelation")







# Random ToDos aus den Notizen
library(ggplot2)

# 1. Baseline berechnen (falls du sie in diesem Skript noch nicht hast)
baseline_prozent <- (sum(Daten$y == "yes") / nrow(Daten)) * 100

# 2. Quoten für die Jobs berechnen
tabelle_job <- table(Daten$job, Daten$y)
quoten_job <- prop.table(tabelle_job, margin = 1) 

# 3. Datenrahmen (Dataframe) für ggplot vorbereiten
quoten_job_df <- as.data.frame(quoten_job)
colnames(quoten_job_df) <- c("job", "y", "Rate")

# Nur die "yes" Erfolge herausfiltern und in echte Prozente umwandeln
yes_quoten_job <- subset(quoten_job_df, y == "yes")
yes_quoten_job$Rate <- yes_quoten_job$Rate * 100

# 4. Der "Money Graph" für dein Todo
ggplot(yes_quoten_job, aes(x = reorder(job, -Rate), y = Rate)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  geom_hline(yintercept = baseline_prozent, color = "lightcoral", linetype = "dashed", size = 1.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Macht die Schrift schräg für bessere Lesbarkeit
  labs(title = "Einfluss des Berufs auf den Abschluss (y)",
       subtitle = "Rote gestrichelte Linie = Gesamtdurchschnitt (Baseline)",
       x = "Beruf (absteigend nach Erfolg sortiert)",
       y = "Erfolgsquote in %")





# Wir filtern uns nur die Daten für März und Mai heraus
maerz_vs_mai <- subset(Daten, month %in% c("mar", "may"))

# Wir bauen eine Kreuztabelle: Monat vs. Ergebnis der vorherigen Kampagne
tafel_vergleich <- table(maerz_vs_mai$month, maerz_vs_mai$poutcome)

# Wir wandeln das in Zeilen-Prozente um, damit wir es fair vergleichen können
round(prop.table(tafel_vergleich, margin = 1) * 100, 1)


library(ggplot2)

# 1. Wir definieren unsere maßgeschneiderte Farbpalette
# Wichtig: Die Namen müssen exakt so heißen wie in der Legende!
poutcome_farben <- c(
  "success" = "lightgreen",
  "failure" = "lightcoral",
  "unknown" = "gray85",      # Ein helles, unaufdringliches Grau
  "other"   = "lightblue"    # Ein neutrales Blau zur Unterscheidung
)

# 2. Der Plot mit dem Farb-Upgrade
ggplot(maerz_vs_mai, aes(x = month, fill = poutcome)) +
  geom_bar(position = "fill", color = "black") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  # Hier klinken wir unsere eigenen Farben ein:
  scale_fill_manual(values = poutcome_farben) + 
  labs(title = "Warum der März gewinnt: Die Kundenstruktur",
       x = "Monat",
       y = "Anteil der Kontaktarten in %",
       fill = "Letztes Kampagnenergebnis (poutcome)")




library(ggplot2)

# 1. Profi-Move: Wir bringen die Monate in die richtige chronologische Reihenfolge
Daten$month <- factor(Daten$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

# 2. Unsere Signalfarben-Palette
poutcome_farben <- c(
  "success" = "lightgreen",
  "failure" = "lightcoral",
  "unknown" = "gray85",      # Die graue Masse der Kaltakquise
  "other"   = "lightblue"    
)

# 3. Der Plot für das komplette Jahr
ggplot(Daten, aes(x = month, fill = poutcome)) +
  geom_bar(position = "fill", color = "black") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = poutcome_farben) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Schrift leicht schräg, sieht edler aus
  labs(title = "Kundenstruktur über das gesamte Jahr",
       subtitle = "Der graue Block zeigt schonungslos, in welchen Monaten Kaltakquise betrieben wurde",
       x = "Monat",
       y = "Anteil der Kontaktarten in %",
       fill = "Letztes Kampagnenergebnis")





# Genauerer Check wg. Monat
# Einmalig installieren, falls noch nicht passiert: 
# install.packages("DescTools")
library(DescTools)

# Wir berechnen Cramérs V für Monat und Abschluss (y)
einfluss_monat <- CramerV(Daten$month, Daten$y)

print(paste("Die Stärke des Einflusses (Cramérs V) liegt bei:", round(einfluss_monat, 3)))


# Wir bauen ein schnelles Basis-Modell, das y vorhersagen soll
# y_num ist deine numerische Zielvariable (0 für no, 1 für yes)
basis_modell <- glm(y_num ~ month + poutcome, data = Daten, family = "binomial")

# Wir schauen uns die Zusammenfassung an
summary(basis_modell)


# Alter und previous
korrelation <- cor.test(Daten$age, Daten$previous, method = "spearman")
print(korrelation)

library(ggplot2)

ggplot(Daten, aes(x = age, y = previous)) +
  # alpha = 0.3 macht die Punkte leicht durchsichtig, so sehen wir, wo sie sich ballen
  geom_jitter(alpha = 0.3, color = "steelblue", width = 0.2, height = 0.2) +
  # Die magische rote Trendlinie
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  theme_minimal() +
  coord_cartesian(ylim = c(0, 50)) + # Optischer Zoom
  labs(title = "Hypothesen-Check: Werden Senioren häufiger kontaktiert?",
       subtitle = "Wenn die These stimmt, muss die rote Linie deutlich nach oben zeigen",
       x = "Alter der Kunden",
       y = "Bisherige Kontakte (previous)")

# Welche Altersgruppe wird kontaktiert? 
Daten$altersgruppe <- cut(Daten$age,
                          breaks = c(17, 30, 50, 70, 120),
                          labels = c("18-30 (Junge Erwachsene)", 
                                     "31-50 (Mittelalter)", 
                                     "51-70 (Ältere Erwachsene)", 
                                     "71+ (Senioren)"))

# Ein schneller Check in der Konsole: Wer hat den höchsten Durchschnitt an Vor-Kontakten?
aggregate(previous ~ altersgruppe, data = Daten, FUN = mean)


library(ggplot2)

ggplot(Daten, aes(x = altersgruppe, fill = altersgruppe)) +
  geom_bar(color = "black") +
  theme_minimal() +
  # Eine ruhige Farbpalette, sieht sehr professionell aus
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(title = "Zielgruppen-Check: Wer wird eigentlich angerufen?",
       subtitle = "Die absolute Masse der Callcenter-Kapazitäten fließt in die mittlere Altersgruppe",
       x = "Altersgruppe",
       y = "Absolute Anzahl der Kontaktierten",
       fill = "Altersgruppe")


# Previous und pday Vergleich
konsistenz_check <- table(Previous_Null = Daten$previous == 0, 
                          Pdays_MinusEins = Daten$pdays == -1)

# Saubere Ausgabe in der Konsole
print(konsistenz_check)


# Wieso werden Senioren doppelt so oft kontaktiert? 
tabelle_erfolg <- table(Daten$altersgruppe, Daten$poutcome)
prozent_erfolg <- prop.table(tabelle_erfolg, margin = 1) * 100
round(prozent_erfolg, 2)

library(ggplot2)

ggplot(Daten, aes(x = altersgruppe, fill = poutcome)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  # Unsere bekannte Farbpalette von vorhin
  scale_fill_manual(values = c("success" = "lightgreen", 
                               "failure" = "lightcoral", 
                               "unknown" = "gray85", 
                               "other" = "lightblue")) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(title = "Historischer Erfolg nach Altersgruppen",
       subtitle = "Sind Senioren im System, weil sie treue Käufer sind?",
       x = "Altersgruppe",
       y = "Anteil der Ergebnisse in %",
       fill = "Letztes Ergebnis (poutcome)")

table(Daten$altersgruppe)


library(DescTools)

# Einfluss der Altersgruppen auf den Erfolg (y)
cramer_alter <- CramerV(Daten$altersgruppe, Daten$y)
print(paste("Cramérs V für die Altersgruppen:", round(cramer_alter, 3)))

# Ein schnelles Modell nur mit den Altersgruppen
modell_alter <- glm(y_num ~ altersgruppe, data = Daten, family = "binomial")
summary(modell_alter)


table(Daten$y_num)
table(Daten$y)


# Noch mal Alter Analyse
# Wir bauen eine Ja/Nein-Variable für die Kontakthistorie
Daten$schon_mal_kontaktiert <- ifelse(Daten$previous > 0, "Ja (Bestandskunde)", "Nein (Kaltakquise)")

# Jetzt berechnen wir die echte Abschlussquote fuer jede Kombi
erfolgsquote_kombi <- aggregate(y_num ~ altersgruppe + schon_mal_kontaktiert, 
                                data = Daten, 
                                FUN = mean)

# Ausgabe in Prozent für die Übersicht
erfolgsquote_kombi$y_num <- round(erfolgsquote_kombi$y_num * 100, 2)
print(erfolgsquote_kombi)


# Das kontrollierte Modell
modell_kontrolliert <- glm(y_num ~ altersgruppe + previous, data = Daten, family = "binomial")
summary(modell_kontrolliert)

# 95 Jährige Analyse
Daten[Daten$age == 95, ]
# Öffnet die 95-jährigen Kunden in einer eigenen interaktiven Tabelle
View(Daten[Daten$age == 95, ])


# Zählt alle Personen, die älter als 85 Jahre alt sind
sum(Daten$age > 85)
# Zeigt dir eine genaue Übersicht aller Altersstufen über 85
table(Daten$age[Daten$age > 85])

sum(Daten$age > 95)


Daten[Daten$age >= 85, ]
# Öffnet die 95-jährigen Kunden in einer eigenen interaktiven Tabelle
View(Daten[Daten$age >= 85, ])


# Balance Check 
table(Daten$balance)
summary(Daten)

library(ggplot2)

ggplot(Daten, aes(x = y, y = balance, fill = y)) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_cartesian(ylim = c(-1000, 10000)) + # Ausreißer-Zoom
  theme_minimal() +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  labs(title = "Budget-Check",
       x = "Erfolgreicher Abschluss (y)",
       y = "Kontostand in €")

# Der exakte Einfluss des Kontostands auf die Zielvariable y
modell_budget <- glm(y_num ~ balance, data = Daten, family = "binomial")
summary(modell_budget)

# Der exakte Befehl für die Chance-Steigerung pro 1.000 € Guthaben
exp(coef(modell_budget)["balance"] * 1000)


# Den reichsten Kunden unter die Lupe nehmen
Daten[Daten$balance == max(Daten$balance), c("age", "job", "marital", "education", "balance")]

# Sortiert den Datensatz nach Kontostand aufsteigend und zeigt die ersten 5 Zeilen
head(Daten[order(Daten$balance), c("age", "job", "marital", "balance")], 5)


# Der typische Kontostand pro Job-Gruppe
aggregate(balance ~ job, data = Daten, FUN = median)



# Berechnet den prozentualen Anteil der Kunden im Minus pro Jobgruppe
prozent_im_minus <- aggregate(balance < 0 ~ job, data = Daten, FUN = mean)
prozent_im_minus$balance <- round(prozent_im_minus$balance * 100, 2)
print(prozent_im_minus)






library(ggplot2)
library(dplyr)

# Schritt 1: Nur Kunden mit echtem Minus filtern
schulden_daten <- Daten %>% filter(balance < 0)

# Schritt 2: Das Diagramm zeichnen (nach Median-Schulden sortiert)
ggplot(schulden_daten, aes(x = reorder(job, balance, FUN = median), y = balance, fill = job)) +
  geom_boxplot(outlier.color = "lightcoral", outlier.alpha = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none") + # Legende weg, steht ja schon auf der X-Achse
  labs(title = "Wer rutscht am tiefsten ins Minus?",
       x = "Berufsgruppe",
       y = "Kontostand (Schulden) in €")



# Zeigt die 5 tiefsten Kontominus-Werte inklusive Alter und Job
head(Daten[order(Daten$balance), c("age", "job", "marital", "balance", "y")], 5)










library(ggplot2)
library(dplyr)

# Nur Kunden mit echtem Minus filtern
schulden_daten <- Daten %>% filter(balance < 0)

ggplot(schulden_daten, aes(x = reorder(job, balance, FUN = median), y = balance)) +
  # Wir nehmen eine einheitliche, ruhige Farbe, das beruhigt das Auge enorm
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "darkred", outlier.alpha = 0.4) +
  # DER GAMECHANGER: Drehen UND Zoom auf den relevanten Bereich (-2000 bis 0)
  coord_flip(ylim = c(-2000, 0)) + 
  theme_minimal() +
  labs(title = "Schulden-Tiefe: Wer steckt am tiefsten im Minus?",
       subtitle = "Fokus auf den Bereich bis -2.000 € (Boxen gestreckt für bessere Lesbarkeit)",
       x = "Berufsgruppe",
       y = "Kontostand (Schulden) in €")


# Days und Zusammenhang mit y
# Check: Hat der Kalendertag einen Einfluss auf den Erfolg?
modell_day <- glm(y_num ~ day, data = Daten, family = "binomial")
summary(modell_day)


# Abschlussquote in % pro Kalendertag berechnen
tag_check <- aggregate(y_num ~ day, data = Daten, FUN = mean)
tag_check$y_num <- round(tag_check$y_num * 100, 2)
print(tag_check)
