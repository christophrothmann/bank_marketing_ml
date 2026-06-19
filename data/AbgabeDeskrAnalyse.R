library(here)
Daten <- read.csv(here::here("data", "bank-full.csv"), header=TRUE, sep=";", fill=TRUE, stringsAsFactors=TRUE)
summary(Daten)



# ========================================================
# DEFAULT ANALYSE 
# ========================================================

# --- Linke Grafik ---
library(ggplot2)
library(here)

plot_links <- ggplot(Daten, aes(x = default)) +
  geom_bar(fill = "#4682B4", color = "black", width = 0.5) + 
  theme_classic() +
  labs(
    title = "Kredit im Verzug? (default)",
    x = "",
    y = "Absolute Häufigkeit"
  ) +
  theme(
    # FIX: plot.title statt plot_title nutzen!
    plot.title = element_text(face = "bold", hjust = 0.5), 
    axis.text = element_text(size = 11)
  )

print(plot_links)

# --- Rechte Grafik ---
library(dplyr)

default_y_probs <- Daten %>%
  group_by(default, y) %>%
  summarise(Anzahl = n(), .groups = "drop") %>%
  group_by(default) %>%
  mutate(Prozent = (Anzahl / sum(Anzahl)) * 100)

gesamt_n <- nrow(Daten)

plot_rechts <- ggplot(default_y_probs, aes(x = default, y = Prozent, fill = y)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.6) +
  
  geom_text(aes(label = sprintf("%.1f", Prozent)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5, fontface = "plain") +
  
  theme_classic() +
  scale_fill_manual(
    values = c("no" = "lightcoral", "yes" = "lightgreen"),
    labels = c(paste0("Abschluss (y): no (Gesamt n=", gesamt_n, ")"), 
               paste0("Abschluss (y): yes (Gesamt n=", gesamt_n, ")"))
  ) +
  labs(
    title = "Zusammenhang zwischen default und y",
    x = "default",
    y = "Häufigkeit (in %)",
    fill = ""
  ) +
  ylim(0, 105) + 
  theme(
    plot_title = element_text(face = "bold", hjust = 0.5),
    legend.position = "top", 
    legend.text = element_text(size = 9)
  )

print(plot_rechts)


# ========================================================
# MONATSANALYSE 
# ========================================================

library(dplyr)
library(ggplot2)
library(scales)

if(!"year" %in% names(Daten)) {
  month_lookup <- c(jan=1, feb=2, mar=3, apr=4, may=5, jun=6, jul=7, aug=8, sep=9, oct=10, nov=11, dec=12)
  month_nums <- month_lookup[as.character(Daten$month)]
  year_diffs <- c(0, diff(month_nums) < 0)
  Daten$year <- 2008 + cumsum(year_diffs)
}

Daten <- Daten %>%
  mutate(
    month_num = c(jan=1, feb=2, mar=3, apr=4, may=5, jun=6, jul=7, aug=8, sep=9, oct=10, nov=11, dec=12)[as.character(month)],
    Datum     = as.Date(paste(year, month_num, "01", sep = "-")),
    Monat_Jahr = format(Datum, "%b %y")
  )

levels_chronologisch <- Daten %>%
  arrange(year, month_num) %>%
  pull(Monat_Jahr) %>%
  unique()

Daten$Monat_Jahr <- factor(Daten$Monat_Jahr, levels = levels_chronologisch)

ggplot(Daten, aes(x = Monat_Jahr, fill = y)) +
  geom_bar(position = "fill") + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("no" = "lightcoral", "yes" = "lightgreen")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Erfolgsquote (y) im echten Zeitverlauf",
       subtitle = "Normiert auf 100% – zeigt die relative Abschlussrate von 2008 bis 2010",
       x = "Zeitverlauf (Monat / Jahr)",
       y = "Prozentualer Anteil",
       fill = "Erfolgreich (y)")

ggplot(Daten, aes(x = Monat_Jahr, fill = poutcome)) +
  geom_bar(position = "fill") + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("failure" = "lightcoral", "other" = "lightblue", 
                               "success" = "lightgreen", "unknown" = "lightgrey")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = "Kundenstruktur (poutcome) im echten Zeitverlauf",
       subtitle = "Normiert auf 100% – zeigt die Verteilung des vorherigen Anruferfolgs",
       x = "Zeitverlauf (Monat / Jahr)",
       y = "Anteil der Kontaktarten in %",
       fill = "Letztes Kampagnenergebnis")




# ========================================================
# BALANCE ANALYSE 
# ========================================================

# --- Linke Grafik ---
library(ggplot2)
library(dplyr)

job_paradox <- Daten %>%
  group_by(job) %>%
  summarise(Median_Guthaben = median(balance)) %>%
  mutate(Farbe = ifelse(job %in% c("student", "unemployed"), "suspekt", "normal"))

ggplot(job_paradox, aes(x = reorder(job, Median_Guthaben), y = Median_Guthaben, fill = Farbe)) +
  geom_col(width = 0.6, alpha = 0.9, show.legend = FALSE) +
  
  geom_text(aes(label = paste0(format(Median_Guthaben, big.mark = ".", decimal.mark = ","), " €")), 
            vjust = -0.6, fontface = "bold", size = 4, color = "black") +
  
  scale_fill_manual(values = c("normal" = "steelblue", "suspekt" = "steelblue")) + 
  
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 950)) +
  labs(
    title = "Budget pro Berufsgruppe",
    x = "Berufsgruppe",
    y = "Kontostand (Median) in €"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "darkgray", margin = margin(b = 15)),
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black") 
  )


# --- Rechte Grafik ---

library(ggplot2)
library(dplyr)

grafik_aufteilung <- Daten %>%
  mutate(Kategorie = ifelse(balance >= 0, "Schnitt Guthaben (wenn im Plus)", "Schnitt Schulden (wenn im Minus)")) %>%
  group_by(job, Kategorie) %>%
  summarise(Mittelwert = mean(balance), .groups = "drop") %>%
  mutate(Betrag_Absolut = abs(Mittelwert))

# Jobs logisch sortieren (nach der Höhe des Guthabens)
ordnung_jobs <- grafik_aufteilung %>%
  filter(Kategorie == "Schnitt Guthaben (wenn im Plus)") %>%
  arrange(Mittelwert) %>%
  pull(job)

grafik_aufteilung$job <- factor(grafik_aufteilung$job, levels = ordnung_jobs)

ggplot(grafik_aufteilung, aes(x = job, y = Betrag_Absolut, fill = Kategorie)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0(round(Betrag_Absolut, 0), " €")), 
            position = position_dodge(width = 0.75), vjust = -0.5, fontface = "bold", size = 3.5, color = "black") +
  scale_fill_manual(values = c("Schnitt Schulden (wenn im Minus)" = "#C62828", "Schnitt Guthaben (wenn im Plus)" = "#2E7D32")) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, max(grafik_aufteilung$Betrag_Absolut) * 1.1)) +
  labs(
    title = "Finanzielle Struktur: Guthaben vs. Schulden",
    x = "Berufsgruppe",
    y = "Durchschnittlicher Betrag in €",
    fill = "Kontozustand"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "darkgray", margin = margin(b = 15)),
    panel.grid.major.x = element_blank(), # Vertikale Linien ausblenden
    legend.position = "top", # Legende nach oben packen
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black")
  )




# ========================================================
# AGE ANALYSE 
# ========================================================

par(mfrow = c(1, 3))
# Abschlussrate nach Altersgruppen
Daten$altersgruppe <- cut(Daten$age, 
                          breaks = c(0, 24, 34, 44, 54, 64, Inf), 
                          labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+"),
                          right = TRUE)

# Relative Häufigkeiten für 'yes' pro Altersgruppe berechnen
rate_table <- prop.table(table(Daten$y, Daten$altersgruppe), margin = 2)
abschlussraten <- rate_table["yes", ] * 100

bp <- barplot(abschlussraten, 
              col = "lightgreen", 
              ylim = c(0, 50),
              main = "Abschlussrate (y = yes) nach Altersgruppe",
              ylab = "Abschlussrate (in %)")

text(x = bp, y = abschlussraten + 1.5, 
     labels = sprintf("%.1f", abschlussraten), 
     cex = 1, font = 1)

# --- Histrogramm ---
hist(Daten$age, 
     col = "#4682B4",
     freq = FALSE, 
     main = "Histogramm: Alter (age)",
     xlab = "", 
     ylab = "Dichte")

# --- Boxplot ---
boxplot(Daten$age, 
        col = "#4682B4", 
        main = "Boxplot: Alter (age)",
        ylab = "Alter")
par(mfrow = c(1, 1))


# ========================================================
# PREVIOUS UND AGE ANALYSE 
# ========================================================

# --- Rechte Grafik ---
library(ggplot2)

ggplot(Daten, aes(x = altersgruppe, fill = altersgruppe)) +
  geom_bar(color = "black") +
  theme_minimal() +
  # Eine ruhige Farbpalette, sieht sehr professionell aus
  scale_fill_brewer(palette = "Blues") + 
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  labs(title = "Zielgruppen-Check: Wer wird eigentlich angerufen?",
       x = "Altersgruppe",
       y = "Absolute Anzahl der Kontaktierten",
       fill = "Altersgruppe")


# --- Spearman Korrelation ---
spearman_wert <- cor(Daten$age, Daten$previous, method = "spearman")
cat(sprintf("Der globale Spearman-Wert beträgt: %.2f\n\n", spearman_wert))



# --- Tabelle, Entdeckung ---
tabelle_analyse <- Daten %>%
  mutate(
    altersgruppe_spezial = cut(
      age, 
      breaks = c(17, 30, 50, 70, Inf), 
      labels = c("18 - 30", "31 - 50", "51 - 70", "71+")
    )
  ) %>%
  group_by(altersgruppe_spezial) %>%
  summarise(
    previous_durchschnitt = round(mean(previous, na.rm = TRUE), 2)
  )
print(tabelle_analyse)


# ========================================================
# POUTCOME UND AGE ANALYSE 
# ========================================================

# --- Rechte Tabelle ---
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
  labs(title = "Erfolg nach Altersgruppen",
       x = "Altersgruppe",
       y = "Anteil der Ergebnisse in %",
       fill = "Letztes Ergebnis (poutcome)")


# --- Kreuztabelle ---
library(dplyr)

tabelle_alle_gruppen <- Daten %>%
  mutate(
    altersgruppe = cut(
      age, 
      breaks = c(17, 30, 50, 70, Inf), 
      labels = c("18 - 30", "31 - 50", "51 - 70", "71+")
    ),
    Bereits_kontaktiert = ifelse(previous > 0, "ja", "nein")
  ) %>%

  group_by(altersgruppe, Bereits_kontaktiert) %>%
  summarise(
    y = round(sum(y == "yes") / n() * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(altersgruppe, desc(Bereits_kontaktiert))

print(tabelle_alle_gruppen)



# ========================================================
# AGE UND BALANCE ANALYSE 
# ========================================================


library(ggplot2)
library(dplyr)
library(here)


Daten_Analyse <- Daten %>%
  mutate(
    y_num = ifelse(y == "yes", 1, 0),
    altersgruppe = cut(
      age, 
      breaks = c(17, 30, 50, 70, Inf), 
      labels = c("18-30 (Junge Erwachsene)", 
                 "31-50 (Mittelalter)", 
                 "51-70 (Ältere Erwachsene)", 
                 "71+ (Senioren)")
    )
  )

# --- Rechte Grafik ---
highlights_schulden <- Daten_Analyse %>%
  group_by(altersgruppe) %>%
  summarise(Median_Guthaben = median(balance), .groups = "drop")

ggplot(highlights_schulden, aes(x = altersgruppe, y = Median_Guthaben, fill = altersgruppe)) +
  geom_col(width = 0.6, alpha = 0.9, show.legend = FALSE) +
  
  geom_text(aes(label = paste0(format(Median_Guthaben, big.mark = ".", decimal.mark = ","), " €")), 
            vjust = -0.6, fontface = "bold", size = 5, color = "black") +
  
  scale_fill_manual(values = c("18-30 (Junge Erwachsene)" = "#B0BEC5", 
                               "31-50 (Mittelalter)" = "#B0BEC5", 
                               "51-70 (Ältere Erwachsene)" = "#B0BEC5", 
                               "71+ (Senioren)" = "#2E7D32")) + 
  theme_minimal(base_size = 14) +
  scale_y_continuous(limits = c(0, 1600)) +
  labs(
    title = "Kontostand nach Altersgruppen",
    x = "Altersgruppe",
    y = "Kontostand (Median) in €"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_text(face = "bold", color = "black")
  )


# --- Kausalitätsberechnung ---
cat("\nStarte den ultimativen Kausalitäts-Check...\n\n")

# Modell rechnet jetzt mit genau demselben vorbereiteten Datensatz
modell_kontrolle <- glm(y_num ~ altersgruppe + balance, data = Daten_Analyse, family = "binomial")
summary(modell_kontrolle)

# Effekt berechnen
beta_balance <- coef(modell_kontrolle)["balance"]
prozent_effekt <- (exp(beta_balance * 1000) - 1) * 100

cat(sprintf("Ergebnis:\n"))
cat(sprintf("Pro 1.000€ mehr Guthaben steigt die Abschlusschance isoliert betrachtet um: %.2f%%\n", prozent_effekt))




# ========================================================
# CAMPAIN ANALYSE 
# ========================================================

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

if(!"year" %in% names(Daten)) {
  month_lookup <- c(jan=1, feb=2, mar=3, apr=4, may=5, jun=6, jul=7, aug=8, sep=9, oct=10, nov=11, dec=12)
  month_nums <- month_lookup[as.character(Daten$month)]
  year_diffs <- c(0, diff(month_nums) < 0)
  Daten$year <- 2008 + cumsum(year_diffs)
}

zeit_daten <- Daten %>%
  group_by(year, month) %>%
  summarise(
    Anzahl_Kontakte = n(),
    Abschluesse     = sum(y == "yes"),
    Abschlussquote  = (Abschluesse / Anzahl_Kontakte) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    month_num = c(jan=1, feb=2, mar=3, apr=4, may=5, jun=6, jul=7, aug=8, sep=9, oct=10, nov=11, dec=12)[as.character(month)],
    Datum     = as.Date(paste(year, month_num, "01", sep = "-"))
  ) %>%
  arrange(Datum)

# --- Oberer Plot ---
p1 <- ggplot(zeit_daten, aes(x = Datum)) +
  geom_line(aes(y = Anzahl_Kontakte, color = "Gesamtzahl Anrufe"), size = 1.2) +
  geom_point(aes(y = Anzahl_Kontakte, color = "Gesamtzahl Anrufe"), size = 2.5) +
  geom_text(aes(y = Anzahl_Kontakte, label = Anzahl_Kontakte), 
            vjust = 1.8, size = 3, color = "firebrick", fontface = "plain") +
  
  geom_line(aes(y = Abschluesse, color = "Erfolgreiche Abschlüsse"), size = 1.2) +
  geom_point(aes(y = Abschluesse, color = "Erfolgreiche Abschlüsse"), size = 2.5) +
  geom_text(aes(y = Abschluesse, label = Abschluesse), 
            vjust = -1.2, size = 3, color = "dodgerblue3", fontface = "plain") +
  
  scale_color_manual(values = c("Erfolgreiche Abschlüsse" = "dodgerblue3", "Gesamtzahl Anrufe" = "firebrick")) +
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months", expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(labels = comma, limits = c(0, 8500)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Zeitverlauf der Werbekampagne (Mai 2008 - November 2010)",
    subtitle = "Gegenüberstellung von Anrufvolumen (rot) und erfolgreichen Abschlüssen (blau) auf gleicher Skala",
    y = "Anzahl Kontakte / Abschlüsse (Absolut)",
    x = "",
    color = "Metrik"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "dimgray"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black")
  )

# --- Unterer Plot ---
p2 <- ggplot(zeit_daten, aes(x = Datum, y = Abschlussquote)) +
  # Grüne Linie + Punkte + Prozentlabels
  geom_line(color = "forestgreen", size = 1.2) +
  geom_point(color = "forestgreen", size = 2.5) +
  geom_text(aes(label = sprintf("%.1f%%", Abschlussquote)), 
            vjust = -1.0, size = 3, color = "forestgreen", fontface = "plain") +
  
  scale_x_date(date_labels = "%b %y", date_breaks = "2 months", expand = expansion(mult = c(0.03, 0.03))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 70)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Entwicklung der relativen Abschlussquote (in %)",
    y = "Abschlussquote (in %)",
    x = ""
  ) +
  theme(
    plot.title = element_text(size = 12, color = "dimgray", face = "plain"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black") 
  )

print(p1)
print(p2)

# --- Pearson Korrelation ---
y_numerisch <- ifelse(Daten$y == "yes", 1, 0)
pearson_kampagne <- cor(Daten$campaign, y_numerisch, method = "pearson")
cat(sprintf("Pearson-Korrelationskoeffizient: %.2f\n", pearson_kampagne))




# ==============================================================================
# DAY ANALYSE (PRO DUALE ACHSE)
# ==============================================================================
library(ggplot2)
library(dplyr)

day_story <- Daten %>%
  mutate(y_num = ifelse(y == "yes", 1, 0)) %>% # Stellt sicher, dass y_num existiert
  group_by(day) %>%
  summarise(
    Anrufe = n(),
    Quote = mean(y_num) * 100,
    .groups = "drop"
  )

ggplot(day_story, aes(x = day)) +
  geom_col(aes(y = Anrufe), fill = "steelblue", alpha = 0.7, width = 0.8) +
  
  geom_line(aes(y = Quote * 80), color = "#C62828", linewidth = 1.2) + 
  geom_point(aes(y = Quote * 80), color = "#C62828", size = 2) +
  
  scale_y_continuous(
    name = NULL,
    sec.axis = sec_axis(~./80, name = NULL)
  ) +
  
  scale_x_continuous(breaks = 1:31) +
  
  theme_minimal(base_size = 14) +
  labs(
    title = "Kontakte und Erfolg",
    x = "Kalendertag"
  ) +
  
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    panel.grid.minor = element_blank(),
    axis.text.y.left = element_text(color = "#37474F", face = "bold"),
    axis.text.y.right = element_text(color = "#C62828", face = "bold"),
    axis.text.x = element_text(size = 10)
  )
