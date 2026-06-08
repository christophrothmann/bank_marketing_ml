# --- Benötigte Bibliotheken prüfen und installieren ---
required_packages <- c("dplyr", "here", "ggplot2")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cran.rstudio.com/")
}

library(dplyr)
library(here)
library(ggplot2)
library(grid)

# Set global options
options(scipen = 999)

# Define directories
output_dir <- here::here("source", "analytics_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
report_file <- here::here(
  "docs",
  "descriptive_report.md"
)

# Reset report file
cat(
  paste0(
    "# Deskriptiver Analysebericht: Die ersten 8 Variablen des ",
    "Bank-Marketing Datensatzes\n\n"
  ),
  file = report_file,
  append = FALSE
)
cat(
  paste0(
    "Dieses Dokument enthält die deskriptive Analyse für die ersten 8 ",
    "Variablen gemäß der Methodik aus den Vorlesungsfolien (Kapitel 2).\n\n"
  ),
  file = report_file,
  append = TRUE
)

# --- 1. Daten laden ---
Daten <- read.csv(
  here::here("data", "bank-full.csv"),
  header = TRUE,
  sep = ";",
  fill = TRUE,
  stringsAsFactors = TRUE
)

# --- 2. Datenaufbereitung & Typkonvertierung ---
# education als ordinale Variable
# "unknown" wird als fehlender Wert (NA) für mathematische
# Berechnungen behandelt, damit die Rangfolge erhalten bleibt.
Daten$education <- factor(
  Daten$education,
  levels = c("primary", "secondary", "tertiary"),
  ordered = TRUE
)

# Nominale Variablen explizit als Faktoren sicherstellen
Daten$job <- as.factor(Daten$job)
Daten$marital <- as.factor(Daten$marital)
Daten$default <- as.factor(Daten$default)
Daten$housing <- as.factor(Daten$housing)
Daten$loan <- as.factor(Daten$loan)
Daten$y <- as.factor(Daten$y)

# --- 3. Hilfsfunktionen definieren ---

# Modus (häufigster Wert) für kategoriale Variablen
get_modus <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) {
    return(NA)
  }
  tbl <- table(x)
  names(tbl)[which.max(tbl)]
}

# Alpha-Quantil für ordinale Variablen (basierend auf der
# kumulativen relativen Häufigkeit)
get_ordinal_quantile <- function(ord_factor, prob) {
  ord_factor <- na.omit(ord_factor)
  if (length(ord_factor) == 0) {
    return(NA)
  }
  tbl <- table(ord_factor)
  cum_prop <- cumsum(tbl) / sum(tbl)
  names(cum_prop)[which(cum_prop >= prob)[1]]
}

# Hilfsfunktion zur Formatierung von Tabellen für Markdown
format_markdown_table <- function(
  tbl,
  title_row = c("Kategorie", "Absolut", "Relativ (%)")
) {
  df <- as.data.frame(tbl)
  if (ncol(df) == 2) {
    total <- sum(df$Freq)
    df$Pct <- round(df$Freq / total * 100, 2)
    md <- paste0("| ", paste(title_row, collapse = " | "), " |\n")
    md <- paste0(md, "|:---|:---|:---|\n")
    for (i in 1:nrow(df)) {
      md <- paste0(
        md, "| ", df[i, 1], " | ", df[i, 2], " | ", df[i, 3], "% |\n"
      )
    }
  } else if (ncol(df) == 3) {
    wide_tbl <- as.matrix(tbl)
    cols <- colnames(wide_tbl)
    rows <- rownames(wide_tbl)

    md <- paste0(
      "| Kategorie \\ y | ", paste(cols, collapse = " | "), " | Gesamt |\n"
    )
    md <- paste0(
      md, "|:---|",
      paste(rep("---|", length(cols) + 1), collapse = ""), "\n"
    )

    row_totals <- rowSums(wide_tbl)

    for (r in rows) {
      row_vals <- wide_tbl[r, ]
      pct_vals <- round(row_vals / row_totals[r] * 100, 1)
      row_str <- paste0("| **", r, "** | ")
      for (c in cols) {
        row_str <- paste0(row_str, row_vals[c], " (", pct_vals[c], "%) | ")
      }
      row_str <- paste0(row_str, row_totals[r], " |\n")
      md <- paste0(md, row_str)
    }
  }
  return(md)
}

# Hilfsfunktion zur Formatierung metrischer Kennzahlen
format_metric_stats <- function(var_name, x) {
  mean_val <- round(mean(x, na.rm = TRUE), 2)
  med_val <- round(median(x, na.rm = TRUE), 2)
  sd_val <- round(sd(x, na.rm = TRUE), 2)
  mad_val <- round(mad(x, na.rm = TRUE), 2)
  iqr_val <- round(IQR(x, na.rm = TRUE), 2)
  min_val <- round(min(x, na.rm = TRUE), 2)
  max_val <- round(max(x, na.rm = TRUE), 2)
  q25 <- round(quantile(x, 0.25, na.rm = TRUE), 2)
  q75 <- round(quantile(x, 0.75, na.rm = TRUE), 2)

  md <- paste0("### Metrische Kennzahlen für **", var_name, "**\n\n")
  md <- paste0(md, "| Kennzahl | Wert |\n")
  md <- paste0(md, "|:---|:---|\n")
  md <- paste0(md, "| Mittelwert | ", mean_val, " |\n")
  md <- paste0(md, "| Median | ", med_val, " |\n")
  md <- paste0(md, "| Standardabweichung | ", sd_val, " |\n")
  md <- paste0(md, "| MAD (Robust) | ", mad_val, " |\n")
  md <- paste0(md, "| Interquartilsabstand (IQR) | ", iqr_val, " |\n")
  md <- paste0(md, "| Minimum | ", min_val, " |\n")
  md <- paste0(md, "| 25%-Quantil | ", q25, " |\n")
  md <- paste0(md, "| 75%-Quantil | ", q75, " |\n")
  md <- paste0(md, "| Maximum | ", max_val, " |\n\n")
  return(md)
}

# Hilfsfunktion zur Formatierung ordinaler Kennzahlen
format_ordinal_stats <- function(var_name, ord_factor) {
  modus_val <- get_modus(ord_factor)
  med_val <- get_ordinal_quantile(ord_factor, 0.5)
  q25 <- get_ordinal_quantile(ord_factor, 0.25)
  q75 <- get_ordinal_quantile(ord_factor, 0.75)

  md <- paste0("### Ordinale Kennzahlen für **", var_name, "**\n\n")
  md <- paste0(md, "| Kennzahl | Kategorie |\n")
  md <- paste0(md, "|:---|:---|\n")
  md <- paste0(md, "| Modus (Häufigster Wert) | ", modus_val, " |\n")
  md <- paste0(md, "| 25%-Quantil | ", q25, " |\n")
  md <- paste0(md, "| Median (50%-Quantil) | ", med_val, " |\n")
  md <- paste0(md, "| 75%-Quantil | ", q75, " |\n\n")
  return(md)
}

# Hilfsfunktion zur Formatierung nominaler Kennzahlen
format_nominal_stats <- function(var_name, x) {
  modus_val <- get_modus(x)
  md <- paste0("### Nominale Kennzahlen für **", var_name, "**\n\n")
  md <- paste0(md, "| Kennzahl | Wert |\n")
  md <- paste0(md, "|:---|:---|\n")
  md <- paste0(md, "| Modus (Häufigster Wert) | ", modus_val, " |\n\n")
  return(md)
}

# Hilfsfunktion zur Berechnung der Spanne der Abschlussraten für kategoriale Variablen
get_conversion_span <- function(data, var_name, translate = TRUE) {
  tbl <- table(data[[var_name]], data$y)
  row_totals <- rowSums(tbl)
  yes_col <- which(colnames(tbl) == "yes")
  rates <- round((tbl[, yes_col] / row_totals) * 100, 1)

  rates <- na.omit(rates)

  min_idx <- which.min(rates)
  max_idx <- which.max(rates)

  min_val <- rates[min_idx]
  min_cat <- names(rates)[min_idx]
  max_val <- rates[max_idx]
  max_cat <- names(rates)[max_idx]

  translate_cat <- function(cat) {
    if (translate) {
      if (cat == "yes") {
        return("ja")
      }
      if (cat == "no") {
        return("nein")
      }
    }
    return(cat)
  }

  paste0(
    sprintf("%.1f", min_val), "% (", translate_cat(min_cat), ") bis ",
    sprintf("%.1f", max_val), "% (", translate_cat(max_cat), ")"
  )
}

# Hilfsfunktion zur Formatierung gruppierter metrischer Kennzahlen
format_bivariate_metric_stats <- function(var_name, x, y) {
  grouped <- data.frame(x = x, y = y)
  grouped <- na.omit(grouped)

  stats_no <- summary(grouped$x[grouped$y == "no"])
  stats_yes <- summary(grouped$x[grouped$y == "yes"])
  stats_all <- summary(grouped$x)

  sd_no <- round(sd(grouped$x[grouped$y == "no"]), 2)
  sd_yes <- round(sd(grouped$x[grouped$y == "yes"]), 2)
  sd_all <- round(sd(grouped$x), 2)

  md <- paste0(
    "### Zusammenhang zwischen **", var_name,
    "** und **y** (Abschluss)\n\n"
  )
  md <- paste0(md, "| Kennzahl | y = no | y = yes | Gesamt |\n")
  md <- paste0(md, "|:---|:---|:---|:---|\n")
  md <- paste0(
    md, "| Minimum | ", stats_no["Min."], " | ",
    stats_yes["Min."], " | ", stats_all["Min."], " |\n"
  )
  md <- paste0(
    md, "| 25%-Quantil | ", stats_no["1st Qu."], " | ",
    stats_yes["1st Qu."], " | ", stats_all["1st Qu."], " |\n"
  )
  md <- paste0(
    md, "| Median | ", stats_no["Median"], " | ",
    stats_yes["Median"], " | ", stats_all["Median"], " |\n"
  )
  md <- paste0(
    md, "| Mittelwert | ", round(stats_no["Mean"], 2), " | ",
    round(stats_yes["Mean"], 2), " | ",
    round(stats_all["Mean"], 2), " |\n"
  )
  md <- paste0(
    md, "| 75%-Quantil | ", stats_no["3rd Qu."], " | ",
    stats_yes["3rd Qu."], " | ", stats_all["3rd Qu."], " |\n"
  )
  md <- paste0(
    md, "| Maximum | ", stats_no["Max."], " | ",
    stats_yes["Max."], " | ", stats_all["Max."], " |\n"
  )
  md <- paste0(
    md, "| Standardabweichung | ", sd_no, " | ",
    sd_yes, " | ", sd_all, " |\n\n"
  )
  return(md)
}

# Hilfsfunktion zum Plotten von Prozent-Kreuztabellen
plot_percentage_barplot <- function(tbl, var_name, filename) {
  row_totals <- rowSums(tbl)
  A_prop <- round(prop.table(tbl, margin = 1) * 100, digits = 1)

  png(file.path(output_dir, filename), width = 700, height = 500)

  if (var_name == "job") {
    par(mar = c(8, 5, 4, 2) + 0.1)
  } else {
    par(mar = c(5, 5, 4, 2) + 0.1)
  }

  obergrenze <- max(A_prop)
  legendentext <- paste0(
    "Abschluss (y): ", colnames(A_prop),
    " (Gesamt n=", sum(tbl), ")"
  )

  my_bar <- barplot(
    t(A_prop),
    beside = TRUE, ylim = c(0, 1.2 * obergrenze),
    col = c("lightcoral", "lightgreen"),
    legend.text = FALSE, # Manuell steuern für feinere Justierung
    ylab = "Häufigkeit (in %)", xlab = var_name,
    main = paste("Zusammenhang zwischen", var_name, "und y"),
    las = ifelse(var_name == "job", 2, 1)
  )

  legend(
    "topright",
    legend = legendentext,
    fill = c("lightcoral", "lightgreen"), bty = "n", cex = 1
  )

  text(my_bar, t(A_prop) + obergrenze / 25, t(A_prop), cex = 0.8)

  dev.off()
}

# ==============================================================================
# --- 4. Übersichtstabelle ---
# ==============================================================================
cat(
  "### Übersicht: Einfluss der Variablen auf y (auf einen Blick)\n\n",
  file = report_file,
  append = TRUE
)
cat(
  paste0(
    "Die folgende Tabelle gibt eine Übersicht über den statistischen ",
    "Einfluss der ersten 8 Variablen auf die Zielvariable **y** ",
    "(Abschluss). Gemäß Kapitel 2 der Vorlesungsfolien wird dies für ",
    "kategoriale Variablen über die Spanne der Abschlussraten (aus den ",
    "Kreuztabellen) und für metrische Variablen über die ",
    "Korrelationskoeffizienten beschrieben:\n\n"
  ),
  file = report_file,
  append = TRUE
)
cat(
  paste0(
    "| Variable | Skalenniveau | Statistisches Maß (aus Folien) | ",
    "Erläuterung des Effekts |\n"
  ),
  file = report_file,
  append = TRUE
)
cat(
  "|:---|:---|:---|:---|\n",
  file = report_file,
  append = TRUE
)

# Zeilen der Übersichtstabelle schreiben (exakte Berechnungen)
cat(
  paste0(
    "| **housing** | nominal | Abschlussrate: ",
    get_conversion_span(Daten, "housing", translate = TRUE), " | ",
    "Deutlicher negativer Einfluss bei bestehendem Kredit |\n"
  ),
  file = report_file, append = TRUE
)
cat(
  paste0(
    "| **job** | nominal | Abschlussrate: ",
    get_conversion_span(Daten, "job", translate = FALSE), " | ",
    "Starker Einfluss (Studenten/Rentner schließen oft ab) |\n"
  ),
  file = report_file, append = TRUE
)
cat(
  paste0(
    "| **education** | ordinal | Abschlussrate: ",
    get_conversion_span(Daten, "education", translate = FALSE), " | ",
    "Moderater positiver Einfluss mit steigender Bildung |\n"
  ),
  file = report_file, append = TRUE
)
cat(
  paste0(
    "| **loan** | nominal | Abschlussrate: ",
    get_conversion_span(Daten, "loan", translate = TRUE), " | ",
    "Spürbarer negativer Einfluss bei bestehendem Privatkredit |\n"
  ),
  file = report_file, append = TRUE
)
cat(
  paste0(
    "| **marital** | nominal | Abschlussrate: ",
    get_conversion_span(Daten, "marital", translate = FALSE), " | ",
    "Moderater Einfluss (Singles schließen am häufigsten ab) |\n"
  ),
  file = report_file, append = TRUE
)
# Pearson und Spearman für metrische Variablen
y_num <- ifelse(Daten$y == "yes", 1, 0)
cor_age_p <- round(cor(Daten$age, y_num, method = "pearson"), 4)
cor_age_s <- round(cor(Daten$age, y_num, method = "spearman"), 4)
cor_bal_p <- round(cor(Daten$balance, y_num, method = "pearson"), 4)
cor_bal_s <- round(cor(Daten$balance, y_num, method = "spearman"), 4)

cat(
  paste0(
    "| **balance** | metrisch | Pearson-R: ", cor_bal_p, " / Spearman: ",
    cor_bal_s, " | Geringer positiver Einfluss (Guthaben-Median ist höher) |\n"
  ),
  file = report_file, append = TRUE
)
cat(
  paste0(
    "| **age** | metrisch | Pearson-R: ", cor_age_p, " / Spearman: ",
    cor_age_s,
    " | Minimaler linearer Korrelationseffekt (U-Verlauf vorhanden) |\n"
  ),
  file = report_file, append = TRUE
)
cat(
  paste0(
    "| **default** | nominal | Abschlussrate: ",
    get_conversion_span(Daten, "default", translate = TRUE), " | ",
    "Sehr geringer Einfluss (Verzugskunden schließen seltener ab) |\n"
  ),
  file = report_file, append = TRUE
)

cat("\n---\n\n", file = report_file, append = TRUE)


# ==============================================================================
# --- 5. Detaillierte Analyse der einzelnen Variablen ---
# ==============================================================================

# ------------------------------------------------------------------------------
# Variable 1: age (metrisch)
# ------------------------------------------------------------------------------
cat(
  "## Variable 1: **age** (metrisch)\n\n",
  file = report_file, append = TRUE
)

# Stats
cat(
  format_metric_stats("age", Daten$age),
  file = report_file, append = TRUE
)

# Univariates Bild (Histogramm + Boxplot)
png(
  file.path(output_dir, "univariate_1_age.png"),
  width = 800, height = 400
)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
hist(
  Daten$age,
  freq = FALSE,
  main = "Histogramm: Alter (age)",
  xlab = "Alter", ylab = "Dichte", col = "steelblue"
)
boxplot(
  Daten$age,
  main = "Boxplot: Alter (age)",
  ylab = "Alter", col = "steelblue"
)
dev.off()

# Bivariat mit y (Grouped Boxplots)
cat(
  format_bivariate_metric_stats("age", Daten$age, Daten$y),
  file = report_file, append = TRUE
)

png(
  file.path(output_dir, "bivariate_1_age_y.png"),
  width = 600, height = 450
)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
boxplot(
  age ~ y,
  data = Daten,
  main = "Alter (age) nach Abschluss (y)",
  xlab = "Abschluss (y)", ylab = "Alter",
  col = c("lightcoral", "lightgreen")
)
dev.off()

# Neue Bivariate Visualisierung: Abschlussrate nach Altersgruppe
# (zeigt nicht-linearen U-Verlauf!)
Daten$age_group <- cut(
  Daten$age,
  breaks = c(0, 25, 35, 45, 55, 65, 100),
  labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+")
)
tbl_age_y <- table(Daten$age_group, Daten$y)
A_age_prop <- round(prop.table(tbl_age_y, margin = 1) * 100, digits = 1)

png(
  file.path(output_dir, "bivariate_1_age_group_y.png"),
  width = 600, height = 450
)
par(mar = c(5, 5, 4, 2) + 0.1)
my_bar <- barplot(
  A_age_prop[, "yes"],
  ylim = c(0, 1.2 * max(A_age_prop[, "yes"])),
  col = "lightgreen",
  ylab = "Abschlussrate (in %)", xlab = "Altersgruppen",
  main = "Abschlussrate (y = yes) nach Altersgruppe"
)
text(my_bar, A_age_prop[, "yes"] + 1, A_age_prop[, "yes"], cex = 0.9)
dev.off()

# Bewertung
cat(
  paste0(
    "### Einfluss auf die Zielvariable y\n\n",
    "* **Lineare Korrelation (Pearson-R)**: ", cor_age_p, "\n",
    "* **Rangkorrelation (Spearman-R)**: ", cor_age_s, "\n",
    "* **Bewertungssatz**: Das Alter hat einen minimalen ",
    "linearen Einfluss auf ",
    "den Abschluss (Pearson-R: ", cor_age_p, "). ",
    "Der Mittelwert der Kunden, die abschließen, liegt geringfügig höher, ",
    "aber der Median ist leicht niedriger. Die geringe lineare Korrelation ",
    "ist jedoch irreführend, da ein nicht-linearer U-Verlauf ",
    "vorliegt: Sehr junge Kunden (<25 Jahre) und ältere Kunden (65+ Jahre) ",
    "haben eine deutlich höhere Abschlussrate (siehe Grafik in v2).\n\n"
  ),
  file = report_file,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Variable 2: job (nominal)
# ------------------------------------------------------------------------------
cat(
  "## Variable 2: **job** (nominal)\n\n",
  file = report_file, append = TRUE
)

# Stats
cat(
  format_nominal_stats("job", Daten$job),
  file = report_file, append = TRUE
)
tbl_job <- table(Daten$job)
cat(
  "### Häufigkeitstabelle für **job**\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(
    tbl_job,
    c("Berufsgruppe", "Absolut", "Relativ (%)")
  ),
  file = report_file, append = TRUE
)

# Univariates Bild
png(
  file.path(output_dir, "univariate_2_job.png"),
  width = 800, height = 500
)
par(mar = c(8, 4, 4, 2) + 0.1)
barplot(
  tbl_job,
  main = "Häufigkeit der Berufsgruppen (job)",
  col = "steelblue", las = 2, ylab = "Absolute Häufigkeit"
)
dev.off()

# Bivariat mit y
tbl_job_y <- table(Daten$job, Daten$y)
cat(
  "\n### Kreuztabelle für **job** und **y** (Abschluss)\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(tbl_job_y),
  file = report_file, append = TRUE
)

plot_percentage_barplot(tbl_job_y, "job", "bivariate_2_job_y.png")

# Bewertung
cat(
  paste0(
    "### Einfluss auf die Zielvariable y\n\n",
    "* **Zusammenhangsmaß (Abschlussrate)**: Spanne von ",
    get_conversion_span(Daten, "job", translate = FALSE), "\n",
    "* **Bewertungssatz**: Der Beruf hat einen deutlichen Einfluss auf den ",
    "Abschluss. Insbesondere Studenten (28.7% Erfolgsrate) und Rentner ",
    "(22.8% Erfolgsrate) schließen überdurchschnittlich häufig Festgelder ",
    "ab, während Arbeiter ('blue-collar', 7.3%) und Unternehmer ",
    "('entrepreneur', 8.3%) sehr niedrige Raten aufweisen.\n\n"
  ),
  file = report_file,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Variable 3: marital (nominal)
# ------------------------------------------------------------------------------
cat(
  "## Variable 3: **marital** (nominal)\n\n",
  file = report_file, append = TRUE
)

# Stats
cat(
  format_nominal_stats("marital", Daten$marital),
  file = report_file, append = TRUE
)
tbl_marital <- table(Daten$marital)
cat(
  "### Häufigkeitstabelle für **marital**\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(
    tbl_marital,
    c("Familienstand", "Absolut", "Relativ (%)")
  ),
  file = report_file, append = TRUE
)

# Univariates Bild
png(
  file.path(output_dir, "univariate_3_marital.png"),
  width = 600, height = 450
)
par(mar = c(5, 4, 4, 2) + 0.1)
barplot(
  tbl_marital,
  main = "Häufigkeit des Familienstands (marital)",
  col = "steelblue", ylab = "Absolute Häufigkeit"
)
dev.off()

# Bivariat mit y
tbl_marital_y <- table(Daten$marital, Daten$y)
cat(
  "\n### Kreuztabelle für **marital** und **y** (Abschluss)\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(tbl_marital_y),
  file = report_file, append = TRUE
)

plot_percentage_barplot(tbl_marital_y, "marital", "bivariate_3_marital_y.png")

# Bewertung
cat(
  paste0(
    "### Einfluss auf die Zielvariable y\n\n",
    "* **Zusammenhangsmaß (Abschlussrate)**: Spanne von ",
    get_conversion_span(Daten, "marital", translate = FALSE), "\n",
    "* **Bewertungssatz**: Der Familienstand hat einen moderaten Einfluss auf ",
    "den Abschluss. Singles haben mit 14.9% die höchste Abschlussquote, ",
    "gefolgt von Geschiedenen (11.9%) und Verheirateten (10.1%).\n\n"
  ),
  file = report_file,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Variable 4: education (ordinal)
# ------------------------------------------------------------------------------
cat(
  "## Variable 4: **education** (ordinal)\n\n",
  file = report_file, append = TRUE
)

# Stats
cat(
  format_ordinal_stats("education", Daten$education),
  file = report_file, append = TRUE
)
tbl_edu <- table(Daten$education, useNA = "ifany")
cat(
  "### Häufigkeitstabelle für **education** (inkl. NA / unknown)\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(
    tbl_edu,
    c("Bildungsstand", "Absolut", "Relativ (%)")
  ),
  file = report_file, append = TRUE
)

# Univariates Bild (Barplot & Boxplot numerisch)
png(
  file.path(output_dir, "univariate_4_education.png"),
  width = 800, height = 400
)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
# Barplot (Häufigkeit)
barplot(
  table(Daten$education),
  main = "Häufigkeit Bildungsstand",
  col = "steelblue", ylab = "Absolute Häufigkeit"
)
# Boxplot (Numerisch)
boxplot(
  as.numeric(na.omit(Daten$education)),
  main = "Boxplot Bildungsstand (1-3)",
  names = "1=prim, 2=sek, 3=tert", col = "steelblue"
)
dev.off()

# Bivariat mit y
tbl_edu_y <- table(Daten$education, Daten$y, useNA = "no")
cat(
  "\n### Kreuztabelle für **education** und **y** (Abschluss, ohne NA)\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(tbl_edu_y),
  file = report_file, append = TRUE
)

plot_percentage_barplot(
  tbl_edu_y, "education",
  "bivariate_4_education_y.png"
)

# Bewertung
# Spearman-Korrelation berechnen da education ordinal ist
cor_edu_s <- round(
  cor(
    as.numeric(Daten$education),
    y_num,
    method = "spearman",
    use = "complete.obs"
  ),
  4
)
cat(
  paste0(
    "### Einfluss auf die Zielvariable y\n\n",
    "* **Zusammenhangsmaß (Abschlussrate)**: Spanne von ",
    get_conversion_span(Daten, "education", translate = FALSE), "\n",
    "* **Rangkorrelation (Spearman-R)**: ", cor_edu_s, "\n",
    "* **Bewertungssatz**: Der Bildungsstand hat einen moderaten positiven ",
    "Einfluss auf den Abschluss (Spearman-R: ", cor_edu_s, "). ",
    "Es zeigt sich ein linearer Trend: Kunden mit höherer Bildung schließen ",
    "häufiger ab (Tertiär: 15.0%, Sekundär: 10.6%, Primär: 8.6%).\n\n"
  ),
  file = report_file,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Variable 5: default (nominal)
# ------------------------------------------------------------------------------
cat(
  "## Variable 5: **default** (nominal)\n\n",
  file = report_file, append = TRUE
)

# Stats
cat(
  format_nominal_stats("default", Daten$default),
  file = report_file, append = TRUE
)
tbl_default <- table(Daten$default)
cat(
  "### Häufigkeitstabelle für **default**\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(
    tbl_default,
    c("Kredit im Verzug?", "Absolut", "Relativ (%)")
  ),
  file = report_file, append = TRUE
)

# Univariates Bild
png(
  file.path(output_dir, "univariate_5_default.png"),
  width = 600, height = 450
)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
barplot(
  tbl_default,
  main = "Kredit im Verzug? (default)",
  col = "steelblue", ylab = "Absolute Häufigkeit"
)
dev.off()

# Bivariat mit y
tbl_default_y <- table(Daten$default, Daten$y)
cat(
  "\n### Kreuztabelle für **default** und **y** (Abschluss)\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(tbl_default_y),
  file = report_file, append = TRUE
)

plot_percentage_barplot(
  tbl_default_y, "default",
  "bivariate_5_default_y.png"
)

# Bewertung
cat(
  paste0(
    "### Einfluss auf die Zielvariable y\n\n",
    "* **Zusammenhangsmaß (Abschlussrate)**: Spanne von ",
    get_conversion_span(Daten, "default", translate = FALSE), "\n",
    "* **Bewertungssatz**: Ein Zahlungsverzug hat einen minimalen Einfluss. ",
    "Kunden mit Zahlungsverzug schließen zwar seltener ab (6.4% vs. 11.8%), ",
    "aber wegen des extrem geringen Anteils betroffener Kunden (1.8% des ",
    "Datensatzes) hat diese Variable eine sehr geringe Gesamtbedeutung.\n\n"
  ),
  file = report_file,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Variable 6: balance (metrisch)
# ------------------------------------------------------------------------------
cat(
  "## Variable 6: **balance** (metrisch)\n\n",
  file = report_file, append = TRUE
)

# Stats
cat(
  format_metric_stats("balance", Daten$balance),
  file = report_file, append = TRUE
)

# Univariates Bild (Histogramm + Boxplot ohne Outliers für Visualisierung)
png(
  file.path(output_dir, "univariate_6_balance.png"),
  width = 800, height = 400
)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
hist(
  Daten$balance,
  freq = FALSE,
  main = "Histogramm: Guthaben (balance)",
  xlab = "Guthaben (in Euro)", ylab = "Dichte", col = "steelblue"
)
boxplot(
  Daten$balance,
  main = "Boxplot (ohne Ausreißer)",
  ylab = "Guthaben (in Euro)", col = "steelblue", outline = FALSE
)
dev.off()

# Bivariat mit y
cat(
  format_bivariate_metric_stats("balance", Daten$balance, Daten$y),
  file = report_file, append = TRUE
)

png(
  file.path(output_dir, "bivariate_6_balance_y.png"),
  width = 600, height = 450
)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
boxplot(
  balance ~ y,
  data = Daten, outline = FALSE,
  main = "Guthaben (balance) nach Abschluss (y)\n(ohne Ausreißer)",
  xlab = "Abschluss (y)", ylab = "Guthaben (in Euro)",
  col = c("lightcoral", "lightgreen")
)
dev.off()

# Bewertung
cat(
  paste0(
    "### Einfluss auf die Zielvariable y\n\n",
    "* **Lineare Korrelation (Pearson-R)**: ", cor_bal_p, "\n",
    "* **Rangkorrelation (Spearman-R)**: ", cor_bal_s, "\n",
    "* **Bewertungssatz**: Das Guthaben hat einen geringen, aber ",
    "spürbaren positiven Einfluss auf den Abschluss (Pearson-R: ",
    cor_bal_p, ", Spearman-R: ", cor_bal_s, "). ",
    "Kunden, die abschließen, haben ein deutlich höheres ",
    "Durchschnittsguthaben (Mittelwert 1804.27 € vs. 1303.71 €) ",
    "und einen fast doppelt so hohen Median (733 € vs. 417 €).\n\n"
  ),
  file = report_file,
  append = TRUE
)

summary(outliers)

# --- Ausreißer-Analyse für balance ---
q75_bal <- quantile(Daten$balance, 0.75, na.rm = TRUE)
iqr_bal <- IQR(Daten$balance, na.rm = TRUE)
outlier_cutoff <- q75_bal + 1.5 * iqr_bal
outliers <- Daten[Daten$balance > outlier_cutoff, ]

# Sortieren nach Guthaben absteigend
outliers <- outliers[order(-outliers$balance), ]

# Exportiere alle Variablen der Ausreißer als CSV
outliers_csv_path <- here::here("source", "analytics_output", "balance_outliers.csv")
write.csv(outliers, file = outliers_csv_path, row.names = FALSE)

# Visualisierung für die Ausreißer erstellen
# Wir erstellen ein Grid mit 4 aussagekräftigen Plots unter Verwendung von ggplot2
# 1. Abschlussquote im Vergleich (Normal vs Ausreißer)
# 2. Altersverteilung im Vergleich
# 3. Job-Verteilung im Vergleich (Top-Berufe)
# 4. Kredite im Vergleich

Daten_plot <- Daten
Daten_plot$outlier_group <- ifelse(Daten_plot$balance > outlier_cutoff, "Ausreißer (>3462 €)", "Normalbereich (≤3462 €)")
Daten_plot$outlier_group <- factor(Daten_plot$outlier_group, levels = c("Normalbereich (≤3462 €)", "Ausreißer (>3462 €)"))

colors_group <- c("Normalbereich (≤3462 €)" = "#5A738E", "Ausreißer (>3462 €)" = "#10B981")

premium_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#2C3E50", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 10, color = "#34495E"),
    axis.text = element_text(color = "#2C3E50"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#ECF0F1"),
    legend.position = "none"
  )

# Plot 1: Abschlussquote
conv_data <- Daten_plot %>%
  group_by(outlier_group) %>%
  summarise(
    total = n(),
    yes_count = sum(y == "yes"),
    rate = (yes_count / total) * 100,
    .groups = "drop"
  )

p1 <- ggplot(conv_data, aes(x = outlier_group, y = rate, fill = outlier_group)) +
  geom_bar(stat = "identity", width = 0.5, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f%%", rate)), vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = colors_group) +
  scale_y_continuous(limits = c(0, 20), expand = c(0, 0)) +
  labs(
    title = "Abschlussquote (y = 'yes') im Vergleich",
    x = "Kundengruppe",
    y = "Abschlussrate (%)"
  ) +
  premium_theme

# Plot 2: Altersverteilung
p2 <- ggplot(Daten_plot, aes(x = outlier_group, y = age, fill = outlier_group)) +
  geom_boxplot(width = 0.4, alpha = 0.8, outlier.shape = 16, outlier.alpha = 0.3) +
  scale_fill_manual(values = colors_group) +
  labs(
    title = "Altersverteilung",
    x = "Kundengruppe",
    y = "Alter"
  ) +
  premium_theme

# Plot 3: Top Job Profiles
job_data <- Daten_plot %>%
  group_by(outlier_group, job) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(outlier_group) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  ungroup()

top_jobs <- Daten_plot %>%
  group_by(job) %>%
  summarise(count = n()) %>%
  top_n(6, count) %>%
  pull(job)

job_data_filtered <- job_data %>%
  filter(job %in% top_jobs)

p3 <- ggplot(job_data_filtered, aes(x = reorder(job, percentage), y = percentage, fill = outlier_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = colors_group) +
  labs(
    title = "Häufigste Berufe (Anteil in %)",
    x = "Beruf",
    y = "Anteil an der Gruppe (%)",
    fill = "Kundengruppe"
  ) +
  premium_theme +
  theme(legend.position = "bottom", legend.title = element_blank())

# Plot 4: Kredite im Vergleich
housing_summary <- Daten_plot %>%
  group_by(outlier_group) %>%
  summarise(pct = sum(housing == "yes") / n() * 100, .groups = "drop") %>%
  mutate(loan_type = "Immobilienkredit (housing)")

personal_summary <- Daten_plot %>%
  group_by(outlier_group) %>%
  summarise(pct = sum(loan == "yes") / n() * 100, .groups = "drop") %>%
  mutate(loan_type = "Privatkredit (loan)")

loan_data <- rbind(housing_summary, personal_summary)

p4 <- ggplot(loan_data, aes(x = loan_type, y = pct, fill = outlier_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), position = position_dodge(width = 0.7), vjust = -0.5, fontface = "bold", size = 3) +
  scale_fill_manual(values = colors_group) +
  scale_y_continuous(limits = c(0, 70), expand = c(0, 0)) +
  labs(
    title = "Bestehende Kredite im Vergleich",
    x = "Kreditart",
    y = "Anteil mit Kredit (%)"
  ) +
  premium_theme

# Save to output_dir
outlier_plot_path <- file.path(output_dir, "balance_outliers_analysis.png")
png(outlier_plot_path, width = 1000, height = 800, res = 120)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p3, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p4, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

dev.off()

# Bericht erweitern
cat(
  paste0(
    "### Analyse der Ausreißer (hohe Guthabenwerte/\"budget\")\n\n",
    "Als Ausreißer (oberes Ende) werden Datenpunkte definiert, deren Guthaben ",
    "über dem Schwellenwert von Q3 + 1.5 * IQR liegt. ",
    "Für die Variable **balance** liegt dieser Schwellenwert bei **", outlier_cutoff, " €**.\n\n",
    "* **Anzahl der Ausreißer**: ", nrow(outliers), " von ", nrow(Daten), " Kunden (", round(nrow(outliers) / nrow(Daten) * 100, 2), "%)\n",
    "* **Abschlussrate (y = yes) unter den Ausreißern**: ", round(mean(outliers$y == "yes") * 100, 2), "%\n",
    "* **Vergleich zur Gesamt-Abschlussrate**: ", round(mean(Daten$y == "yes") * 100, 2), "%\n\n",
    "Die kompletten Datenpunkte der Ausreißer mit allen Variablen wurden in der folgenden CSV-Datei gespeichert: ",
    "[balance_outliers.csv](../source/analytics_output/balance_outliers.csv).\n\n",
    "#### Visualisierung der Ausreißer-Analyse:\n\n",
    "![Visualisierung der Ausreißer-Analyse](../source/analytics_output/balance_outliers_analysis.png)\n\n",
    "#### Erste 10 Ausreißer (sortiert nach höchstem Guthaben):\n\n"
  ),
  file = report_file,
  append = TRUE
)

# Erstelle Markdown-Tabelle der Top 10 Ausreißer
top_10_outliers <- head(outliers, 10)
table_cols <- c("age", "job", "marital", "education", "balance", "housing", "loan", "y")
top_10_sub <- top_10_outliers[, table_cols]

md_table <- "| age | job | marital | education | balance | housing | loan | y |\n"
md_table <- paste0(md_table, "|:---|:---|:---|:---|:---|:---|:---|:---|\n")
for (i in 1:nrow(top_10_sub)) {
  md_table <- paste0(
    md_table, "| ",
    top_10_sub$age[i], " | ",
    top_10_sub$job[i], " | ",
    top_10_sub$marital[i], " | ",
    top_10_sub$education[i], " | ",
    top_10_sub$balance[i], " € | ",
    top_10_sub$housing[i], " | ",
    top_10_sub$loan[i], " | ",
    top_10_sub$y[i], " |\n"
  )
}
cat(paste0(md_table, "\n---\n\n"), file = report_file, append = TRUE)


# ------------------------------------------------------------------------------
# Variable 7: housing (nominal)
# ------------------------------------------------------------------------------
cat(
  "## Variable 7: **housing** (nominal)\n\n",
  file = report_file, append = TRUE
)

# Stats
cat(
  format_nominal_stats("housing", Daten$housing),
  file = report_file, append = TRUE
)
tbl_housing <- table(Daten$housing)
cat(
  "### Häufigkeitstabelle für **housing**\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(
    tbl_housing,
    c("Immobilienkredit?", "Absolut", "Relativ (%)")
  ),
  file = report_file, append = TRUE
)

# Univariates Bild
png(
  file.path(output_dir, "univariate_7_housing.png"),
  width = 600, height = 450
)
par(mar = c(5, 4, 4, 2) + 0.1)
barplot(
  tbl_housing,
  main = "Immobilienkredit? (housing)",
  col = "steelblue", ylab = "Absolute Häufigkeit"
)
dev.off()

# Bivariat mit y
tbl_housing_y <- table(Daten$housing, Daten$y)
cat(
  "\n### Kreuztabelle für **housing** und **y** (Abschluss)\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(tbl_housing_y),
  file = report_file, append = TRUE
)

plot_percentage_barplot(tbl_housing_y, "housing", "bivariate_7_housing_y.png")

# Bewertung
cat(
  paste0(
    "### Einfluss auf die Zielvariable y\n\n",
    "* **Zusammenhangsmaß (Abschlussrate)**: Spanne von ",
    get_conversion_span(Daten, "housing", translate = FALSE), "\n",
    "* **Bewertungssatz**: Das Vorhandensein eines Immobilienkredits ",
    "hat einen starken negativen Einfluss auf den Abschluss. ",
    "Kunden ohne Immobilienkredit schließen mit 16.7% mehr als ",
    "doppelt ",
    "so häufig ab wie Kunden mit Kredit (7.7%).\n\n"
  ),
  file = report_file,
  append = TRUE
)


# ------------------------------------------------------------------------------
# Variable 8: loan (nominal)
# ------------------------------------------------------------------------------
cat(
  "## Variable 8: **loan** (nominal)\n\n",
  file = report_file, append = TRUE
)

# Stats
cat(
  format_nominal_stats("loan", Daten$loan),
  file = report_file, append = TRUE
)
tbl_loan <- table(Daten$loan)
cat(
  "### Häufigkeitstabelle für **loan**\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(
    tbl_loan,
    c("Privatkredit?", "Absolut", "Relativ (%)")
  ),
  file = report_file, append = TRUE
)

# Univariates Bild
png(
  file.path(output_dir, "univariate_8_loan.png"),
  width = 600, height = 450
)
par(mar = c(5, 4, 4, 2) + 0.1)
barplot(
  tbl_loan,
  main = "Privatkredit? (loan)",
  col = "steelblue", ylab = "Absolute Häufigkeit"
)
dev.off()

# Bivariat mit y
tbl_loan_y <- table(Daten$loan, Daten$y)
cat(
  "\n### Kreuztabelle für **loan** und **y** (Abschluss)\n\n",
  file = report_file, append = TRUE
)
cat(
  format_markdown_table(tbl_loan_y),
  file = report_file, append = TRUE
)

plot_percentage_barplot(tbl_loan_y, "loan", "bivariate_8_loan_y.png")

# Bewertung
cat(
  paste0(
    "### Einfluss auf die Zielvariable y\n\n",
    "* **Zusammenhangsmaß (Abschlussrate)**: Spanne von ",
    get_conversion_span(Daten, "loan", translate = FALSE), "\n",
    "* **Bewertungssatz**: Ein Privatkredit hat einen moderaten negativen ",
    "Einfluss auf den Abschluss. ",
    "Kunden ohne Privatkredit schließen fast doppelt so häufig ab (12.7%) ",
    "wie Kunden mit Privatkredit (6.7%).\n"
  ),
  file = report_file,
  append = TRUE
)

cat(
  paste0(
    "Analyse erfolgreich abgeschlossen. Ergebnisse wurden ",
    "in 'source/analytics_output/' gespeichert.\n"
  )
)
