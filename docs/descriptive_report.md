# Deskriptiver Analysebericht: Die ersten 8 Variablen des Bank-Marketing Datensatzes

Dieses Dokument enthält die deskriptive Analyse für die ersten 8 Variablen gemäß der Methodik aus den Vorlesungsfolien (Kapitel 2).

### Übersicht: Einfluss der Variablen auf y (auf einen Blick)

Die folgende Tabelle gibt eine Übersicht über den statistischen Einfluss der ersten 8 Variablen auf die Zielvariable **y** (Abschluss). Gemäß Kapitel 2 der Vorlesungsfolien wird dies für kategoriale Variablen über die Spanne der Abschlussraten (aus den Kreuztabellen) und für metrische Variablen über die Korrelationskoeffizienten beschrieben:

| Variable | Skalenniveau | Statistisches Maß (aus Folien) | Erläuterung des Effekts |
|:---|:---|:---|:---|
| **housing** | nominal | Abschlussrate: 7.7% (ja) bis 16.7% (nein) | Deutlicher negativer Einfluss bei bestehendem Kredit |
| **job** | nominal | Abschlussrate: 7.3% (blue-collar) bis 28.7% (student) | Starker Einfluss (Studenten/Rentner schließen oft ab) |
| **education** | ordinal | Abschlussrate: 8.6% (primary) bis 15.0% (tertiary) | Moderater positiver Einfluss mit steigender Bildung |
| **loan** | nominal | Abschlussrate: 6.7% (ja) bis 12.7% (nein) | Spürbarer negativer Einfluss bei bestehendem Privatkredit |
| **marital** | nominal | Abschlussrate: 10.1% (married) bis 14.9% (single) | Moderater Einfluss (Singles schließen am häufigsten ab) |
| **balance** | metrisch | Pearson-R: 0.0528 / Spearman: 0.1003 | Geringer positiver Einfluss (Guthaben-Median ist höher) |
| **age** | metrisch | Pearson-R: 0.0252 / Spearman: -0.0087 | Minimaler linearer Korrelationseffekt (U-Verlauf vorhanden) |
| **default** | nominal | Abschlussrate: 6.4% (ja) bis 11.8% (nein) | Sehr geringer Einfluss (Verzugskunden schließen seltener ab) |

---

## Variable 1: **age** (metrisch)

### Metrische Kennzahlen für **age**

| Kennzahl | Wert |
|:---|:---|
| Mittelwert | 40.94 |
| Median | 39 |
| Standardabweichung | 10.62 |
| MAD (Robust) | 10.38 |
| Interquartilsabstand (IQR) | 15 |
| Minimum | 18 |
| 25%-Quantil | 33 |
| 75%-Quantil | 48 |
| Maximum | 95 |

### Zusammenhang zwischen **age** und **y** (Abschluss)

| Kennzahl | y = no | y = yes | Gesamt |
|:---|:---|:---|:---|
| Minimum | 18 | 18 | 18 |
| 25%-Quantil | 33 | 31 | 33 |
| Median | 39 | 38 | 39 |
| Mittelwert | 40.84 | 41.67 | 40.94 |
| 75%-Quantil | 48 | 50 | 48 |
| Maximum | 95 | 95 | 95 |
| Standardabweichung | 10.17 | 13.5 | 10.62 |

### Einfluss auf die Zielvariable y

* **Lineare Korrelation (Pearson-R)**: 0.0252
* **Rangkorrelation (Spearman-R)**: -0.0087
* **Bewertungssatz**: Das Alter hat einen minimalen linearen Einfluss auf den Abschluss (Pearson-R: 0.0252). Der Mittelwert der Kunden, die abschließen, liegt geringfügig höher, aber der Median ist leicht niedriger. Die geringe lineare Korrelation ist jedoch irreführend, da ein nicht-linearer U-Verlauf vorliegt: Sehr junge Kunden (<25 Jahre) und ältere Kunden (65+ Jahre) haben eine deutlich höhere Abschlussrate (siehe Grafik in v2).

## Variable 2: **job** (nominal)

### Nominale Kennzahlen für **job**

| Kennzahl | Wert |
|:---|:---|
| Modus (Häufigster Wert) | blue-collar |

### Häufigkeitstabelle für **job**

| Berufsgruppe | Absolut | Relativ (%) |
|:---|:---|:---|
| admin. | 5171 | 11.44% |
| blue-collar | 9732 | 21.53% |
| entrepreneur | 1487 | 3.29% |
| housemaid | 1240 | 2.74% |
| management | 9458 | 20.92% |
| retired | 2264 | 5.01% |
| self-employed | 1579 | 3.49% |
| services | 4154 | 9.19% |
| student | 938 | 2.07% |
| technician | 7597 | 16.8% |
| unemployed | 1303 | 2.88% |
| unknown | 288 | 0.64% |

### Kreuztabelle für **job** und **y** (Abschluss)

| Kategorie \ y | no | yes | Gesamt |
|:---|---|---|---|
| **admin.** | 4540 (87.8%) | 631 (12.2%) | 5171 |
| **blue-collar** | 9024 (92.7%) | 708 (7.3%) | 9732 |
| **entrepreneur** | 1364 (91.7%) | 123 (8.3%) | 1487 |
| **housemaid** | 1131 (91.2%) | 109 (8.8%) | 1240 |
| **management** | 8157 (86.2%) | 1301 (13.8%) | 9458 |
| **retired** | 1748 (77.2%) | 516 (22.8%) | 2264 |
| **self-employed** | 1392 (88.2%) | 187 (11.8%) | 1579 |
| **services** | 3785 (91.1%) | 369 (8.9%) | 4154 |
| **student** | 669 (71.3%) | 269 (28.7%) | 938 |
| **technician** | 6757 (88.9%) | 840 (11.1%) | 7597 |
| **unemployed** | 1101 (84.5%) | 202 (15.5%) | 1303 |
| **unknown** | 254 (88.2%) | 34 (11.8%) | 288 |
### Einfluss auf die Zielvariable y

* **Zusammenhangsmaß (Abschlussrate)**: Spanne von 7.3% (blue-collar) bis 28.7% (student)
* **Bewertungssatz**: Der Beruf hat einen deutlichen Einfluss auf den Abschluss. Insbesondere Studenten (28.7% Erfolgsrate) und Rentner (22.8% Erfolgsrate) schließen überdurchschnittlich häufig Festgelder ab, während Arbeiter ('blue-collar', 7.3%) und Unternehmer ('entrepreneur', 8.3%) sehr niedrige Raten aufweisen.

## Variable 3: **marital** (nominal)

### Nominale Kennzahlen für **marital**

| Kennzahl | Wert |
|:---|:---|
| Modus (Häufigster Wert) | married |

### Häufigkeitstabelle für **marital**

| Familienstand | Absolut | Relativ (%) |
|:---|:---|:---|
| divorced | 5207 | 11.52% |
| married | 27214 | 60.19% |
| single | 12790 | 28.29% |

### Kreuztabelle für **marital** und **y** (Abschluss)

| Kategorie \ y | no | yes | Gesamt |
|:---|---|---|---|
| **divorced** | 4585 (88.1%) | 622 (11.9%) | 5207 |
| **married** | 24459 (89.9%) | 2755 (10.1%) | 27214 |
| **single** | 10878 (85.1%) | 1912 (14.9%) | 12790 |
### Einfluss auf die Zielvariable y

* **Zusammenhangsmaß (Abschlussrate)**: Spanne von 10.1% (married) bis 14.9% (single)
* **Bewertungssatz**: Der Familienstand hat einen moderaten Einfluss auf den Abschluss. Singles haben mit 14.9% die höchste Abschlussquote, gefolgt von Geschiedenen (11.9%) und Verheirateten (10.1%).

## Variable 4: **education** (ordinal)

### Ordinale Kennzahlen für **education**

| Kennzahl | Kategorie |
|:---|:---|
| Modus (Häufigster Wert) | secondary |
| 25%-Quantil | secondary |
| Median (50%-Quantil) | secondary |
| 75%-Quantil | tertiary |

### Häufigkeitstabelle für **education** (inkl. NA / unknown)

| Bildungsstand | Absolut | Relativ (%) |
|:---|:---|:---|
| primary | 6851 | 15.15% |
| secondary | 23202 | 51.32% |
| tertiary | 13301 | 29.42% |
| NA | 1857 | 4.11% |

### Kreuztabelle für **education** und **y** (Abschluss, ohne NA)

| Kategorie \ y | no | yes | Gesamt |
|:---|---|---|---|
| **primary** | 6260 (91.4%) | 591 (8.6%) | 6851 |
| **secondary** | 20752 (89.4%) | 2450 (10.6%) | 23202 |
| **tertiary** | 11305 (85%) | 1996 (15%) | 13301 |
### Einfluss auf die Zielvariable y

* **Zusammenhangsmaß (Abschlussrate)**: Spanne von 8.6% (primary) bis 15.0% (tertiary)
* **Rangkorrelation (Spearman-R)**: 0.0721
* **Bewertungssatz**: Der Bildungsstand hat einen moderaten positiven Einfluss auf den Abschluss (Spearman-R: 0.0721). Es zeigt sich ein linearer Trend: Kunden mit höherer Bildung schließen häufiger ab (Tertiär: 15.0%, Sekundär: 10.6%, Primär: 8.6%).

## Variable 5: **default** (nominal)

### Nominale Kennzahlen für **default**

| Kennzahl | Wert |
|:---|:---|
| Modus (Häufigster Wert) | no |

### Häufigkeitstabelle für **default**

| Kredit im Verzug? | Absolut | Relativ (%) |
|:---|:---|:---|
| no | 44396 | 98.2% |
| yes | 815 | 1.8% |

### Kreuztabelle für **default** und **y** (Abschluss)

| Kategorie \ y | no | yes | Gesamt |
|:---|---|---|---|
| **no** | 39159 (88.2%) | 5237 (11.8%) | 44396 |
| **yes** | 763 (93.6%) | 52 (6.4%) | 815 |
### Einfluss auf die Zielvariable y

* **Zusammenhangsmaß (Abschlussrate)**: Spanne von 6.4% (yes) bis 11.8% (no)
* **Bewertungssatz**: Ein Zahlungsverzug hat einen minimalen Einfluss. Kunden mit Zahlungsverzug schließen zwar seltener ab (6.4% vs. 11.8%), aber wegen des extrem geringen Anteils betroffener Kunden (1.8% des Datensatzes) hat diese Variable eine sehr geringe Gesamtbedeutung.

## Variable 6: **balance** (metrisch)

### Metrische Kennzahlen für **balance**

| Kennzahl | Wert |
|:---|:---|
| Mittelwert | 1362.27 |
| Median | 448 |
| Standardabweichung | 3044.77 |
| MAD (Robust) | 664.2 |
| Interquartilsabstand (IQR) | 1356 |
| Minimum | -8019 |
| 25%-Quantil | 72 |
| 75%-Quantil | 1428 |
| Maximum | 102127 |

### Zusammenhang zwischen **balance** und **y** (Abschluss)

| Kennzahl | y = no | y = yes | Gesamt |
|:---|:---|:---|:---|
| Minimum | -8019 | -3058 | -8019 |
| 25%-Quantil | 58 | 210 | 72 |
| Median | 417 | 733 | 448 |
| Mittelwert | 1303.71 | 1804.27 | 1362.27 |
| 75%-Quantil | 1345 | 2159 | 1428 |
| Maximum | 102127 | 81204 | 102127 |
| Standardabweichung | 2974.2 | 3501.1 | 3044.77 |

### Einfluss auf die Zielvariable y

* **Lineare Korrelation (Pearson-R)**: 0.0528
* **Rangkorrelation (Spearman-R)**: 0.1003
* **Bewertungssatz**: Das Guthaben hat einen geringen, aber spürbaren positiven Einfluss auf den Abschluss (Pearson-R: 0.0528, Spearman-R: 0.1003). Kunden, die abschließen, haben ein deutlich höheres Durchschnittsguthaben (Mittelwert 1804.27 € vs. 1303.71 €) und einen fast doppelt so hohen Median (733 € vs. 417 €).

## Variable 7: **housing** (nominal)

### Nominale Kennzahlen für **housing**

| Kennzahl | Wert |
|:---|:---|
| Modus (Häufigster Wert) | yes |

### Häufigkeitstabelle für **housing**

| Immobilienkredit? | Absolut | Relativ (%) |
|:---|:---|:---|
| no | 20081 | 44.42% |
| yes | 25130 | 55.58% |

### Kreuztabelle für **housing** und **y** (Abschluss)

| Kategorie \ y | no | yes | Gesamt |
|:---|---|---|---|
| **no** | 16727 (83.3%) | 3354 (16.7%) | 20081 |
| **yes** | 23195 (92.3%) | 1935 (7.7%) | 25130 |
### Einfluss auf die Zielvariable y

* **Zusammenhangsmaß (Abschlussrate)**: Spanne von 7.7% (yes) bis 16.7% (no)
* **Bewertungssatz**: Das Vorhandensein eines Immobilienkredits hat einen starken negativen Einfluss auf den Abschluss. Kunden ohne Immobilienkredit schließen mit 16.7% mehr als doppelt so häufig ab wie Kunden mit Kredit (7.7%).

## Variable 8: **loan** (nominal)

### Nominale Kennzahlen für **loan**

| Kennzahl | Wert |
|:---|:---|
| Modus (Häufigster Wert) | no |

### Häufigkeitstabelle für **loan**

| Privatkredit? | Absolut | Relativ (%) |
|:---|:---|:---|
| no | 37967 | 83.98% |
| yes | 7244 | 16.02% |

### Kreuztabelle für **loan** und **y** (Abschluss)

| Kategorie \ y | no | yes | Gesamt |
|:---|---|---|---|
| **no** | 33162 (87.3%) | 4805 (12.7%) | 37967 |
| **yes** | 6760 (93.3%) | 484 (6.7%) | 7244 |
### Einfluss auf die Zielvariable y

* **Zusammenhangsmaß (Abschlussrate)**: Spanne von 6.7% (yes) bis 12.7% (no)
* **Bewertungssatz**: Ein Privatkredit hat einen moderaten negativen Einfluss auf den Abschluss. Kunden ohne Privatkredit schließen fast doppelt so häufig ab (12.7%) wie Kunden mit Privatkredit (6.7%).
