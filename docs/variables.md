# Bank-Marketing Datensatz: Variablenübersicht

Diese Übersicht beschreibt die Variablen der Bank-Marketing-Kampagne.

## Eingabevariablen (Input)

| ID | Variable | Skalenniveau | Beschreibung / Ausprägungen |
|:---|:---|:---|:---|
| 1 | **age** | metrisch | Alter des Kunden |
| 2 | **job** | nominal | Beruf: Typ des Jobs (admin, blue-collar, entrepreneur, etc.) |
| 3 | **marital** | nominal | Familienstand (married, divorced, single) |
| 4 | **education** | ordinal | Bildungsstand (primary < secondary < tertiary) |
| 5 | **default** | nominal | Kredit im Verzug? (yes, no) |
| 6 | **balance** | metrisch | Durchschnittliches jährliches Guthaben (in Euro) |
| 7 | **housing** | nominal | Immobilienkredit? (yes, no) |
| 8 | **loan** | nominal | Privatkredit? (yes, no) |
| 9 | **contact** | nominal | Kontakt-Kommunikation (cellular, telephone, unknown) |
| 10 | **day** | metrisch | Letzter Kontakttag des Monats (1-31) |
| 11 | **month** | ordinal | Letzter Kontaktmonat (jan < feb < ... < dec) |
| 12 | **duration** | metrisch | Dauer des letzten Kontakts (in Sekunden) |
| 13 | **campaign** | metrisch | Anzahl der Kontakte während dieser Kampagne |
| 14 | **pdays** | metrisch | Tage seit dem letzten Kontakt (-1 = kein Kontakt) |
| 15 | **previous** | metrisch | Anzahl der Kontakte vor dieser Kampagne |
| 16 | **poutcome** | nominal | Ergebnis der vorherigen Kampagne (success, failure, other, unknown) |

## Zielvariable (Output)

| ID | Variable | Skalenniveau | Beschreibung |
|:---|:---|:---|:---|
| 17 | **y** | nominal | Festgeld abgeschlossen? (yes, no) |
