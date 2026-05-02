library(dplyr)
library(here)



Daten <- read.csv(here::here("data", "bank-additional-full.csv"), header=TRUE, sep=";", fill=TRUE, stringsAsFactors=TRUE)

## Umbenennen von Spalten
Daten <- rename(Daten,
  relationship=martial,
  subscriped=y
)

## Variablen in Fakoren umwandeln (siehe NotizenZuDenDaten.docx)



## Plots

## Histogramme für alle numerischen Variablen


summary(Daten)
