# R-Script Befehle die wir benutzt haben

### Installation von Paketen

```R
install.packages("dplyr")
install.packages("here")

```

### Einbinden von Paketen

```R
library(dplyr)
library(here)
```

### Installationspakete in renv.lock vermerken

```R
renv::snapshot()
```

### Bibliotheken von renv.lock wiederherstellen/installieren

```R
renv::restore()
```
