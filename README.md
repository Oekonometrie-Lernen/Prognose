Prognose
============

Um die Animation auf Ihrem Rechner auszuführen (_Localhost_), benötigen Sie die R Pakete `shiny`, `ggplot2` und `dplyr`. 

```
# Notwendiges Packet wird installiert
install.packages("shiny")
install.packages("ggplot2")
install.packages("remotes")
install_github("AnalytixWare/ShinySky")
library(shiny)
library(ggplot2)
library(shinysky)
runGitHub("Prognose", "Oekonometrie-Lernen")
```

Erfolgreiches Ausführen dieser Schritte wird Ihnen ermöglichen die Animation lokal auf Ihrem eigenen Rechner laufen zu lassen.   
Ausführliche Informationen wie man eine Anwendung mithilfe des RStudio Shiny Pakets schreibt, finden Sie unter folgendem [Tutorium.](http://shiny.rstudio.com/tutorial/)


