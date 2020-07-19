#Beschreibung: In dieser Datei werden Funktionen definiert, die im Hintergrund ablaufen
#und einem das Leben erleichtern sollen.

#Importiere benötigte Libraries
library(tidyverse)

#Gibt eine Matrix img als Bild in Graustufen (und nicht auf dem Kopf wie image()) aus
#TODO: ?-Eintrag
#TODO: Test
#TODO: Fehler, warnings
image_ef <- function(img) {
  img %>%
    apply(1, rev) %>%
    t() %>%
    image(,col=hcl.colors(12, "Grays", rev = FALSE))
}


