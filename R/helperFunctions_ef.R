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

#Liest eine .csv-Datei der folgenden Struktur ein und gibt sie als n x width x height - Array wieder aus
#Jede Zeile der .csv Datei bezeichnet ein Bild. Die ersten width Pixel bezeichnen die erste Bildzeile, die
#zweiten die zweite Bildzeile ...
#n = Anzahl der Zeilen/Bilder, height = Höhe der Bilder, width = Breite der Bilder
#TODO: ?-Eintrag
#TODO: Test
#TODO: Fehler, Warnings
csv_to_array_ef <- function(path, width = 64, height = 64) {
  data  <- read.csv(path)
  nrow <- nrow(data)
  data %>% as.matrix() %>% as.double() %>% as.matrix() -> data
  dim(data) <- c(nrow, width, height)
  data
}
