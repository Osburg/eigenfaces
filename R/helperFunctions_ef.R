#Importiere benötigte Libraries
#library(tidyverse)

#Gibt eine Matrix img als Bild in Graustufen (und nicht auf dem Kopf wie image()) aus
#TODO: ?-Eintrag
#TODO: Test
#TODO: Fehler, warnings
imgShow_ef <- function(img) {
  img %>%
    apply(1, rev) %>%
    t() %>%
    image(col=hcl.colors(12, "Grays", rev = FALSE))
}

#Liest eine .csv-Datei der folgenden Struktur ein und gibt sie als n x dimension - Array wieder aus
#Jede Zeile der .csv Datei bezeichnet ein Bild. Die ersten width Pixel bezeichnen die erste Bildzeile, die
#zweiten die zweite Bildzeile ...
#n = Anzahl der Zeilen/Bilder, dimension = Dimension der einzelnen Zeilen (z.B. 64x64 für quad. Bilder)
#TODO: ?-Eintrag
#TODO: Test
#TODO: Fehler, Warnings
csv_to_array_ef <- function(path, dimension = c(64,64)) {
  data  <- read.csv(path)
  nrow <- nrow(data)
  data %>% as.matrix() %>% as.double() %>% as.matrix() -> data
  dim(data) <- c(nrow, dimension)
  data
}

#TODO: Schreibe eine Funktion, die eine Menge von Bildpfaden in eine .csv-Datei umwandelt


#Generische Funktionen zum Speichern von image_ef oder trainingDataset_ef Objekten
to_csv <- function(obj) UseMethod("to_csv")

to_jpeg <- function(obj) UseMethod("to_csv")

to_png <- function(obj) UseMethod("to_csv")


#'Normalization
#'
#' @param obj an object to be normalized (e.g. of class 'image_ef', 'imageset_ef')
#'
normalize <- function(obj) UseMethod("normalize")

#' Feature Space Projection
#'
#' Projects an image or set of images into feature space defined by a set of eigenfaces
#'
#' @param obj an object to be proected on eigenfaces (e.g. of class 'image_ef', 'imageset_ef')
#' @param eigenfaces an object of class 'imageset_ef'. Describes the vectors to project on.
#' @param avgFace an object of class 'image_ef'. Average face of the underlying imageset_ef
#' @param showCoefficients a logical vector of length 1 (TRUE or FALSE). Determines whether to additionally return the coefficients of the linear combination representing obj.
#'
#' For details see FSP.image_ef and FSP.imageset_ef
FSP <- function(obj, eigenfaces, avgFace, showCoefficients = TRUE) UseMethod("FSP", obj)
