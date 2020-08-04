library(tidyverse)
source("R/helperFunctions_ef.R")
source("R/image_ef.R")

#Lädt .csv-Datei und speichert es als trainingDataset_ef
load_trainingDataset_ef <- function(path, imgDim) {
  #TODO: Baue Funktion ein, um Farbbilder zu laden

  #Lese Daten ein und speichere sie als [n_img, imgDim]-Array
  data  <- read.csv(path)
  nrow <- nrow(data)
  data %>% as.matrix() %>% as.double() %>% as.matrix() -> data
  dim(data) <- c(nrow, imgDim)

  #Erstelle Objekt der Klasse trainingDataset_ef
  td <- list()
  class(td) <- "trainingDataset_ef"

  #Füge die image_ef Objekte in das trainingDataset_ef Objekt ein
  for (i in 1:dim(data)[1]) td[[i]] <- image_ef(data[i,,])

  td
}

#Erstellt neues trainingDataset_ef mit den Werten aus lst
trainingDataset_ef <- function(lst) {
  stopifnot("Eingabe muss eine Liste sein" = is.list(lst))
  #TODO: teste, ob alle image_ef Objekte die gleiche imgDim haben
}

#Testet, ob eine Eingabe von der Klasse trainingDataset_ef ist
is.trainingDataset_ef <- function(td) is.element("trainingDataset_ef", class(td))

#Normalisiert die Gesichter, d.h. es zieht von jedem Pixel den über das Bild gemittelten Pixelwert ab
normalize_faces <- function(td) {
  stopifnot("Eingabe muss ein trainingDataset_ef sein" = is.trainingDataset_ef(td))
  for (i in 1:length(td)) {
    td[[i]] = td[[i]] - sum(td[[i]])/length(td[[i]])
  }
  td
}

#Berechnet das Durchschnittsgesicht
avg_face <- function(td) {
  stopifnot("Eingabe muss ein trainingDataset_ef sein" = is.trainingDataset_ef(td))

  avg <- 0
  for (i in 1:length(td)){
    avg <- avg + td[[i]]
  }
  avg <- avg/length(td)

  avg
}

#Berechnet die Kovarianzmatrix zu einem trainingDataset_ef
cov_ef <- function(td) {

  #Überführe td in eine Gesichtsmatrix
  ncol <- length(td)
  td %>% unlist() %>% matrix(ncol=ncol) -> td


  td
}

