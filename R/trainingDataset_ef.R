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
  #TODO: Gibt allen Einträgen der Liste die gleiche Dimension imgDim <- neuer Parameter der Funktion

  class(lst) <- "trainingDataset_ef"

  lst
}

#Testet, ob eine Eingabe von der Klasse trainingDataset_ef ist
is.trainingDataset_ef <- function(td) is.element("trainingDataset_ef", class(td))

#Normalisiert die Gesichter, d.h. es zieht von jedem Pixel den über das Bild gemittelten Pixelwert ab
normalize_faces <- function(td) {
  stopifnot("Eingabe muss ein trainingDataset_ef sein" = is.trainingDataset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>1)

  for (i in 1:length(td)) {
    td[[i]] = td[[i]] - sum(td[[i]])/length(td[[i]])
  }
  td
}

#Berechnet das Durchschnittsgesicht
avg_face <- function(td) {
  stopifnot("Eingabe muss ein trainingDataset_ef sein" = is.trainingDataset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>1)

  avg <- 0
  for (i in 1:length(td)){
    avg <- avg + td[[i]]
  }
  avg <- avg/length(td)

  avg
}

#Zieht das Durchschnittsgesicht von jedem Gesicht ab
subtract_avg_face <- function(td) {
  stopifnot("Eingabe muss ein trainingDataset_ef sein" = is.trainingDataset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>1)

  #Berechne Durchschnittsgesicht
  avg <- avg_face(td)

  #Ziehe Durchschnittgesicht von jedem Gesicht ab
  for (i in 1:length(td)) td[[i]] <- td[[i]] - avg

  td
}


#Berechnet die Eigenwerte und Vektoren zur Kovarianzmatrix
getEigenfaces <- function(td, nfaces = 15) {
  stopifnot("Eingabe muss ein trainingDataset_ef sein" = is.trainingDataset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>1)

  #Normalisiere die Eigengesichter und ziehe das Durchschnittsgesicht ab
  td %>% normalize_faces() %>% subtract_avg_face() -> td


  #Überführe td in eine Gesichtsmatrix A
  ncol <- length(td)
  td %>% unlist() %>% matrix(ncol=ncol) -> A

  #Berechne die Matrix L = A^T * A
  L <- t(A) %*% A

  #Löse das Eigenwertproblem für L
  eig <- eigen(L)

  #Berechne daraus die ersten cp Eigenvektoren von A * A^T
  cp <- min(length(eig$values), nfaces)
  eigenvals <- eig$values[1:cp]
  eigenvects <- eig$vectors[,1:cp]
  eigenvects <- A %*% eigenvects

  #Erstelle aus den Eigenvektoren ein trainingDataset_ef
  imgDim <- dim(td[[1]])

  eigenfaces <- list()

  for (i in 1:cp) {
    eigenface <- eigenvects[,i]
    dim(eigenface) <- imgDim
    eigenfaces[[i]] <- image_ef(eigenface)
  }

  eigenfaces <- trainingDataset_ef(eigenfaces)

  eigenfaces
}



