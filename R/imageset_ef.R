library(tidyverse)
source("R/helperFunctions_ef.R")
source("R/image_ef.R")

#Lädt .csv-Datei und speichert es als imageset_ef
load_imageset_ef <- function(path, imgDim) {
  #TODO: Baue Funktion ein, um Farbbilder zu laden

  #Lese Daten ein und speichere sie als [n_img, imgDim]-Array
  data  <- read.csv(path)
  nrow <- nrow(data)
  data %>% as.matrix() %>% as.double() %>% as.matrix() -> data
  dim(data) <- c(nrow, imgDim)

  #Erstelle Objekt der Klasse imageset_ef
  td <- list()
  class(td) <- "imageset_ef"

  #Füge die image_ef Objekte in das imageset_ef Objekt ein
  for (i in 1:dim(data)[1]) td[[i]] <- image_ef(data[i,,])

  td
}

#Erstellt neues imageset_ef_ef mit den Werten aus lst
imageset_ef <- function(lst) {
  stopifnot("Eingabe muss eine Liste sein" = is.list(lst))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(lst)>0)

  #TODO: teste, ob alle image_ef Objekte die gleiche imgDim haben
  #TODO: Gibt allen Einträgen der Liste die gleiche Dimension imgDim <- neuer Parameter der Funktion

  #Wandelt Listenelemente in image_ef Objekte um
  for (i in 1:length(lst)) {
    lst[[i]] <- image_ef(lst[[i]])
  }

  #Erzeugt Klassenattribut
  class(lst) <- "imageset_ef"

  lst
}

#Testet, ob eine Eingabe von der Klasse imageset_ef_ef ist
is.imageset_ef <- function(td) is.element("imageset_ef", class(td))

#Normalisiert die Gesichter, d.h. es zieht von jedem Pixel den über das Bild gemittelten Pixelwert ab
normalize_faces <- function(td) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>0)

  for (i in 1:length(td)) {
    td[[i]] = td[[i]] - sum(td[[i]])/length(td[[i]])
  }
  td
}

#Berechnet das Durchschnittsgesicht
avg_face <- function(td) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>0)

  avg <- 0
  for (i in 1:length(td)){
    avg <- avg + td[[i]]
  }
  avg <- avg/length(td)

  avg
}

#Zieht das Durchschnittsgesicht von jedem Gesicht ab
subtract_avg_face <- function(td) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>0)

  #Berechne Durchschnittsgesicht
  avg <- avg_face(td)

  #Ziehe Durchschnittgesicht von jedem Gesicht ab
  for (i in 1:length(td)) td[[i]] <- td[[i]] - avg

  td
}

#Hauptkomponentenanalyse für Bilddateien (die Eigenvektoren werden als image_ef Objekte zurückgegeben)
PCA <- function(td, showEigenvals = TRUE) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>0)

  #Überführe td in eine Gesichtsmatrix A
  ncol <- length(td)
  td %>% unlist() %>% matrix(ncol=ncol) -> A

  #Berechne die Matrix L = A^T * A
  L <- t(A) %*% A

  #Löse das Eigenwertproblem für L
  eig <- eigen(L)

  #Berechne daraus die Eigenvektoren von A * A^T
  eigenvals <- eig$values
  eigenvects <- A %*% eig$vectors

  #Überführe die Eigenvektoren in ein imageset_ef Objekt
  imgDim <- dim(td[[1]])

  eigenfaces <- list()

  for (i in 1:ncol(eigenvects)) {
    eigenface <- eigenvects[,i]
    dim(eigenface) <- imgDim
    eigenfaces[[i]] <- image_ef(eigenface)
  }

  eigenfaces <- imageset_ef(eigenfaces)

  if (showEigenvals) return(list(eigenfaces, eigenvals))
  else return(list(eigenfaces))
}


#Berechnet die Eigenwerte und Vektoren zur Kovarianzmatrix
getEigenfaces <- function(td, nfaces = 15) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>0)

  #Normalisiere die Eigengesichter und ziehe das Durchschnittsgesicht ab
  td %>% normalize_faces() %>% subtract_avg_face() -> td

  #Führe die Hauptkomponentenanalyse durch und entnehmen nur die ersten nfaces Eigenfaces
  lst <- PCA(td, showEigenvals = FALSE)
  eigenfaces <- lst[[1]]
  nfaces <- min(nfaces, length(eigenfaces))
  eigenfaces <- eigenfaces[1:nfaces]

  imageset_ef(eigenfaces)
}


eigenfaces <- getEigenfaces(td, 15)
class(eigenfaces)
