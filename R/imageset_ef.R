library(tidyverse)
source("R/helperFunctions_ef.R")
source("R/image_ef.R")


#' Load csv-File and Create Imageset_ef
#'
#' Loads csv-file and saves it as imageset_ef. Imageset_ef will be a list of
#' multiple imgDim x imgDim arrays.
#'
#' @param path Path to dataset. Needs to be in csv-format.
#' @param imgDim Tupel. Two dimensional vector to indicate dimension of seperate
#' images.
#'
#' @return Returns dataset as list.
#' @export
#'
#' @examples
#' # Import Olivetti-faces
#' td <- load_imageset_ef("olivetti_X.csv", c(64,64))
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


#' Load csv-File and Create Array
#'
#' Loads csv-file and creates an array containing the respective class labels of
#' the dataset.
#'
#' @param path Path to dataset. Needs to be in csv-format.
#'
#' @return Returns 1D-array.
#' @export
#'
#' @examples
#' # Load classes of Olivetti-Dataset
#' classes <- load_classes_ef("olivetti_y.csv")
load_classes_ef <- function(path) {
  data <- read.csv(path)
  data
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


#' Normalize
#'
#' Normalizes all entries in the imageset_ef list.
#'
#' @param td List of arrays. Training data.
#'
#' @return Returns normalized version of td.
#' @export
#'
#' @examples
#' # Import Olivetti-faces
#' td <- load_imageset_ef("olivetti_X.csv", c(64,64))
#' # Normalize
#' normalize(td)
normalize.imageset_ef <- function(td) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>0)

  for (i in 1:length(td)) {
    td[[i]] <- normalize(td[[i]])
  }
  td
}


#' Calculate Average Face
#'
#' Calculates average data (here: face) of the given dataset td.
#'
#' @param td List of arrays. Training data.
#'
#' @return Returns average face as array.
#' @export
#'
#' @examples
#' # Import Olivetti-faces
#' td <- load_imageset_ef("olivetti_X.csv", c(64,64))
#' # Get average face
#' avg_face(td)
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
#' Subtract Average Face
#'
#' Makes use of avg_face() function to subtract the averagge data (here: face)
#' of a given data set.
#'
#' @param td List of arrays. Training data.
#'
#' @return Returns td - average face.
#' @export
#'
#' @examples
#' # Import Olivetti-faces
#' td <- load_imageset_ef("olivetti_X.csv", c(64,64))
#' # Subtract
#' subtract_avg_face(td)
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
  #L <- t(A) %*% A
  L <- A %*% t(A)

  #Löse das Eigenwertproblem für L
  eig <- eigen(L)

  #Berechne daraus die Eigenvektoren von A * A^T
  eigenvals <- eig$values
  #eigenvects <- A %*% eig$vectors
  eigenvects <- eig$vectors

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
get_eigenfaces <- function(td, nfaces = 15) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Eingabe muss mindestens die Länge 1 haben" = length(td)>0)

  #Normalisiere die Eigengesichter und ziehe das Durchschnittsgesicht ab
  td %>% normalize() %>% subtract_avg_face() -> td

  #Führe die Hauptkomponentenanalyse durch und entnehmen nur die ersten nfaces Eigenfaces
  lst <- PCA(td, showEigenvals = FALSE)
  eigenfaces <- lst[[1]]
  nfaces <- min(nfaces, length(eigenfaces))
  eigenfaces <- eigenfaces[1:nfaces]

  imageset_ef(eigenfaces)
}

