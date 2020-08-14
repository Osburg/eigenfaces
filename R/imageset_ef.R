#library(tidyverse)
# source("R/helperFunctions_ef.R")
# source("R/image_ef.R")

#' Load csv-File and Create Imageset_ef
#'
#' Loads csv-file and saves it as imageset_ef. Imageset_ef will be a list of
#' multiple imgDim x imgDim arrays.
#'
#' @param path Path to dataset. Needs to be in csv-format.
#' @param imgDim Tupel. Two dimensional vector to indicate dimension of seperate
#' images.
#'
#' @return Returns dataset as \code{imageset_ef} object.
#' @export
#'
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
load_imageset_ef <- function(path, imgDim) {
  stopifnot("imgDim must be numeric" = is.numeric(imgDim))
  stopifnot("imgDim must be of length 2" = length(imgDim)==2)
  stopifnot("imgDim must be atomic" = is.atomic(imgDim))

  #Lese Daten ein und speichere sie als [n_img, imgDim]-Array
  data  <- read.csv(path, header=FALSE)
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
#' olivetti <- system.file("extdata","olivetti_y.csv",package="eigenfaces")
#' classes <- load_classes_ef(olivetti)
load_classes_ef <- function(path) {
  data <- read.csv(path, header=FALSE)
  data
}

#' Creates an Object of class 'imageset_ef'
#'
#' @param lst list of objects capable of being used as input for \code{image_ef()} function.
#' @return object of class 'imageset_ef',a list consisting of objects of class 'image_ef'.
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' # Normalize
#' normalize(td)
#' @export
imageset_ef <- function(lst) {
  stopifnot("lst must be a list" = is.list(lst))
  stopifnot("lst must be at least of length 1" = length(lst)>0)
  stopifnot("lst must be numeric" = all(sapply(lst, is.numeric)))

  #Wandelt Listenelemente in image_ef Objekte um
  for (i in 1:length(lst)) {
    lst[[i]] <- image_ef(lst[[i]])
  }

  #Teste, ob alle image_ef Objekte die gleiche Dimension haben
  testdim <- dim(lst[[1]])
  for (i in 1:length(lst)) {
    stopifnot("dimension must be the same for all images" = dim(lst[[i]]) == testdim)
  }

  #Erzeugt Klassenattribut
  class(lst) <- "imageset_ef"

  lst
}

#' Tests if an object is of class 'imageset_ef'
#'
#' @param td An object to be tested
#' @return logical vector of legnth 1, (TRUE or FALSE)
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' # Test
#' is.imageset_ef(td)
#'
#' is.imageset_ef(c(1,2,3,4))
#' @export
is.imageset_ef <- function(td) {
  if (is.element("imageset_ef", class(td)) && is.list(td))
    if (length(td) == 0) return(TRUE)
    else {
      for (i in 1:length(td)) {
        if (is.image_ef(td[[i]])==FALSE) return(FALSE)
        else {
          if (!identical(dim(td[[i]]), dim(td[[1]]))) return(FALSE)
        }
      }
      return(TRUE)
    }
  else return(FALSE)
}

#' Normalization of an imageset_ef
#'
#' Normalizes all entries in the imageset_ef list.
#'
#' @param obj List of arrays. Training data.
#' @param ... additional arguments
#'
#' @return Returns normalized version of td.
#' @export
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' # Normalize
#' normalize(td)
normalize.imageset_ef <- function(obj, ...) {
  stopifnot("obj must be of class 'imageset_ef'" = is.imageset_ef(obj))
  stopifnot("obj must be at least of length 1" = length(obj)>0)

  for (i in 1:length(obj)) {
    obj[[i]] <- normalize(obj[[i]])
  }
  obj
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
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' # Get average face
#' avg_face(td)
avg_face <- function(td) {
  stopifnot("td must be of class 'imageset_ef'" = is.imageset_ef(td))
  stopifnot("td must be at least of length 1" = length(td)>0)

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
#' Makes use of avg_face() function to subtract the average data (here: face)
#' of a given data set.
#'
#' @param td List of arrays. Training data.
#'
#' @return Returns td - average face.
#' @export
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' # Subtract
#' subtract_avg_face(td)
subtract_avg_face <- function(td) {
  stopifnot("td must be of class 'imageset_ef'" = is.imageset_ef(td))
  stopifnot("td must be at least of length 1" = length(td)>0)

  #Berechne Durchschnittsgesicht
  avg <- avg_face(td)

  #Ziehe Durchschnittgesicht von jedem Gesicht ab
  for (i in 1:length(td)) td[[i]] <- td[[i]] - avg

  td
}

#' Performs principle component analysis for image files
#'
#' Calculates the data covariance matrix of the original data.
#' Returns eigenvectors (and eigenvalues) of the covariance matrix as an 'imageset_ef' object.
#'
#'\code{td} is the 'imageset_ef'-object where the images are saved as 'image_ef' objects. \code{showEigenvals} determins
#' whether the eigenvalues are returned in addition to the eigenvectors (FALSE means only the eigenvectors are returned).
#' When the number of pixels of each images is much bigger than the number of images in \code{td} it is faster to diagonalize \code{t(A) %*% A}
#' instead of the covariance matrix. However, this causes that only a subset of the eigenvectors of the covariance matrix is returned.
#' If quick is set TRUE, this option is activated.
#'
#' @param td an object of class 'imageset_ef', training data.
#' @param showEigenvals logical vector (TRUE or FALSE).
#' @param quick logical vector (TRUE or FALSE).
#'
#' @return list of length 1 (when showEigenvals <- FALSE; contains an 'imageset_ef' object consisting of the eigenvectors as 'image_ef' objects) or 2 (when showEigenvals <- TRUE; additionally contains a list of the eigenvalues).
#' @references Marinovsky F., Wagner P., Gesichtserkennung mit Eigenfaces, FH Zittau/Görlitz
#' @export
#'
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' PCA(td, showEigenvals = FALSE, quick=TRUE)
PCA <- function(td, showEigenvals = TRUE, quick = FALSE) {
  force(quick)
  stopifnot("td must be of class 'imageset_ef'" = is.imageset_ef(td))
  stopifnot("td must be at least of length 1" = length(td)>0)
  #stopifnot("showEigenvals must be logical" = is.logical(showEigenvals))
  stopifnot("showEigenvals must be of length 1" = length(showEigenvals)==1)
  stopifnot("quick must be logical" = is.logical(quick))
  stopifnot("quick must be of length 1" = length(quick)==1)

  #Überführe td in eine Gesichtsmatrix A
  ncol <- length(td)
  td %>% unlist() %>% matrix(ncol=ncol) -> A

  if (quick) {
    #Berechne die Matrix L = A^T * A
    L <- t(A) %*% A


    #Löse das Eigenwertproblem für L
    eig <- eigen(L)

    #Berechne daraus die Eigenvektoren von A * A^T
    eigenvals <- eig$values
    eigenvects <- A %*% eig$vectors
  }

  else {
    #Berechne die Matrix L = A^T * A
    L <- A %*% t(A)

    #Löse das Eigenwertproblem für L
    eig <- eigen(L)

    #Berechne daraus die Eigenvektoren von A * A^T
    eigenvals <- eig$values
    eigenvects <- eig$vectors
  }

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

#' Calculate the eigenvectors and eigenvalues of the covariance matrix
#'
#' Makes use of the PCA function to perform the principle component analysis.
#' The data is normalized before performing the PCA.
#'
#' @param td an object of class 'imageset_ef'. List of arrays. Training data.
#' @param nfaces a numeric vector of length 1. The desired number of eigenfaces.
#' @param quick a logical vector of length 1. Determines if the PCA is done with 'quick=TRUE' or 'quick=FALSE'.
#' @return Returns n=1,...,nfaces Eigenfaces.
#'
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' get_eigenfaces(td, 9)
#' PCA(td,showEigenvals=FALSE, quick=TRUE)
#' @references Marinovsky F., Wagner P., Gesichtserkennung mit Eigenfaces, FH Zittau/Görlitz
#' @export
get_eigenfaces <- function(td, nfaces = 15, quick = FALSE) {
  force(quick)
  stopifnot("td must be of class 'imageset_ef'" = is.imageset_ef(td))
  stopifnot("td must be at least of length 1" = length(td)>0)
  stopifnot("nfaces must be numeric" = is.numeric(15))
  stopifnot("nfaces must be of length 1" = length(nfaces)==1)
  stopifnot("quick must be logical" = is.logical(quick))
  stopifnot("quick must be of length 1" = length(quick)==1)

  #Normalisiere die Eigengesichter und ziehe das Durchschnittsgesicht ab
  td %>% normalize() %>% subtract_avg_face() -> td

  #Führe die Hauptkomponentenanalyse durch und entnehmen nur die ersten nfaces Eigenfaces
  lst <- PCA(td, showEigenvals = FALSE, quick)
  eigenfaces <- lst[[1]]
  nfaces <- min(nfaces, length(eigenfaces))
  eigenfaces <- eigenfaces[1:nfaces]

  imageset_ef(eigenfaces)
}
