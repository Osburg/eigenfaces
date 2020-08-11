library(tidyverse)
library(pracma)
source("R/helperFunctions_ef.R")
source("R/imageset_ef.R")
source("R/image_ef.R")

#' Projection of a vector onto a unit vector
#'
#' @param x numeric vector or matrix
#' @param y numeric vector or matrix
#' @return scalar product of x with unit vector in y-direction
#' @examples
#' proj(c(1,1,1), c(1,0,0))
proj <- function(x,y) {
  stopifnot("x and y must be of the same length" = length(x)==length(y))
  x <- as.vector(x)
  y <- as.vector(y)

  #Normiere y
  y <- y / sqrt(dot(y,y))

  dot(x,y)
}

#TODO: Es ist bisher nicht sichergestellt, dass die Eigenfaces orthogonal sind (auch wenn es bei Tests stets so war).
#Man sollte zumindest in der PCA sicherstellen, dass die Eigenwerte paarweise verschieden sind, dies würde orthogonalität absichern

#' Projection of an 'image_ef' object onto a set of orthogonal eigenfaces
#'
#' The projection of the vector onto the linear span of the eigenfaces is computed
#' by projecting an 'image_ef' onto every single vector of the set of eigenfaces. A given average
#' face is substracted before the projection and added onto the projection afterwards.
#'
#' @param img an object of class 'image_ef'
#' @param eigenfaces an object of class 'imageset_ef', eigenfaces of an 'imageset_ef' object td
#' @param avgFace an object of class 'image_ef', average face of td
#' @param showCoefficients (optional) logical vector of length 1 (TRUE or FALSE)
#'
#' @return case \code{showCoefficients=FALSE}: a list of length 1 containing an object of class 'image_ef',
#' projection of \code{img} onto the linear span of \code{eigenfaces}
#' case \code{showCoefficients=TRUE}: a list of length 2 additionally containing a numeric vector where the
#' coefficients for the representation with the eigenfaces are saved
#'
#' @examples
#' # Import Olivetti-faces
#' td <- load_imageset_ef("olivetti_X.csv", c(64,64))
#' img <- td[[42]]
#' eigenfaces <- get_eigenfaces(td, nfaces = 50)
#' avgFace <- avg_face(td)
#'
#' projection <- FSP(img, eigenfaces, avgFace, showCoefficients=FALSE)[[1]]
#'
#' @export
FSP.image_ef <- function(img, eigenfaces, avgFace, showCoefficients = FALSE) {
  stopifnot("eigenfaces muss ein imageset_ef sein" = is.imageset_ef(eigenfaces))
  stopifnot("eigenfaces muss mindestens die Länge 1 haben" = length(eigenfaces)>0)
  stopifnot("img muss ein  image_ef sein" = is.image_ef(img))
  stopifnot("img und Elemente von eigenfaces müssen die gleiche Dimension besitzen" = dim(img) == dim(eigenfaces[[1]]))

  #Normalisiere img
  img <- normalize(img)

  #Ziehe Durchschnittsgesicht ab
  img <- img - avgFace

  #Berechne Projektion auf Eigenvektoren
  lambda <- c()
  projFace <- 0
  for (i in 1:length(eigenfaces)){
    lambda <- c(lambda, proj(img, eigenfaces[[i]]))
    projFace <- projFace + lambda[[i]] * eigenfaces[[i]]
  }

  #Addiere das Durchschnittsgesicht wieder auf
  projFace <- projFace + avgFace

  if(showCoefficients) return(list(projFace, lambda))
  else return(list(projFace))
}

#' Projection of an 'imageset_ef' object onto a set of orthogonal eigenfaces
#'
#' applies \code{FSP.image_ef} to every single 'image_ef' object of the 'imageset_ef' object.
#' The projected faces and the coefficients are returned as a list each (combined to another list)
#'
#' @param td an object of class 'imageset_ef'
#' @param eigenfaces an object of class 'imageset_ef', eigenfaces of an 'imageset_ef' object td
#' @param avgFace an object of class 'image_ef', average face of td
#' @param showCoefficients (optional) logical vector of length 1 (TRUE or FALSE)
#'
#' @return case \code{showCoefficients=FALSE}: a list of length 1 containing a list of objects of class 'image_ef',
#' projections of the elements of \code{td} onto the linear span of \code{eigenfaces}
#' case \code{showCoefficients=TRUE}: a list of length 2 additionally containing a list of numeric vectors where the
#' coefficients for the representation with the eigenfaces are saved.
#'
#' @examples
#' # Import Olivetti-faces
#' td <- load_imageset_ef("olivetti_X.csv", c(64,64))
#' eigenfaces <- get_eigenfaces(td, nfaces = 50)
#' avgFace <- avg_face(td)
#'
#' projection <- FSP(td, eigenfaces, avgFace, showCoefficients=FALSE)[[1]]
#'
#' @export
FSP.imageset_ef <- function(td, eigenfaces, avgFace, showCoefficients = FALSE) {
  stopifnot("eigenfaces muss ein imageset_ef sein" = is.imageset_ef(eigenfaces))
  stopifnot("eigenfaces muss mindestens die Länge 1 haben" = length(eigenfaces)>0)
  stopifnot("td muss ein  imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Elemente von td und Elemente von eigenfaces müssen die gleiche Dimension besitzen" = dim(td[[1]]) == dim(eigenfaces[[1]]))
  stopifnot("td muss mindestens die Länge 1 haben" = length(td)>0)

  if (showCoefficients == FALSE) {
    for (i in 1:length(td)) {
      td[[i]] <- FSP(td[[i]], eigenfaces, avgFace, showCoefficients = showCoefficients)[[1]]
    }
    return(list(td))
  }

  else {
    coeffs <- list()
    for (i in 1:length(td)) {
      proj <- FSP(td[[i]], eigenfaces, avgFace, showCoefficients = showCoefficients)
      td[[i]] <- proj[[1]]
      coeffs[[i]] <- proj[[2]]
    }
    return(list(td, coeffs))
  }
}

