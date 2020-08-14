#library(tidyverse)
#library(pracma)
# source("R/helperFunctions_ef.R")
# source("R/imageset_ef.R")
# source("R/image_ef.R")


#' Projection of a vector onto a unit vector
#'
#' @param x numeric vector or matrix
#' @param y numeric vector or matrix
#' @return scalar product of x with unit vector in y-direction
#' @export
#' @examples
#' proj(c(1,1,1), c(1,0,0))
proj <- function(x,y) {
  stopifnot("x and y must be of the same length" = length(x)==length(y))
  stopifnot("x must be numeric" = is.numeric(x))
  stopifnot("y must be numeric" = is.numeric(y))

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
#' @param obj an object of class 'image_ef'
#' @param eigenfaces an object of class 'imageset_ef', eigenfaces of an 'imageset_ef' object td
#' @param avgFace an object of class 'image_ef', average face of td
#' @param showCoefficients (optional) logical vector of length 1 (TRUE or FALSE)
#' @param ... additional arguments
#'
#' @return case \code{showCoefficients=FALSE}: a list of length 1 containing an object of class 'image_ef',
#' projection of \code{img} onto the linear span of \code{eigenfaces}
#' case \code{showCoefficients=TRUE}: a list of length 2 additionally containing a numeric vector where the
#' coefficients for the representation with the eigenfaces are saved
#'
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' img <- td[[42]]
#' eigenfaces <- get_eigenfaces(td, nfaces = 50, quick=TRUE)
#' avgFace <- avg_face(td)
#' projection <- FSP(img, eigenfaces, avgFace, showCoefficients=FALSE)[[1]]
#' @export
FSP.image_ef <- function(obj, eigenfaces, avgFace, showCoefficients = FALSE, ...) {
  stopifnot("avgFace must be of class 'iage_ef'" = is.image_ef(avgFace))
  stopifnot("obj must be of class 'image_ef'" = is.image_ef(obj))
  #stopifnot("eigenfaces must be of class 'imageset_ef'" = is.imageset_ef(eigenfaces))
  #stopifnot("obj must be at least of length 1" = length(obj)>0)
  #stopifnot("obj und imageset_ef must have the same dimension" = dim(obj) == dim(obj[[1]]))
  stopifnot("showCoefficients must be logical" = is.logical(showCoefficients))
  stopifnot("showCoefficients must be of length 1" = length(showCoefficients)==1)

  #Normalisiere img
  obj <- normalize(obj)

  #Ziehe Durchschnittsgesicht ab
  obj <- obj - avgFace

  #Berechne Projektion auf Eigenvektoren
  lambda <- c()
  projFace <- 0
  for (i in 1:length(eigenfaces)){
    lambda <- c(lambda, proj(obj, eigenfaces[[i]]))
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
#' @param obj an object of class 'imageset_ef'
#' @param eigenfaces an object of class 'imageset_ef', eigenfaces of an 'imageset_ef' object td
#' @param avgFace an object of class 'image_ef', average face of td
#' @param showCoefficients (optional) logical vector of length 1 (TRUE or FALSE)
#' @param ... additional arguments
#'
#' @return case \code{showCoefficients=FALSE}: a list of length 1 containing a list of objects of class 'image_ef',
#' projections of the elements of \code{td} onto the linear span of \code{eigenfaces}
#' case \code{showCoefficients=TRUE}: a list of length 2 additionally containing a list of numeric vectors where the
#' coefficients for the representation with the eigenfaces are saved.
#'
#' @examples
#' # Import Olivetti-faces
#' olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(olivetti, c(64,64))
#' eigenfaces <- get_eigenfaces(td, nfaces = 50, quick=TRUE)
#' avgFace <- avg_face(td)
#' projection <- FSP(td, eigenfaces, avgFace, showCoefficients=FALSE)[[1]]
#' @export
FSP.imageset_ef <- function(obj, eigenfaces, avgFace, showCoefficients = FALSE, ...) {
  stopifnot("eigenfaces must be of class 'imageset_ef'" = is.imageset_ef(eigenfaces))
  stopifnot("eigenfaces must be at least of length 1" = length(eigenfaces)>0)
  stopifnot("obj must be of class 'imageset_ef'" = is.imageset_ef(obj))
  stopifnot("obj must be at least of length 1" = length(obj)>0)
  stopifnot("showCoefficients must be of length 1" = length(showCoefficients)==1)
  stopifnot("The elements of obj and eigenfaces must be of the same dimension" = dim(obj[[1]]) == dim(eigenfaces[[1]]))

  if (showCoefficients == FALSE) {
    for (i in 1:length(obj)) {
      obj[[i]] <- FSP(obj[[i]], eigenfaces, avgFace, showCoefficients = showCoefficients)[[1]]
    }
    return(list(obj))
  }

  else {
    coeffs <- list()
    for (i in 1:length(obj)) {
      proj <- FSP(obj[[i]], eigenfaces, avgFace, showCoefficients = showCoefficients)
      obj[[i]] <- proj[[1]]
      coeffs[[i]] <- proj[[2]]
    }
    return(list(obj, coeffs))
  }
}
