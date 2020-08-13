#library(tidyverse)
source("R/helperFunctions_ef.R")
source("R/imageset_ef.R")
source("R/image_ef.R")
source("R/FeatureSpaceProjection.R")

#' Euclidean distance between the coefficient vectors of the projection of two images
#'
#' @param coeffs1 numeric vector, first coefficient vector
#' @param coeffs2 numeric vector, second coefficient vector
#'
#' @return numeric vector of length 1, the euclidean distance of \code{coeffs1} and \code{coeffs2}
#' @examples
#' # Import Olivetti-faces
#' td <- load_imageset_ef("../olivetti_X.csv", c(64,64))
#' #compute coefficients
#' eigenfaces <- get_eigenfaces(td, nfaces = 50)
#' avgFace <- avg_face(td)
#' coeffs1 <- FSP(td[[1]], eigenfaces, avgFace, showCoefficients=TRUE)[[2]]
#' coeffs2 <- FSP(td[[2]], eigenfaces, avgFace, showCoefficients=TRUE)[[2]]
#' distance_ef(coeffs1, coeffs2)
distance_ef <- function(coeffs1, coeffs2) {
  stopifnot("coeffs1 must be numeric" = is.numeric(coeffs1))
  stopifnot("coeffs2 must be numeric" = is.numeric(coeffs2))
  stopifnot("Length of coeffs1 and coeffs 2 must be equal" = length(coeffs1) == length(coeffs2))

  #Berechne Differenz
  diff = coeffs1 - coeffs2

  #Berechne die Norm der Differenz
  norm <- sqrt(dot(diff, diff))

  norm
}

#' Find the best matching faces of an 'imageset_ef' for a given 'image_ef'
#'
#' Finds the closest \code{nclosest} images in an object of class 'imageset_ef'. For the determination
#' of the closest images \code{neigenfaces} are used. There are two possible modes (\code{quick = TRUE}, \code{quick = FALSE}).
#' For further explanation see \code{?PCA}.
#'
#' @param img an object of class 'image_ef', the functions searches for similar images to this image
#' @param td an object of class 'imageset_ef', set of images where the function searches for similar images
#' @param nclosest (optional, default: \code{nclosest=3}) an numeric vector of length 1, number of similar images to be ruturned
#' @param neigenfaces (optional, default: \code{neigenfaces=15}) an numeric vector of length 1, number of eigenfaces used for finding the closest faces
#' @param quick logical vector of length  (TRUE or FALSE), see \code{PCA}
#'
#' @return 'imageset_ef' object containing the \code{nclosest} images from \code{td}, which are 'closest' to \code{img}
#'
#' @examples
#' # Import Olivetti-faces
#' td <- load_imageset_ef("../olivetti_X.csv", c(64,64))
#' img <- td[[1]]
#' closest <- classification_ef(img, td)
#'
#' @export
classification_ef <- function(img, td, nclosest = 3, neigenfaces = 15, quick = FALSE) {
  stopifnot("img must be of class 'image_ef'" = is.image_ef(img))
  stopifnot("td must be of class 'imageset_ef'" = is.imageset_ef(td))
  stopifnot("td must be at least of length 1" = length(td)>0)
  stopifnot("img und imageset_ef must have the same dimension" = dim(img) == dim(td[[1]]))
  stopifnot("nclosest must be numeric" = is.numeric(nclosest))
  stopifnot("nclosest must be of length 1" = length(nclosest)==1)
  stopifnot("neigenfaces must be numeric" = is.numeric(neigenfaces))
  stopifnot("neigenfaces must be of length 1" = length(neigenfaces)==1)
  stopifnot("quick must be logical" = is.logical(quick))
  stopifnot("quick must be of length 1" = length(quick)==1)

  #Berechne Eigenfaces (neigenfaces Stück)
  eigenfaces <- get_eigenfaces(td, nfaces = neigenfaces, quick = quick)
  avgFace <- avg_face(normalize(td))

  #Berechne Projektionen
  imgProj <- FSP(img, eigenfaces, avgFace, showCoefficients = TRUE)

  tdProj <- FSP(td, eigenfaces, avgFace, showCoefficients = TRUE)

  #Berechne Distanzen
  dist <- c()
  for (i in 1:length(tdProj[[1]])){
    dist <- c(dist, distance_ef(imgProj[[2]], tdProj[[2]][[i]]))
  }

  class(td) <- NULL

  #Speichere Distanzen und td (Bilder) in einem Tibble
  tb <- tibble("images_ef" = as.list(td), "distances" = dist)

  class(td) <- "imageset_ef"

  #Sortiere tb aufsteigend nach distance
  tb <- arrange(tb, dist)

  #Gebe die ersten nclosest image_efs aus
  nclosest <- min(nclosest, length(td))
  td <- tb[["images_ef"]][1:nclosest]

  imageset_ef(td)
}
