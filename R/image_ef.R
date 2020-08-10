library(tidyverse)
source("R/helperFunctions_ef.R")

<<<<<<< HEAD
#' Object image_ef
#'
#' Creates objects of class image_ef.
#' They are printed as images in grayscale.
#' is.image_ef is more a general test of an object being interpretable as an image.
#' @param x List of arrays.
#' @return Returns image in grayscale.  ##Stimmt das?
#' @usage image_ef(x)
#' is.image_ef(x)
#' @examples
#' ##Todo: bessere Beispiele
#' image_ef(x)
#' is.image_ef(x)
=======
#' Create an image_ef object from x.
#'
#' @param x A matrix or an atomic vector of type numeric
#' @return An object of class \code{image_ef} and attribute \code{dim}. Values are taken from x.
#' the value of \code{dim} corresponds to the value of \code{dim(x)}. If
#' \code{dim} is not set for \code{x}, \code{c(length(x), 1)} is used instead.
#' @examples
#' image_ef(matrix(c(1,2,3,4), nrow=2))
#' image_ef(c(1,2,3,4))
>>>>>>> 01c8ade9c8426b705bfb85c97137da54bb3abbe0
#' @export
image_ef <- function(x) {
  stopifnot("x muss vom typ 'numeric' sein" = is.numeric(x))
  if (is.null(dim(x))) {
    x <- as.matrix(x)
  }
  class(x) <- "image_ef"
  x
}
?print
#' Show an image_ef Object as Image in the Plots section.
#'
#' An implementation of the \code{print} function for 'image_ef' objects
#'
#' @param img An image_ef object
#' @example
#' img <- image_ef(matrix(c(1,0,1,0), nrow=2))
print.image_ef <- function(img) imgShow_ef(img)

#' Test if the Input is of Class 'image_ef'
#'
#' @param img An object to be tested
#' @return TRUE, if \code{img} is of class 'image_ef', FALSE otherwise
#' @example
#' img <- image_ef(c(1,2,3,4))
#' is.image_ef(img)
#'
#' noImg <- "a"
#' is
#' @export
is.image_ef <- function(img) is.element("image_ef", class(img))

<<<<<<< HEAD
#' Normalize of image_ef objects
#'
#' Normalize an image_ef object.
#'
#' The image is subtracted by the sum of all images divided by their length.
#' @param img an object of class 'image_ef'.
#' @return Returns normalized version of img.
#' @examples
#' ##Todo: bessere Beispiele
#' normalize(img)
#' @export

#Normalisiere image_ef Objekt
=======
#' Normalization of an Object of class 'image_ef'
#'
#' Subtracts the mean over all pixel from every single pixel and returns the result
#'
#' @param img An object of class 'image_ef'
#' @return An object of class 'image_ef'. The pixel values are those of img minus the mean over all pixels of img.
#' @example
#' normalize(image_ef(c(1,0,1,0)))
#' @references https://www.bytefish.de/pdf/eigenfaces.pdf
>>>>>>> 01c8ade9c8426b705bfb85c97137da54bb3abbe0
normalize.image_ef <- function(img) {
  stopifnot("Eingabe muss ein image_ef sein" = is.image_ef(img))

  img <- img - sum(img)/length(img)
  img
}
