#library(tidyverse)
# source("R/helperFunctions_ef.R")


#' Create an image_ef object from x.
#'
#' @param x A matrix or an atomic vector of type numeric.
#' @return An object of class \code{image_ef} and attribute \code{dim}. Values are taken from x.
#' the value of \code{dim} corresponds to the value of \code{dim(x)}. If
#' \code{dim} is not set for \code{x}, \code{c(length(x), 1)} is used instead.
#' @examples
#' image_ef(matrix(c(1,2,3,4), nrow=2))
#' image_ef(c(1,2,3,4))
#' @export
image_ef <- function(x) {
  stopifnot("x muss vom typ 'numeric' sein" = is.numeric(x))
  if (is.null(dim(x))) {
    x <- as.matrix(x)
  }
  class(x) <- "image_ef"
  x
}

#' Show an image_ef object as image in the plots section
#'
#' An implementation of the \code{print} function for 'image_ef' objects.
#'
#' @param img An image_ef object.
#' @param ... TODO
#' @examples
#' img <- image_ef(matrix(c(1,0,1,0), nrow=2))
#' @export
print.image_ef <- function(img, ...) {
  stopifnot("img must be of class 'image_ef'" = is.image_ef(img))
  imgShow_ef(img)
}

#' Test if the input is of class 'image_ef'
#'
#' @param img An object to be tested.
#' @return TRUE, if \code{img} is of class 'image_ef', FALSE otherwise
#' @examples
#' img <- image_ef(c(1,2,3,4))
#' is.image_ef(img)
#'
#' noImg <- "a"
#' is.image_ef(noImg)
#' @export
is.image_ef <- function(img) {is.numeric(img) && is.element("image_ef", class(img))}

#' Normalization of an object of class 'image_ef'
#'
#' Subtracts the mean over all pixel from every single pixel and returns the result.
#'
#' @param img An object of class 'image_ef'.
#' @return An object of class 'image_ef'. The pixel values are those of img minus the mean over all pixels of img.
#' @export
#' @examples
#' normalize(image_ef(c(1,0,1,0)))
#' @references https://www.bytefish.de/pdf/eigenfaces.pdf
normalize.image_ef <- function(img) {
  stopifnot("Eingabe muss ein image_ef sein" = is.image_ef(img))

  img <- img - sum(img)/length(img)
  img
}
