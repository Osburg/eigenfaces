library(tidyverse)
source("R/helperFunctions_ef.R")

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
#' @export

#Erstelle aus x ein Objekt der Klasse image_ef
image_ef <- function(x) {
  if (is.null(dim(x))) {
    x <- as.matrix(x)
  }
  class(x) <- "image_ef"
  x
}

#Gebe Bild aus
print.image_ef <- function(img) imgShow_ef(img)

#Prüfe, ob es sich um ein image_ef handelt
is.image_ef <- function(img) is.element("image_ef", class(img))

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
normalize.image_ef <- function(img) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.image_ef(img))

  img <- img - sum(img)/length(img)
  img
}

