library(tidyverse)
source("R/helperFunctions_ef.R")

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

#Normalisiere image_ef Objekt
normalize.image_ef <- function(img) {
  stopifnot("Eingabe muss ein imageset_ef sein" = is.image_ef(img))

  img <- img - sum(img)/length(img)
  img
}

