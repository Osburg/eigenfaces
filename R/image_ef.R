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

