library(tidyverse)
source("R/helperFunctions_ef.R")
source("R/imageset_ef.R")
source("R/image_ef.R")
source("R/FeatureSpaceProjection.R")

#Berechnet Distanz zweier image_ef Objekte
distance_ef <- function(img1, img2) {
  stopifnot("img1 muss ein image_ef sein" = is.image_ef(img1))
  stopifnot("img2 muss ein image_ef sein" = is.image_ef(img2))

  img1 <- as.vector(img1)
  img2 <- as.vector(img2)

  #Berechne Differenz
  diff <- img1 - img2

  #Berechne die Norm der Differenz
  norm <- sqrt(dot(diff, diff))

  norm
}

#Findet image_ef Objekt im imageset_ef Objekt td mit minimaler Distanz zu image_ef img
classification_ef <- function(img, td, nfaces = 15) {
  stopifnot("img muss ein image_ef sein" = is.image_ef(img))
  stopifnot("td muss ein imageset_ef sein" = is.image_ef(td))
  stopifnot("tm muss mindestens Länge 1 haben" = length(td)>0)
  stopifnot("img und Elemente von eigenfaces müssen die gleiche Dimension besitzen" = dim(img) == dim(td[[1]]))

  #Berechne Eigenfaces (nfaces Stück)
  eigenfaces <-


  #Berechne Distanzen


}


