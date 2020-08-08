library(tidyverse)
source("R/helperFunctions_ef.R")
source("R/imageset_ef.R")
source("R/image_ef.R")
source("R/FeatureSpaceProjection.R")

#Berechnet Distanz zweier image_ef Objekte
distance_ef <- function(coeffs1, coeffs2) {
  stopifnot("Längen der Koeffizientenvektoren müssen übereinstimmen" = length(coeffs1) == length(coeffs2))

  #Berechne Differenz
  diff = coeffs1 - coeffs2

  #Berechne die Norm der Differenz
  norm <- sqrt(dot(diff, diff))

  norm
}

#Findet die nclosest image_ef Objekte im imageset_ef Objekt td mit minimaler Distanz zu image_ef img
classification_ef <- function(img, td, nclosest = 3, neigenfaces = 15, quick = FALSE) {
  stopifnot("img muss ein image_ef sein" = is.image_ef(img))
  stopifnot("td muss ein imageset_ef sein" = is.imageset_ef(td))
  stopifnot("td muss mindestens Länge 1 haben" = length(td)>0)
  stopifnot("img und Elemente von eigenfaces müssen die gleiche Dimension besitzen" = dim(img) == dim(td[[1]]))

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


