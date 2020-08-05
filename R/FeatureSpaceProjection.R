library(tidyverse)
library(pracma)
source("R/helperFunctions_ef.R")
source("R/imageset_ef.R")
source("R/image_ef.R")

#Projeziert x auf den Einheitsvektor in y-Richtung
proj <- function(x,y) {
  x <- as.vector(x)
  y <- as.vector(y)

  #Normiere y
  y <- y / sqrt(dot(y,y))

  dot(x,y)
}


#TODO: Es ist bisher nicht sichergestellt, dass die Eigenfaces orthogonal sind (auch wenn es bei Tests stets so war).
#Man sollte zumindest in der PCA sicherstellen, dass die Eigenwerte paarweise verschieden sind, dies würde orthogonalität absichern

#Projiziert image_ef img auf ein gegebenes Set von eigenfaces
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

#Projiziert imageset_ef td auf ein gegebenes Set von eigenfaces
FSP.imageset_ef <- function(td, eigenfaces, avgFace, showCoefficients = FALSE) {
  stopifnot("eigenfaces muss ein imageset_ef sein" = is.imageset_ef(eigenfaces))
  stopifnot("eigenfaces muss mindestens die Länge 1 haben" = length(eigenfaces)>0)
  stopifnot("td muss ein  imageset_ef sein" = is.imageset_ef(td))
  stopifnot("Elemente von td und Elemente von eigenfaces müssen die gleiche Dimension besitzen" = dim(td[[1]]) == dim(eigenfaces[[1]]))
  stopifnot("td muss mindestens die Länge 1 haben" = length(td)>0)

  for (i in 1:length(td)) {
    td[[i]] <- FSP(td[[i]], eigenfaces, avgFace)
    print(i)
  }

  td
}



