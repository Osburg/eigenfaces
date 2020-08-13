# Temporary file

# Todo: Read-in different file-types
library(tidyverse)
library(imager)

#' Load any datase
#'
#' Makes it possible to not only load .csv files (as in the case of the Olivetti
#' Dataset) but also to load seperate .png, .jpeg or .bmp files as an imageset.
#'
#' @param folderpath str, relative path to the folder (incl. the foldername)
#' @param filetype str, lower letter filetype
#'
#' @return td, List of arrays. Training Data.
#' @export
#'
#' @examples
#' td <- load_any_imageset("testfiles", "jpg")
#' imageShow_ef(td[[1]])
load_any_imageset <- function(folderpath, filetype) {
  filepath <- paste(folderpath, "/",
                     list.files(path = folderpath, pattern=paste("*.",filetype,sep="")),
                     sep = "")

  # Creates object of the class imageset_ef
  td <- list()
  class(td) <- "imageset_ef"

  # Puts images in the imageset_ef object
  cnt <- 1
  for (fp in filepath) {
    im <- load.image(fp)

    # Turn RGB into grayscale
    if (dim(im)[4]==3) {
      im <- grayscale(im)
    }

    class(im) <- "image_ef"
    td[[cnt]] <- im[,,1,1]
    cnt <- cnt+1
  }
  td
}

td <- load_any_imageset("testfiles", "jpg")



