#Gibt eine Matrix img als Bild in Graustufen (und nicht auf dem Kopf wie image()) aus



imgShow_ef <- function(img) {
  img %>%
    apply(1, rev) %>%
    t() %>%
    image(col=hcl.colors(12, "Grays", rev = FALSE),xaxt="n",yaxt="n")
}

#Liest eine .csv-Datei der folgenden Struktur ein und gibt sie als n x dimension - Array wieder aus
#Jede Zeile der .csv Datei bezeichnet ein Bild. Die ersten width Pixel bezeichnen die erste Bildzeile, die
#zweiten die zweite Bildzeile ...
#n = Anzahl der Zeilen/Bilder, dimension = Dimension der einzelnen Zeilen (z.B. 64x64 für quad. Bilder)
csv_to_array_ef <- function(path, dimension = c(64,64)) {
  data  <- read.csv(path)
  nrow <- nrow(data)
  data %>% as.matrix() %>% as.double() %>% as.matrix() -> data
  dim(data) <- c(nrow, dimension)
  data
}

#'Normalization
#'
#' @param obj an object to be normalized (e.g. of class 'image_ef', 'imageset_ef')
#' @param ... additional arguments
#' @export
normalize <- function(obj, ...) UseMethod("normalize")

#' Feature Space Projection
#'
#' Projects an image or set of images into feature space defined by a set of eigenfaces
#'
#' @param obj an object to be proected on eigenfaces (e.g. of class 'image_ef', 'imageset_ef')
#' @param eigenfaces an object of class 'imageset_ef'. Describes the vectors to project on.
#' @param avgFace an object of class 'image_ef'. Average face of the underlying imageset_ef
#' @param showCoefficients a logical vector of length 1 (TRUE or FALSE). Determines whether to additionally return the coefficients of the linear combination representing obj.
#' @param ... additional arguments
#'
#' For details see FSP.image_ef and FSP.imageset_ef
#' @export
FSP <- function(obj, eigenfaces, avgFace, showCoefficients = TRUE, ...) UseMethod("FSP")
