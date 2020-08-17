#' Eigenface package
#' @details The package contains the principal component analyses of a Dataset of face images (here: Olivetti Data).
#'          It provides several functionalities:
#'
#'          * output of the n important Eigenfaces of a Dataset
#'          * Finding of similar faces for a given new face by projection on the main components.
#'          * Presentation of figures from the Dataset with reduced number of main components.
#' @references \url{https://www.bytefish.de/pdf/eigenfaces.pdf}
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
#' @import tidyverse
#' @import pracma
#' @import shiny
#' @import imager
#' @importFrom grDevices hcl.colors
#' @importFrom graphics image
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @import tibble
#' @importFrom dplyr arrange
#' @import testthat
NULL



