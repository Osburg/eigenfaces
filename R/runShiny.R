#Run shiny

# #' @export
# runShiny <- function() {
#   appDir <- system.file("shiny-apps", "shiny_ef", package = "eigenfaces")
#   if (appDir == "") {
#     stop("Could not find directory. Try re-installing `eigenfaces`.", call. = FALSE)
#   }
#
#   shiny::runApp(appDir, display.mode = "normal")
# }



#' Run Shiny App for Olivetti Dataset
#'
#' Opens customized shiny app for the Olivetti Dataset. No need to load Dataset
#' before usage. Number of eigenfaces is limited to 400.
#' Functions: 1. Display any image of the dataset. 2. Reconstruct this image
#' with an amount of eigenfaces of your choice. 3. Display the first 400
#' eigenfaces.
#'
#' @return Command to open shiny app.
#' @export
#'
#' @examples
#' # Opens Shiny App for Olivetti Dataset
#' runShiny_ef()
runShiny_ef <- function() {
  source("R/helperFunctions_ef.R")
  source("R/imageset_ef.R")
  source("R/image_ef.R")
  source("R/FeatureSpaceProjection.R")

  td <<- load_imageset_ef("inst/extdata/olivetti_X.csv", c(64,64))
  ef <<- get_eigenfaces(td, 400, quick = FALSE) #auf FALSE aendern fuer finale Version

  shiny::runApp("inst/shiny-apps/shiny_ef")
}



#' Run Shiny App for Any Dataset
#'
#' Opens shiny app for any suitable dataset. Number of eigenvectors is limited to
#' 400.
#' Functions: 1. Display any image of the dataset. 2. Reconstruct this image
#' with an amount of eigenvectors of your choice. 3. Display the first 400
#' eigenvectors.
#'
#' @param td Object of class imageset_ef. Training data.
#'
#' @return Command to open shiny app.
#' @export
#'
#' @examples
#' # Opens Shiny App for (e.g.) MNIST Dataset
#' td <- load_imageset_ef("inst/extdata/mnist_X.csv", c(28,28))
#' runShiny_general(td)
runShiny_general <- function(td) {
  source("R/helperFunctions_ef.R")
  source("R/imageset_ef.R")
  source("R/image_ef.R")
  source("R/FeatureSpaceProjection.R")

  #td <<- load_imageset_ef("inst/extdata/mnist_X.csv", c(28,28))
  td <<- td
  ef <<- get_eigenfaces(td, 400, quick = FALSE) #auf FALSE aendern fuer finale Version

  shiny::runApp("inst/shiny-apps/shiny_general")
}
