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



runShiny_ef <- function() {
  source("R/helperFunctions_ef.R")
  source("R/imageset_ef.R")
  source("R/image_ef.R")
  source("R/FeatureSpaceProjection.R")
  #source("R/temp_file_testing_on_mnist.R")

  td <<- load_imageset_ef("inst/extdata/olivetti_X.csv", c(64,64))
  ef <<- get_eigenfaces(td, 400, quick = FALSE)

  shiny::runApp("inst/shiny-apps/shiny_ef")
}

