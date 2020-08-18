#Run shiny

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
#' \dontrun{
#' runShiny_ef()
#' }
runShiny_ef <- function() {
  shiny::shinyAppDir(system.file("inst/shiny-apps/shiny_ef/", package = "eigenfaces"))
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
#' \dontrun{
#' mnist <- system.file("extdata","mnist_X.csv",package="eigenfaces")
#' td <- load_imageset_ef(mnist, c(28,28))
#' runShiny_general(td)
#' }
runShiny_general <- function(td) {
  stopifnot("td must be of class 'imageset_ef'" = is.imageset_ef(td))
  stopifnot("td must be at least of length 1" = length(td)>0)
  shiny::shinyAppDir(system.file("inst/shiny-apps/shiny_general/", package = "eigenfaces"))
}
