#' #library(tidyverse)
#' source("R/helperFunctions_ef.R")
#' source("R/imageset_ef.R")
#' source("R/image_ef.R")
#' source("R/FeatureSpaceProjection.R")
#'
#'
#'
#' #Plot of Olivetti Dataset
#'
#' #' Plot of Olivetti Dataset
#' #'
#' #' Plot different face images of the Olivetti Dataset.
#' #'
#' #' The Olivetti Dataset contains 40 Persons which are photographed at 10 different times, variying lighting facial express and facial detail.
#' #' All faces have black background and are grey level. The size is 64*64. The images were taken between April 1992 and April 1994.
#' #'
#' #' @param Persons desired persons of the Olivetti Dataset. It can be an integer or double vector or a double value. Default gives first 10 different persons.
#' #' @param number desired number of photos of the selected persons. Default is 10.
#' #'
#' #' @return Returns Olivetti-faces from the Olivetti Dataset.
#' #'
#' #' @examples
#' #' plot_olivetti()
#' #' plot_olivetti(2,2)
#' #' plot_olivetti(1:3,10)
#' #' plot_olivetti(c(1,2),10)
#' #' @export
#'
#' # plot_olivetti1 <- function(Persons=10, number=10){
#' #   td <- load_imageset_ef("olivetti_X.csv", c(64,64))
#' #   par(mfrow=c(Persons,number))
#' #   for (i in 1:Persons) {
#' #       for(j in 1:number){
#' #         idx <- (i-1)*10 + j
#' #         imgShow_ef(td[[idx]])
#' #       }
#' #     }
#' #   }
#' #
#' # plot_olivetti1(2,10)
#'
#' plot_olivetti <- function(Persons=1:10, number=10){
#'   olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
#'   td <- load_imageset_ef(olivetti, c(64,64))
#'   if ((typeof(Persons)=="integer"|typeof(Persons)=="double")&length(Persons)!=1){
#'   par(mfrow=c(length(Persons),number))
#'   for (i in Persons) {
#'     for(j in 1:number){
#'       idx <- (i-1)*10 + j
#'       imgShow_ef(td[[idx]])
#'     }
#'   }
#'   }
#'   if(typeof(Persons)=="double"){
#'   par(mfrow=c(length(Persons),number))
#'   for (i in Persons) {
#'     for(j in 1:number){
#'       idx <- (i-1)*10 + j
#'       imgShow_ef(td[[idx]])
#'     }
#'   }
#'   }
#'
#' }
#'
#' #plot_olivetti(3,10)
#'
