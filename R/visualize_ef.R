# #library(tidyverse)
# source("R/helperFunctions_ef.R")
# source("R/imageset_ef.R")
# source("R/image_ef.R")
# source("R/FeatureSpaceProjection.R")
#
# # Import Olivetti
# olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
# td <- load_imageset_ef(olivetti, c(64,64))
# olivetti_y <- system.file("extdata","olivetti_y.csv",package="eigenfaces")
# classes <- load_classes_ef(olivetti_y)
#
# ###############
# # Plot example images
#
#
# # Plot n examples of the Olivetti faces. 0 <= n <= 39.
# plot_examples <- function(td, n, mode) {
#   # n:    0 <= n <= 39.
#   # mode: either ef or ex (for eigenfaces or examples)
#   par(mfrow=c(round(sqrt(n)+0.49),round(sqrt(n)+0.49)),mar = c(2,2,2,2))
#
#   if (deparse(substitute(mode)) == "ex") {
#     for (i in 1:n) {
#       idx <- i*10
#       imgShow_ef(td[[idx]])
#     }
#     mtext("Example Images of the Olivetti Faces Dataset", side = 3, line = -1.5, outer = TRUE)
#   }
#
#   if (deparse(substitute(mode)) == "ef") {
#     for (i in 1:n) {
#       imgShow_ef(td[[i]])
#     }
#     mtext("Eigenfaces of the Olivetti Faces Dataset", side = 3, line = -1.5, outer = TRUE)
#   }
#   par(mfrow = c(1,1)) # Reset to original parameters
# }
#
# # How to use function
# # This is the image used in README.md
# #plot_examples(td, 30, mode=ef)
#
#
# ###############
# # Plot eigenfaces
#
# # Get eigenfaces and then reuse plot_examples function in mode=ef
# ef <- get_eigenfaces(td, 9)
# #ef
# #plot_examples(ef,9, mode=ef)
#
#
# ################
# # Reconstruction of faces
#
# # TODO: turn this into nice looking format
# # TODO: introduce argument. use n eigenfaces for reconstruction
# #FSP(td[[5]], ef, avg_face(normalize(td)))
# #td[[5]]
#
#         ################
# # Projection on two first eigenvectors
# #library(ggplot2) #is in library(tidyverse)
#
# ef1 <- as.vector(ef[[1]])
# ef2 <- as.vector(ef[[2]])
#
# # Initialization
# data <- data.frame(matrix(ncol=3,nrow=5, dimnames=list(NULL, c("ef1_x", "ef2_y", "class"))))
#
# # Fill with projections and corresponding classes
# for (i in 1:length(td)) {
#   data[i,] <- c(as.vector(td[[i]])%*%ef1,as.vector(td[[i]])%*%ef2,classes[[1]][i])
# }
#
# #data
#
# # Scatterplot
# #ggplot(data, aes(x=ef1_x, y=ef2_y, color=class)) + geom_point(shape=19, size=1.2) +
# #geom_text(aes(label=class), size=2, hjust = -1, vjust = 0.3)
# #  scale_color_gradientn(colours = rainbow(100))
#
