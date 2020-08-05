library(tidyverse)
source("R/helperFunctions_ef.R")
source("R/imageset_ef.R")
source("R/image_ef.R")

# Import Olivetti
td <- load_imageset_ef("olivetti_X.csv", c(64,64))
classes <- load_classes_ef("olivetti_y.csv")


###############
# Plot example images


# Plot n examples of the Olivetti faces. 0 <= n <= 39.
plot_examples <- function(td, n, mode) {
  # n:    0 <= n <= 39.
  # mode: either ef or ex (for eigenfaces or examples)
  par(mfrow=c(round(sqrt(n)+0.49),round(sqrt(n)+0.49)),mar = c(2,2,2,2))

  if (deparse(substitute(mode)) == "ex") {
    for (i in 1:n) {
      idx <- i*10
      imgShow_ef(td[[idx]])
    }
    mtext("Example Images of the Olivetti Faces Dataset", side = 3, line = -1.5, outer = TRUE)
  }

  if (deparse(substitute(mode)) == "ef") {
    for (i in 1:n) {
      imgShow_ef(td[[i]])
    }
    mtext("Eigenfaces of the Olivetti Faces Dataset", side = 3, line = -1.5, outer = TRUE)
  }
  par(mfrow = c(1,1)) # Reset to original parameters
}

# How to use function
# This is the image used in README.md
plot_examples(td, 9, mode=ex)


###############
# Plot eigenfaces

# Get eigenfaces and then reuse plot_examples function in mode=ef
ef <- getEigenfaces(td, 9)
plot_examples(ef, 9, mode=ef)

