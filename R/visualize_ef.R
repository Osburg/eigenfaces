library(tidyverse)
source("R/helperFunctions_ef.R")
source("R/imageset_ef.R")
source("R/image_ef.R")

td <- load_imageset_ef("olivetti_X.csv", c(64,64))
imgShow_ef(td[[2]])


