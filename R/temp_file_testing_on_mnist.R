# Testing mnist data

#source("R/visualize_ef.R")


load_imageset_mnist <- function(path, imgDim) {
  stopifnot("imgDim must be numeric" = is.numeric(imgDim))
  stopifnot("imgDim must be of length 2" = length(imgDim)==2)
  stopifnot("imgDim must be atomic" = is.atomic(imgDim))

  #Lese Daten ein und speichere sie als [n_img, imgDim]-Array
  data  <- read.csv(path, header=FALSE)

  #MNIST!
  data <- data[2:length(data)]

  nrow <- nrow(data)
  data %>% as.matrix() %>% as.double() %>% as.matrix() -> data
  dim(data) <- c(nrow, imgDim)

  #Erstelle Objekt der Klasse imageset_ef
  td <- list()
  class(td) <- "imageset_ef"

  #Füge die image_ef Objekte in das imageset_ef Objekt ein
  for (i in 1:dim(data)[1]) td[[i]] <- image_ef(data[i,,])

  td
}

load_classes_mnist <- function(path, header=FALSE) {
  td2 <- read.csv(path, header=FALSE)
  #MNIST!
  mnist_classes <- td2[[1]]
  mnist_classes
}


##############

td2 <- load_imageset_mnist("mnist_test.csv", c(28,28))
classes_mnist <- load_classes_mnist("mnist_test.csv")

#plot_examples(td2, 9, mode=ex)

ef2 <- get_eigenfaces(td2, 400)
#plot_examples(ef2, 9, mode=ef)

#FSP(td2[[1]], ef2, avg_face(normalize(td2)))
#td2[[1]]

# Projection on two first eigenvectors
library(ggplot2)

ef1x <- as.vector(ef2[[1]])
ef2y <- as.vector(ef2[[2]])

# Initialization
data <- data.frame(matrix(ncol=3,nrow=5, dimnames=list(NULL, c("ef1_x", "ef2_y", "class"))))

# Fill with projections and corresponding classes
for (i in 1:length(td)) {
  data[i,] <- c(as.vector(td2[[i]])%*%ef1x,as.vector(td2[[i]])%*%ef2y,classes_mnist[i])
}

#data

# Scatterplot
#ggplot(data, aes(x=ef1_x, y=ef2_y, color=class)) + geom_point(shape=19, size=1.2) +
  #geom_text(aes(label=class), size=2, hjust = -1, vjust = 0.3)
#  scale_color_gradientn(colours = rainbow(100))
