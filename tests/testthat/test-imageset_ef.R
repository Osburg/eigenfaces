#test load_imageset_ef
test_that("loading sets of images works", {
  olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
  td <- load_imageset_ef(olivetti, c(64,64))

  expect_equal(is.imageset_ef(td), TRUE)
  expect_equal(length(td), 400)
  for (i in 1:400) {
    expect_equal(is.image_ef(td[[i]]), TRUE)
    expect_equal(length(td[[i]]), 64*64)
    expect_equal(dim(td[[i]]), c(64,64))
  }
})

#test load_classes_ef
test_that("loading sets of classes works", {
  olivetti <- system.file("extdata","olivetti_y.csv",package="eigenfaces")
  classes <- load_classes_ef(olivetti)

  expect_equal(length(classes[[1]]), 400)
  for (i in 1:400) {
    expect_equal(is.numeric(classes[[1]][[i]]), TRUE)
    expect_equal(length(classes[[1]][[i]]), 1)
  }
})

#test imageset_ef
test_that("creating imageset from list of matrices works", {
  img1 <- matrix(c(0,1,1,0), nrow=2)
  img2 <- matrix(c(1,1,1,1), nrow=2)
  lst <- list(img1, img2)
  td <- imageset_ef(lst)

  expect_equal(length(td), 2)
  expect_equal(is.imageset_ef(td), TRUE)
  for (i in 1:2) {
    expect_equal(is.image_ef(td[[i]]), TRUE)
    expect_equal(length(td[[i]]), 4)
    expect_equal(dim(td[[i]]), c(2,2))
  }
})

#test is.imageset_ef
test_that("recognizes a list of images_ef objects of class 'imageset_ef' as imageset_ef", {
  img1 <- image_ef(matrix(c(0,1,1,0), nrow=2))
  img2 <- image_ef(matrix(c(1,1,1,1), nrow=2))
  lst <- list(img1, img2)
  class(lst) <- "imageset_ef"

  expect_equal(is.imageset_ef(lst), TRUE)
})

test_that("refuses a list of objects (not of class 'image_ef') of class 'imageset_ef'", {
  img1 <- image_ef(matrix(c(0,1,1,0), nrow=2))
  img2 <- "a"
  lst <- list(img1, img2)
  class(lst) <- "imageset_ef"

  expect_equal(is.imageset_ef(lst), FALSE)
})

test_that("refuses a list of image_ef objects when class 'imageset_ef' is missing", {
  img1 <- image_ef(matrix(c(0,1,1,0), nrow=2))
  img2 <- image_ef(matrix(c(1,1,1,1), nrow=2))
  lst <- list(img1, img2)

  expect_equal(is.imageset_ef(lst), FALSE)
})

test_that("refuses a list of image_ef objects of class 'imageset_ef', if image_ef objects have different dimensions", {
  img1 <- image_ef(matrix(c(0,1,1,0), nrow=2))
  img2 <- image_ef(matrix(c(1,1,1,1), nrow=4))
  lst <- list(img1, img2)
  class(lst) <- "imageset_ef"

  expect_equal(is.imageset_ef(lst), FALSE)
})


#test avg_face
test_that("returns an image_ef of the same dimension", {
  img1 <- image_ef(matrix(c(0,1,1,0), nrow=2))
  img2 <- image_ef(matrix(c(0,3,3,0), nrow=2))
  td <- imageset_ef(list(img1, img2))

  avgFace <- avg_face(td)

  expect_equal(is.image_ef(avgFace), TRUE)
  expect_equal(dim(avgFace), c(2,2))
})

test_that("computing the average works", {
  img1 <- image_ef(matrix(c(0,1,1,0), nrow=2))
  img2 <- image_ef(matrix(c(0,3,3,0), nrow=2))
  td <- imageset_ef(list(img1, img2))

  avgFace <- avg_face(td)

  expect_equal(avgFace, image_ef(matrix(c(0,2,2,0), nrow=2)))
})

#test PCA
test_that("works fine in normal mode", {
  img1 <- image_ef(matrix(c(1,0,0,1), nrow=2, byrow=TRUE))
  img2 <- image_ef(matrix(c(2,0,0,2), nrow=2, byrow=TRUE))
  img3 <- image_ef(matrix(c(10,0,0,10), nrow=2, byrow=TRUE))
  td <- imageset_ef(list(img1, img2, img3))

  out1 <- PCA(td, showEigenvals = TRUE, quick = FALSE)
  out2 <- PCA(td, showEigenvals = FALSE, quick = FALSE)

  #out1
  expect_equal(is.list(out1), TRUE)
  expect_equal(length(out1), 2)
  expect_equal(is.imageset_ef(out1[[1]]), TRUE)
  expect_equal(is.numeric(out1[[2]]), TRUE)
  expect_equal(length(out1[[2]]), 4)
  expect_equal(out1[[2]][[1]], 210)
  expect_equal(out1[[2]][[2]], 0)
  expect_equal(out1[[2]][[3]], 0)
  expect_equal(out1[[2]][[4]], 0)

  expect_equal(length(out1[[1]]), 4)
  mat <- matrix(c(105,0,0,105, 0,0,0,0, 0,0,0,0, 105,0,0,105), nrow=4, byrow=TRUE)

  vec <- out1[[1]][[1]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 210*vec)

  vec <- out1[[1]][[2]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

  vec <- out1[[1]][[3]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

  vec <- out1[[1]][[4]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

  #out2
  expect_equal(is.list(out2), TRUE)
  expect_equal(length(out2), 1)
  expect_equal(is.imageset_ef(out2[[1]]), TRUE)

  expect_equal(length(out2[[1]]), 4)
  mat <- matrix(c(105,0,0,105, 0,0,0,0, 0,0,0,0, 105,0,0,105), nrow=4, byrow=TRUE)

  vec <- out2[[1]][[1]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 210*vec)

  vec <- out2[[1]][[2]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

  vec <- out2[[1]][[3]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

  vec <- out2[[1]][[4]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)
})


test_that("works fine in quick mode", {
  img1 <- image_ef(matrix(c(1,0,0,1), nrow=2, byrow=TRUE))
  img2 <- image_ef(matrix(c(2,0,0,2), nrow=2, byrow=TRUE))
  img3 <- image_ef(matrix(c(10,0,0,10), nrow=2, byrow=TRUE))
  td <- imageset_ef(list(img1, img2, img3))

  out1 <- PCA(td, showEigenvals = TRUE, quick = TRUE)
  out2 <- PCA(td, showEigenvals = FALSE, quick = TRUE)

  #out1
  expect_equal(is.list(out1), TRUE)
  expect_equal(length(out1), 2)
  expect_equal(is.imageset_ef(out1[[1]]), TRUE)
  expect_equal(is.numeric(out1[[2]]), TRUE)
  expect_equal(length(out1[[2]]), 3)
  expect_equal(out1[[2]][[1]], 210)
  expect_equal(out1[[2]][[2]], 0)
  expect_equal(out1[[2]][[3]], 0)

  expect_equal(length(out1[[1]]), 3)
  mat <- matrix(c(105,0,0,105, 0,0,0,0, 0,0,0,0, 105,0,0,105), nrow=4, byrow=TRUE)

  vec <- out1[[1]][[1]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 210*vec)

  vec <- out1[[1]][[2]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

  vec <- out1[[1]][[3]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

  #out2
  expect_equal(is.list(out2), TRUE)
  expect_equal(length(out2), 1)
  expect_equal(is.imageset_ef(out2[[1]]), TRUE)

  expect_equal(length(out2[[1]]), 3)
  mat <- matrix(c(105,0,0,105, 0,0,0,0, 0,0,0,0, 105,0,0,105), nrow=4, byrow=TRUE)

  vec <- out2[[1]][[1]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 210*vec)

  vec <- out2[[1]][[2]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

  vec <- out2[[1]][[3]]
  class(vec) <- NULL
  dim(vec) <- c(4,1)
  expect_equal(mat%*%vec, 0*vec)

})

#Test get_eigenfaces
#comment: not every feature has to be tested here. The test of quick mode
#guarantees for the functionality of parts of this function (e.g. quick mode/ normal mode...)
test_that("works in normal mode", {
  img1 <- image_ef(matrix(c(1,0,0,1), nrow=2, byrow=TRUE))
  img2 <- image_ef(matrix(c(2,0,0,2), nrow=2, byrow=TRUE))
  img3 <- image_ef(matrix(c(10,0,0,10), nrow=2, byrow=TRUE))
  td <- imageset_ef(list(img1, img2, img3))

  eigenfaces <- get_eigenfaces(td, nfaces=2, quick=FALSE)

  expect_equal(is.imageset_ef(eigenfaces), TRUE)
  expect_equal(length(eigenfaces), 2)
})
