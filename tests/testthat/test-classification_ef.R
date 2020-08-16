test_that("returns an 'imageset_ef' of length nclosest and elements of correct dimension.", {
  #create test image and imageset
  img1 <- image_ef(matrix(c(1,1,1,1), nrow=2))
  img2 <- image_ef(matrix(c(1,0,1,0), nrow=2))
  img3 <- image_ef(matrix(c(0,1,0,1), nrow=2))
  img4 <- image_ef(matrix(c(1,2,1,2), nrow=2))
  img5 <- image_ef(matrix(c(1,1,1,0), nrow=2))
  td <- imageset_ef(list(img1, img2, img3, img4, img5))
  nclosest <- 2
  neigenfaces <- 2
  closest <- classification_ef(img1, td, nclosest, neigenfaces, quick = FALSE)

  expect_equal(is.imageset_ef(closest), TRUE)
  expect_equal(length(closest), 2)
  expect_equal(dim(closest[[1]]), c(2,2))
  expect_equal(is.image_ef(closest[[1]]), TRUE)
  expect_equal(is.image_ef(closest[[2]]), TRUE)
})

test_that("classification works for quick mode", {
  #create test image and imageset
  img1 <- image_ef(matrix(c(1,0,0,1), nrow=2, byrow=TRUE))
  img2 <- image_ef(matrix(c(2,0,0,2), nrow=2, byrow=TRUE))
  img3 <- image_ef(matrix(c(10,0,0,10), nrow=2, byrow=TRUE))
  td <- imageset_ef(list(img1, img2, img3))
  nclosest <- 2
  neigenfaces <- 1

  #apply classification_ef
  closest <- classification_ef(img1, td, nclosest, neigenfaces, quick = TRUE)

  #test results
  expect_equal(closest[[1]], img1)
  expect_equal(closest[[2]], img2)
})

test_that("classification works for normal mode", {
  #create test image and imageset
  img1 <- image_ef(matrix(c(1,0,0,1), nrow=2, byrow=TRUE))
  img2 <- image_ef(matrix(c(2,0,0,2), nrow=2, byrow=TRUE))
  img3 <- image_ef(matrix(c(10,0,0,10), nrow=2, byrow=TRUE))
  td <- imageset_ef(list(img1, img2, img3))
  nclosest <- 2
  neigenfaces <- 1

  #apply classification_ef
  closest <- classification_ef(img1, td, nclosest, neigenfaces, quick = FALSE)

  #test results
  expect_equal(closest[[1]], img1)
  expect_equal(closest[[2]], img2)
})
