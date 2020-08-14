#test image_ef
test_that("conversion of vector (where dim == NULL) to image_ef works", {
  vec <- c(1,2,3,4)
  img <- image_ef(vec)

  expect_equal(is.image_ef(img), TRUE)
  expect_equal(img[[1]], 1)
  expect_equal(img[[2]], 2)
  expect_equal(img[[3]], 3)
  expect_equal(img[[4]], 4)
})

test_that("conversion of matrix to image_ef works", {
  vec <- matrix(c(1,2,3,4,5,6), nrow=2)
  img <- image_ef(vec)

  expect_equal(is.image_ef(img), TRUE)
  expect_equal(img[[1]], 1)
  expect_equal(img[[2]], 2)
  expect_equal(img[[3]], 3)
  expect_equal(img[[4]], 4)
  expect_equal(dim(img), c(2,3))
})

#test is.image_ef
test_that("accepts a numeric atomic vector of class 'image_ef'", {
  vec <- c(1,2,3,4)
  class(vec) <- 'image_ef'

  expect_equal(is.image_ef(vec), TRUE)
})

test_that("refuses a numeric atomic vector not of class 'image_ef'", {
  vec <- c(1,2,3,4)
  class(vec) <- 'image_def'

  expect_equal(is.image_ef(vec), FALSE)
})

test_that("refuses a non-numeric  vector  of class 'image_ef'", {
  vec <- list("a","b","c")
  class(vec) <- 'image_ef'

  expect_equal(is.image_ef(vec), FALSE)
})
