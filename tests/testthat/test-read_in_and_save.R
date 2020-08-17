# test read_in_and_save

test_that("loading sets of seperate images works", {
  folder <- system.file("extdata","testfiles",package="eigenfaces")
  td <- load_any_imageset(folder, "jpg")

  expect_equal(is.imageset_ef(td), TRUE)
  expect_equal(length(td), 5)
  for (i in 1:5) {
    expect_equal(is.image_ef(td[[i]]), TRUE)
  }
})

test_that("converting of image_ef object works", {
  img1 <- image_ef(matrix(c(0,1,1,0), nrow=2))
  class(img1) <- "image_ef"
  img1 <- as.cimg(unclass(img1))

  expect_equal(is.image_ef(img1), FALSE)
  expect_equal(is.cimg(img1), TRUE)
})
