#Test FSP.image_ef
test_that("projection of image_ef works", {
  obj <- image_ef(c(3,3,-3,-3))
  img1 <- image_ef(c(2,0,0,0))
  img2 <- image_ef(c(0,2,0,0))
  eigenfaces <- imageset_ef(list(img1,img2))
  avgFace <- image_ef(c(0,0,0,0))

  out1 <- FSP(obj, eigenfaces, avgFace, showCoefficients = TRUE)
  out2 <- FSP(obj, eigenfaces, avgFace, showCoefficients = FALSE)

  #out1
  expect_equal(is.list(out1), TRUE)
  expect_equal(length(out1), 2)
  expect_equal(is.image_ef(out1[[1]]), TRUE)
  expect_equal(dim(out1[[1]]), c(4,1))
  expect_equal(out1[[1]][1], 3)
  expect_equal(out1[[1]][2], 3)
  expect_equal(out1[[1]][3], 0)
  expect_equal(out1[[1]][3], 0)
  expect_equal(is.numeric((out1[[2]])), TRUE)
  expect_equal(length(out1[[2]]), 2)
  expect_equal(out1[[2]][1], 1.5)
  expect_equal(out1[[2]][2], 1.5)

  #out2
  expect_equal(is.list(out2), TRUE)
  expect_equal(length(out2), 1)
  expect_equal(is.image_ef(out2[[1]]), TRUE)
  expect_equal(dim(out2[[1]]), c(4,1))
  expect_equal(out2[[1]][1], 3)
  expect_equal(out2[[1]][2], 3)
  expect_equal(out2[[1]][3], 0)
  expect_equal(out2[[1]][3], 0)
})


#Test FSP.imageset_ef
test_that("projection of image_ef works", {
  obj <- imageset_ef(lst(image_ef(c(3,3,-3,-3))))
  img1 <- image_ef(c(2,0,0,0))
  img2 <- image_ef(c(0,2,0,0))
  eigenfaces <- imageset_ef(list(img1,img2))
  avgFace <- image_ef(c(0,0,0,0))

  out1 <- FSP(obj, eigenfaces, avgFace, showCoefficients = TRUE)
  out2 <- FSP(obj, eigenfaces, avgFace, showCoefficients = FALSE)

  #out1
  expect_equal(is.list(out1), TRUE)
  expect_equal(length(out1), 2)

  out1img <- out1[[1]]
  out1coeff <- out1[[2]]

  expect_equal(is.list(out1img), TRUE)
  expect_equal(length(out1img), 1)
  expect_equal(is.image_ef(out1img[[1]]), TRUE)
  expect_equal(dim(out1img[[1]]), c(4,1))
  expect_equal(out1img[[1]][1], 3)
  expect_equal(out1img[[1]][2], 3)
  expect_equal(out1img[[1]][3], 0)
  expect_equal(out1img[[1]][3], 0)

  expect_equal(is.list(out1coeff), TRUE)
  expect_equal(length(out1coeff), 1)
  expect_equal(is.numeric(out1coeff[[1]]), TRUE)
  expect_equal(length(out1coeff[[1]]), 2)
  expect_equal(out1coeff[[1]][1], 1.5)
  expect_equal(out1coeff[[1]][2], 1.5)

  #out2
  expect_equal(is.list(out2), TRUE)
  expect_equal(length(out2), 1)

  out1img <- out1[[1]]

  expect_equal(is.list(out1img), TRUE)
  expect_equal(length(out1img), 1)
  expect_equal(is.image_ef(out1img[[1]]), TRUE)
  expect_equal(dim(out1img[[1]]), c(4,1))
  expect_equal(out1img[[1]][1], 3)
  expect_equal(out1img[[1]][2], 3)
  expect_equal(out1img[[1]][3], 0)
  expect_equal(out1img[[1]][3], 0)
})
