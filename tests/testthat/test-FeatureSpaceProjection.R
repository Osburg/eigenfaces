#Test FSP.image_ef
test_that("projection of image_ef works", {
  obj <- image_ef(c(3,3,-3,-3))
  img1 <- image_ef(c(2,0,0,0))
  img2 <- image_ef(c(0,2,0,0))
  eigenfaces <- imageset_ef(list(img1,img2))
  avgFace <- image_ef(c(0,0,0,0))

  out1 <- FSP(obj, eigenfaces, avgFace, showCoefficients = TRUE)
  out2 <- FSP(obj, eigenfaces, avgFace, showCoefficients = FALSE)

  expect_equal(is.image_ef(out1[[1]]), TRUE)
  out1[[1]][2]


})
