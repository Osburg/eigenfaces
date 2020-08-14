#test load_imageset_ef
test_that("loading sets of images works", {

})




olivetti <- system.file("extdata","olivetti_X.csv",package="eigenfaces")
td <- load_imageset_ef(olivetti, c(64,64))
system.file("olivetti_X.csv",package="eigenfaces")
?system.file
