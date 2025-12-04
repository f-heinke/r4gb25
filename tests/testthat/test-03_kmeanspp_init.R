library("r4gb25")
test_that("kmeans++ initialization works", {
  X <- scale( iris[,1:4] )

  testthat::expect_no_error(
    kmpp_init( X, k = 5 )
  )

})
