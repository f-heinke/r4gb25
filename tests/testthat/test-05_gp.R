test_that("GP algorithm works correctly", {
  X <- iris[, -5]
  Xs <- scale( X )

  D <- dist( Xs )

  expect_no_failure(
    r1 <- gp( D)
  )

  expect_no_failure(
    r2 <- gp( D )
  )

})
