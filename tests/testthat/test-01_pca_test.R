library("r4gb25")

test_that("pca is correctly conducted for WBC dataset", {
  if( F) {
    d <- read.table("tests/test_data/wbcd.slim.tab")
  }

  d <- read.table("../test_data/wbcd.slim.tab")
  ds <- d[, 3:12]

  testthat::expect_no_failure(
    X <- apply(ds, 2, scaleN)
  )

  X <- apply(ds, 2, scaleN)

  testthat::expect_no_failure(
    pca_res <- pca( X )
  )

  pca_res <- pca( X )

  testthat::expect_equal( sum( pca_res$eigen.values ) , ncol( X ) )
})
