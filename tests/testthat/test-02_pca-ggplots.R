library("r4gb25")

test_that("pca results are correctly plotted using ggplot2", {

  if( F) {
    d <- read.table("tests/test_data/wbcd.slim.tab")
  }

  d <- read.table("../test_data/wbcd.slim.tab")
  ds <- d[, 3:12]

  X <- apply(ds, 2, scaleN)

  pca_res <- pca( X )

  testthat::expect_no_failure(
    pca_ggplots( pca_res )
  )
})
