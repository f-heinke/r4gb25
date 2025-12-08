# devtools::load_all()
test_that("k-means scree testing works", {
  X <- as.matrix( iris[,-5] )

  testthat::expect_no_failure({
    e1 <- km_scree_test( X = X, K = 2:6, init_method = km_rand_init )
    plot( e1 )

  }

  )

  testthat::expect_no_failure({
    e2 <- km_scree_test( X = X, K = 2:6, init_method = kmpp_init )
    plot( e2 )
  }

  )
})
