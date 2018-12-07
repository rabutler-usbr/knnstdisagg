context("test knn_params_default()")

test_that("no errors occur for various values of n", {
  vv <- 1:1000
  vv <- vv ^ 2
  for (i in vv) {
    expect_is(knn_params_default(i), "knn_params", label = paste("n =", i))
  }
})
