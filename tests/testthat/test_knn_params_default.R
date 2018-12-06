context("test knn_params_default()")

test_that("no errors occur for various values of n", {
  for (i in 1:100000) {
    expect_is(knn_params_default(i), "knn_params", label = paste("n =", i))
  }
})
