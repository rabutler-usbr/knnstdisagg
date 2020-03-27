context("check knn_params object creation")

test_that("creation errors correctly", {
  expect_error(
    knn_params(k = c(2, 1), weights = c(0.6, 0.4)),
    "`k` should be a single whole number >= 1",
    fixed = TRUE
  )

  expect_error(
    knn_params(k = -7, weights = c(0.6, 0.4)),
    "`k` should be a single whole number >= 1",
    fixed = TRUE
  )

  expect_error(
    knn_params(k = 4.2, weights = c(0.6, 0.4)),
    "`k` should be a single whole number >= 1",
    fixed = TRUE
  )

  expect_error(
    knn_params(k = 2, weights = c(1, 2)),
    "`weights` should sum to 1",
    fixed = TRUE
  )

  expect_error(
    knn_params(k = 2, weights = c(0.1, 0.5)),
    "`weights` should sum to 1",
    fixed = TRUE
  )

})
