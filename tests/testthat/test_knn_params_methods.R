context("test knn_params methods")

x <- knn_params_default(55)
x3 <- knn_params(4, rep(.25, 4))
x4 <- knn_params_default(1000000)

test_that("test print.knn_params", {
  expect_output(x2 <- print(x))
  expect_identical(x, x2)
  expect_output(x4 <- print(x3))
  expect_identical(x3, x4)
  expect_output(x5 <- print(x4))
  expect_identical(x4, x5)
})
