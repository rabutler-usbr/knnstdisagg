context("check knn_get_index_year code")


flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
# made up historical data to use as index years
ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))

# knn_get_index_year errors ------------------------
test_that("knn_get_index_year errors correctly", {
  expect_error(
    knn_get_index_year(4, ind_flow), 
    "`ann_flow` should be a 2-column matrix"
  )
  expect_error(
    knn_get_index_year(flow_mat, 7),
    "`ann_index_flow` should be a 2-column matrix"
  )
  expect_error(
    knn_get_index_year(flow_mat[1,], ind_flow),
    "`ann_flow` should be a 2-column matrix"
  )
  expect_error(knn_get_index_year(flow_mat, ind_flow, k_weights = c(1, 7)))
  expect_error(
    knn_get_index_year(flow_mat, ind_flow, list(K = 2, weights = c(0.6, 0.4))),
    "`k_weights` should have only names `k` and `weights`"
  )
  expect_error(
    knn_get_index_year(flow_mat, ind_flow, list(k = 2, Weights = c(0.6, 0.4))),
    "`k_weights` should have only names `k` and `weights`"
  )
  
  expect_error(
    knn_get_index_year(flow_mat, ind_flow, list(k = 2, weights = c(0.6, 0.4), z = 1)),
    "`k_weights` should have only names `k` and `weights`"
  )
  
  expect_error(
    knn_get_index_year(flow_mat, ind_flow, list(k = c(2, 1), weights = c(0.6, 0.4))),
    "`k_weights$k` should be a single whole number >= 1",
    fixed = TRUE
  )
  
  expect_error(
    knn_get_index_year(flow_mat, ind_flow, list(k = -7, weights = c(0.6, 0.4))),
    "`k_weights$k` should be a single whole number >= 1",
    fixed = TRUE
  )
  
  expect_error(
    knn_get_index_year(flow_mat, ind_flow, list(k = 4.2, weights = c(0.6, 0.4))),
    "`k_weights$k` should be a single whole number >= 1",
    fixed = TRUE
  )
  
  expect_error(
    knn_get_index_year(flow_mat, ind_flow[1:3,], list(k = 4, weights = rep(0.25, 4))),
    "`k` should be <= the number of potential index years.",
    fixed = TRUE
  )
  
  expect_error(
    knn_get_index_year(flow_mat, ind_flow, list(k = 2, weights = c(1, 2))),
    "`k_weights$weights` should sum to 1",
    fixed = TRUE
  )
  
  expect_error(
    knn_get_index_year(flow_mat, ind_flow, list(k = 2, weights = c(0.1, 0.5))),
    "`k_weights$weights` should sum to 1",
    fixed = TRUE
  )
})


# knn_get_index_year structure ---------------------
test_that("knn_get_index_year returns correct structure", {
  expect_type(tmp <- knn_get_index_year(flow_mat, ind_flow), "double")
  expect_true(is.matrix(tmp))
  expect_equal(dim(tmp), c(3, 1))
  expect_true(all(tmp %in% ind_flow[, 1]))
  
  expect_type(
    tmp <- knn_get_index_year(flow_mat[1, , drop = FALSE], ind_flow), 
    "double"
  )
  expect_true(is.matrix(tmp))
  expect_equal(dim(tmp), c(1, 1))
  expect_true(all(tmp %in% ind_flow[, 1]))
  
  expect_type(
    tmp <- knn_get_index_year(
      flow_mat, 
      ind_flow, 
      list(k = 4, weights = rep(0.25, 4))
    ), 
    "double"
  )
  expect_true(is.matrix(tmp))
  expect_equal(dim(tmp), c(3, 1))
  expect_true(all(tmp %in% ind_flow[, 1]))
  
  expect_type(
    tmp <- knn_get_index_year(
      flow_mat, 
      ind_flow, 
      list(k = 4, weights = c(.9, .08, 0.01, 0.01))
    ), 
    "double"
  )
  expect_true(is.matrix(tmp))
  expect_equal(dim(tmp), c(3, 1))
  expect_true(all(tmp %in% ind_flow[, 1]))
  
  # check k = 1, and weights = 1
  expect_type(
    tmp <- knn_get_index_year(
      flow_mat, 
      ind_flow, 
      list(k = 1, weights = 1)
    ), 
    "double"
  )
  expect_true(is.matrix(tmp))
  expect_equal(dim(tmp), c(3, 1))
  expect_true(all(tmp %in% ind_flow[, 1]))
  for(i in seq_len(nrow(flow_mat))) {
    expect_identical(
      tmp[i],
      ind_flow[which.min(abs(flow_mat[i, 2] - ind_flow[, 2])), 1]
    )
  }
})

