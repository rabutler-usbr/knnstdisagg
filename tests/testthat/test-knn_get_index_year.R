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
    "`k_weights` should be a `knn_params` object"
  )

  expect_error(
    knn_get_index_year(
      flow_mat,
      ind_flow,
      list(k = 2, weights = c(0.6, 0.4), z = 1)
    ),
    "`k_weights` should be a `knn_params` object"
  )

  expect_error(
    knn_get_index_year(
      flow_mat,
      ind_flow[1:3,],
      knn_params(k = 4, weights = rep(0.25, 4))
    ),
    "`k_weights$k` should be <= the number of potential index years.",
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
      knn_params(k = 4, weights = rep(0.25, 4))
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
      knn_params(k = 4, weights = c(.9, .08, 0.01, 0.01))
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
      knn_params(k = 1, weights = 1)
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

# random_seed -------------------------
test_that("random_seed is reproducible and different", {
  i1 <- knn_get_index_year(flow_mat, ind_flow, random_seed = 4567)
  i2 <- knn_get_index_year(flow_mat, ind_flow)
  i3 <- knn_get_index_year(flow_mat, ind_flow, random_seed = 4567)
  i4 <- knn_get_index_year(flow_mat, ind_flow)
  i5 <- knn_get_index_year(flow_mat, ind_flow)
  i6 <- knn_get_index_year(flow_mat, ind_flow, random_seed = 4568)
  i7 <- knn_get_index_year(flow_mat, ind_flow, random_seed = 4567)

  expect_identical(i1, i3)
  expect_identical(i1, i7)
  expect_identical(i2, i4)
  expect_false(isTRUE(all.equal(i1, i2)))
  expect_false(isTRUE(all.equal(i3, i4)))
  expect_false(isTRUE(all.equal(i3, i5)))
  expect_false(isTRUE(all.equal(i4, i5)))
  expect_false(isTRUE(all.equal(i1, i6)))
})
