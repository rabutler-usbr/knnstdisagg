library(xts)
context("Test `as.data.frame.knnst`")

load(file = "nf_test_data.rda")

index_flow <- nf_index_flow
mon_flow <- nf_mon_flow

lf <- cbind(2019:2021, c(9000000, 15000000, 12500000))
nsim <- 3

expect_message(knnst <- knn_space_time_disagg(
  lf,
  index_flow,
  mon_flow,
  scale_sites = 1:20,
  nsim = nsim
))

add_cols <- c("ym", "year", "month", "simulation", "index_year")

# test on full sample ---------------------------
test_that("data in data.frame matches data in knnst", {
  expect_is(df <- as.data.frame(knnst), "data.frame")
  expect_equal(ncol(df), 5 + ncol(knnst_get_disagg_data(knnst, 1)))
  expect_true(all(add_cols %in% names(df)))
  expect_true(all(unique(df$simulation) %in% 1:nsim))
  expect_true(all(1:nsim %in% unique(df$simulation)))

  # get the added column indeces
  add_cols_i <- match(add_cols, colnames(df))

  for (i in seq_len(nsim)) {
    expect_equivalent(
      knnst_get_disagg_data(knnst, i),
      as.matrix(df[df$simulation == i, -add_cols_i])
    )
    expect_equal(
      rownames(knnst_get_disagg_data(knnst, i)),
      df[df$simulation == i,]$ym
    )
    expect_equal(
      knnst$disagg_sims[[i]]$index_years,
      df[df$simulation == i,]$index_year[seq(12, nrow(lf) * 12, 12)]
    )
  }

  expect_equal(
    df$ym,
    paste(df$year, formatC(df$month, width = 2, flag = "0"), sep = "-")
  )
})


# test with only 1 simulation ------------------------

flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
# made up historical data to use as index years
ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
# make up monthly flow for two sites
mon_flow <- cbind(
  rnorm(80 * 12, mean = 20, sd = 5),
  rnorm(80 * 12, mean = 120, sd = 45)
)
nsim <- 1
knnst <- knn_space_time_disagg(
  flow_mat,
  ind_flow,
  mon_flow,
  scale_sites = 1:2,
  nsim = 1
)

test_that("data in data.frame matches data in knnst", {
  expect_is(df <- as.data.frame(knnst), "data.frame")
  expect_equal(ncol(df), 5 + ncol(knnst_get_disagg_data(knnst, 1)))
  expect_true(all(add_cols %in% names(df)))
  expect_true(all(unique(df$simulation) %in% 1:nsim))
  expect_true(all(1:nsim %in% unique(df$simulation)))

  # get the added column indeces
  add_cols_i <- match(add_cols, colnames(df))

  for (i in seq_len(nsim)) {
    expect_equivalent(
      knnst_get_disagg_data(knnst, i),
      as.matrix(df[df$simulation == i, -add_cols_i])
    )
    expect_equal(
      rownames(knnst_get_disagg_data(knnst, i)),
      df[df$simulation == i,]$ym
    )
    expect_equal(
      knnst$disagg_sims[[i]]$index_years,
      df[df$simulation == i,]$index_year[seq(12, nrow(lf) * 12, 12)]
    )
  }

  expect_equal(
    df$ym,
    paste(df$year, formatC(df$month, width = 2, flag = "0"), sep = "-")
  )
})

