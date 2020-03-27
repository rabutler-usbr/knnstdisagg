# This only checks the errors in sending bad parameters to write_knnst()
# and that the files get created.
# See test_space_time_disagg_simple.R for more tests of the data created by
# calling this function with actual disagg data

# setup ----------------------
flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
# made up historical data to use as index years
ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
# make up monthly flow for two sites
mon_flow <- cbind(
  rnorm(80 * 12, mean = 20, sd = 5),
  rnorm(80 * 12, mean = 120, sd = 45)
)
disagg <- knn_space_time_disagg(
  flow_mat, ind_flow, mon_flow, scale_sites = 1:2, nsim = 5
)

# check errors --------------
test_that("write_knnst() errors expectedly", {
  expect_error(
    write_knnst(data.frame(a = 1:12), tempdir()),
    "`disagg` should be a `knnst` object.",
    fixed = TRUE
  )
  expect_error(
    write_knnst(disagg, 1),
    "`path` should be a character scalar.",
    fixed = TRUE
  )
  expect_error(
    write_knnst(disagg, c("path", "two")),
    "`path` should be a character scalar.",
    fixed = TRUE
  )
  expect_error(
    write_knnst(disagg, "non/existing/path"),
    "`path` should exist.",
    fixed = TRUE
  )
})

# check files exist ---------
test_that("write_knnst() creates correct files", {
  expect_identical(write_knnst(disagg, tempdir()), disagg)
  for (n in 1:5) {
    expect_true(file.exists(file.path(tempdir(), paste0("disagg_", n, ".csv"))))
  }
  expect_true(file.exists(file.path(tempdir(), "index_years.csv")))
})
