# setup for disagg --------------

library(xts)

load(file = "nf_test_data.rda")

index_flow <- nf_index_flow
mon_flow <- nf_mon_flow

lf <- cbind(2019:2021, c(9000000, 15000000, 12500000))
nsim <- 1
ym <- zoo::as.yearmon("2019-01") + 0:35/12
ym <- paste(format(ym, "%Y"), format(ym, "%m"), sep = "-")

disagg <- knn_space_time_disagg(
  lf,
  index_flow,
  mon_flow,
  start_month = 1,
  scale_sites = 1:20,
  nsim = nsim
)

# plots fail --------------

test_that("plots fail", {
  expect_error(
    plot(disagg, site = 1, bin_size = 2),
    "In `plot.knnst()`, `site` should be a character with length of 1.",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = "S44", bin_size = 2),
    "In `plot.knnst()`, `site` should be a valid site name.",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = c("S1", "S2"), bin_size = 2),
    "In `plot.knnst()`, `site` should be a character with length of 1.",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = "Cameo", which = NULL, bin_size = 2),
    "In `plot.knnst()`, `which` should be numeric values in 1:15",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = "Cameo", which = c(0,1), bin_size = 2),
    "In `plot.knnst()`, `which` should be numeric values in 1:15",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = "Cameo", which = "all", bin_size = 2),
    "In `plot.knnst()`, `which` should be numeric values in 1:15",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = "Cameo", show = 1, bin_size = 2),
    "In `plot.knnst()`, `show` should be a logical scalar.",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = "Cameo", show = c(TRUE, FALSE), bin_size = 2),
    "In `plot.knnst()`, `show` should be a logical scalar.",
    fixed = TRUE
  )
})

# plots - named xts ---------------------------
test_that("plots succeed for named xts data", {
  expect_is(plot(disagg, site = "Bluff", bin_size = 3), "knnstplot") %>%
    expect_length(3)
  expect_is(
    plot(disagg, site = "Greendale", which = 1:15, base_units = "kaf",
         bin_size = 3),
    "knnstplot"
  ) %>%
    expect_length(15)
  expect_is(
    p1 <- plot(disagg, which = 2, site = "Watson", bin_size = 3),
    "knnstplot"
  ) %>%
    expect_length(4)
  expect_is(
    p2 <- plot(disagg, which = c(15, 2), site = "Watson", bin_size = 3),
    "knnstplot"
  ) %>%
    expect_length(4)
  # remove environment and then compare
  p1[["Feb-pdf"]] <- NULL
  p2[["Feb-pdf"]] <- NULL
  expect_true(all.equal(p1[["Feb-pdf"]], p2[["Feb-pdf"]]))
})

# plots - unnamed matrices ---------------------------
flow_mat <- cbind(2000:2005, c(1400, 1567, 1325, 1456, 900, 2000))
# made up historical data to use as index years
ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
# make up monthly flow for two sites
mon_flow <- cbind(
  rnorm(80 * 12, mean = 20, sd = 5),
  rnorm(80 * 12, mean = 120, sd = 45)
)
disagg <- knn_space_time_disagg(
  flow_mat,
  ind_flow,
  mon_flow,
  start_month = 1,
  nsim = 1,
  scale_sites = 1:2
)

test_that("plots succeed for unnamed matrices", {
  expect_is(plot(disagg, site = "S2", bin_size = 3), "knnstplot") %>%
    expect_length(3)
  expect_is(
    plot(disagg, site = "S1", which = 1:15, base_units = "kaf", bin_size = 3),
    "knnstplot"
  ) %>%
    expect_length(15)
  expect_is(
    p1 <- plot(disagg, which = 2, site = "S1", bin_size = 4),
    "knnstplot"
  ) %>%
    expect_length(4)
  expect_is(
    p2 <- plot(disagg, which = c(15, 2), site = "S1", bin_size = 3),
    "knnstplot"
  ) %>%
    expect_length(4)
  # remove environment and then compare
  p1[["Feb-pdf"]] <- NULL
  p2[["Feb-pdf"]] <- NULL
  expect_true(all.equal(p1[["Feb-pdf"]], p2[["Feb-pdf"]]))
})

# get_agg_year() -------------------------
test_that("knnstdisagg:::get_agg_year() works", {
  expect_identical(knnstdisagg:::get_agg_year("2000-01", 1), 2000)
  expect_identical(knnstdisagg:::get_agg_year("2000-12", 1), 2000)
  expect_identical(knnstdisagg:::get_agg_year("2000-01", 10), 2000)
  expect_identical(knnstdisagg:::get_agg_year("2000-09", 10), 2000)
  expect_identical(knnstdisagg:::get_agg_year("2000-12", 10), 2001)
  expect_identical(knnstdisagg:::get_agg_year("1999-10", 10), 2000)
  expect_identical(knnstdisagg:::get_agg_year("1999-06", 7), 1999)
  expect_identical(knnstdisagg:::get_agg_year("1999-07", 7), 2000)
  # test vectorized version
  expect_identical(
    knnstdisagg:::get_agg_year(
      c("2000-01", "2000-09", "2000-12", "1999-10"),
      10
    ),
    c(2000, 2000, 2001, 2000))
})

# start_year ---------------------------
# changing start_year should not change annual stats when using k = 1
flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
# made up historical data to use as index years
ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
# make up monthly flow for two sites
mon_flow <- cbind(
  rnorm(80 * 12, mean = 20, sd = 5),
  rnorm(80 * 12, mean = 120, sd = 45)
)

test_that("changing start_month does not change data", {
  expect_is(
    d1 <- knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 1,
      scale_sites = 1:2,
      k_weights = knn_params(1, 1)
    ),
    "knnst"
  )
  expect_is(
    d2 <- knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 10,
      scale_sites = 1:2,
      k_weights = knn_params(1, 1)
    ),
    "knnst"
  )
  expect_is(p1 <- plot(d1, site = "S1", which = 15, bin_size = 3), "knnstplot")
  expect_is(p2 <- plot(d2, site = "S1", which = 15, bin_size = 3), "knnstplot")
  expect_identical(
    p1[["annual-stats"]][["data"]],
    p2[["annual-stats"]][["data"]]
  )
})
