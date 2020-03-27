# setup for disagg --------------

library(xts)

load(file = "nf_test_data.rda")

index_flow <- nf_index_flow
mon_flow <- nf_mon_flow

lf <- cbind(2019:2021, c(9000000, 15000000, 12500000))
nsim <- 5
ym <- zoo::as.yearmon("2019-01") + 0:35/12
ym <- paste(format(ym, "%Y"), format(ym, "%m"), sep = "-")

# ** check specifying index years, and make sure values match exactly
# ** check specifiying 1, and no sf_sites
setup(dir.create("tmp_disagg"))
teardown(unlink("tmp_disagg", recursive = TRUE))

disagg <- knn_space_time_disagg(
  lf,
  index_flow,
  mon_flow,
  scale_sites = 1:20,
  nsim = nsim
)

# plots fail --------------

test_that("plots fail", {
  expect_error(
    plot(disagg, site = 1),
    "In `plot.knnst()`, `site` should be a character with length of 1.",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = "S44"),
    "In `plot.knnst()`, `site` should be a valid site name.",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, site = c("S1", "S2")),
    "In `plot.knnst()`, `site` should be a character with length of 1.",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, which = NULL),
    "In `plot.knnst()`, `which` should be numeric values in 1:15",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, which = c(0,1)),
    "In `plot.knnst()`, `which` should be numeric values in 1:15",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, which = "all"),
    "In `plot.knnst()`, `which` should be numeric values in 1:15",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, show = 1),
    "In `plot.knnst()`, `show` should be a logical scalar.",
    fixed = TRUE
  )
  expect_error(
    plot(disagg, show = c(TRUE, FALSE)),
    "In `plot.knnst()`, `show` should be a logical scalar.",
    fixed = TRUE
  )
})

# plots - named xts ---------------------------
test_that("plots succeed for named xts data", {
  expect_is(plot(disagg, site = "Bluff"), "knnstplot") %>% expect_length(3)
  expect_is(
    plot(disagg, site = "Greendale", which = 1:15, base_units = "kaf"),
    "knnstplot"
  ) %>%
    expect_length(15)
  expect_is(p1 <- plot(disagg, which = 2, site = "Watson"), "knnstplot") %>%
    expect_length(4)
  expect_is(
    p2 <- plot(disagg, which = c(15, 2), site = "Watson"),
    "knnstplot"
  ) %>%
    expect_length(4)
  # remove environment and then compare
  p1[["Feb-cdf"]] <- NULL
  p2[["Feb-cdf"]] <- NULL
  expect_true(all.equal(p1[["Feb-cdf"]], p2[["Feb-cdf"]]))
})

# plots - unnamed matrices ---------------------------
flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
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
  nsim = 10,
  scale_sites = 1:2
)

test_that("plots succeed for unnamed matrices", {
  expect_is(plot(disagg, site = "S2"), "knnstplot") %>% expect_length(3)
  expect_is(
    plot(disagg, site = "S1", which = 1:15, base_units = "kaf"),
    "knnstplot"
  ) %>%
    expect_length(15)
  expect_is(p1 <- plot(disagg, which = 2, site = "S1"), "knnstplot") %>%
    expect_length(4)
  expect_is(
    p2 <- plot(disagg, which = c(15, 2), site = "S1"),
    "knnstplot"
  ) %>%
    expect_length(4)
  # remove environment and then compare
  p1[["Feb-cdf"]] <- NULL
  p2[["Feb-cdf"]] <- NULL
  expect_true(all.equal(p1[["Feb-cdf"]], p2[["Feb-cdf"]]))
})
