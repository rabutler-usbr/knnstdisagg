# errors ------------
test_that("knnst_temporal_cor() errors", {
  expect_error(knnst_temporal_cor(data.frame(a = 1:12), "Cameo", 10))
  expect_error(knnst_temporal_cor(ex_disagg, "CAMEO", 50))
  expect_error(knnst_temporal_cor(ex_disagg, c("Camoe", "Hoover"), 50))
  expect_error(knnst_temporal_cor(ex_disagg, 1, 50))
  expect_error(knnst_temporal_cor(ex_disagg, "Cameo", c(50, 20)))
  expect_error(knnst_temporal_cor(ex_disagg, "Cameo", c(5000)))
  expect_error(knnst_temporal_cor(ex_disagg, "Cameo", 1))
})

# works with named input ----------------
test_that("knnst_temporal_cor() works with named input.", {
  # randomly select the site
  site <- sample(colnames(ex_disagg$mon_flow), 1)
  expect_is(
    zz <- knnst_temporal_cor(ex_disagg, site, 50),
    "knnst_tmpcor"
  )

  expect_setequal(
    names(zz),
    c("disagg_cor", "pattern_cor", "bin_size", "site")
  )

  expect_is(dc <- zz[["disagg_cor"]], "data.frame")
  expect_is(pc <- zz[["pattern_cor"]], "data.frame")
  expect_identical(zz[["site"]], site)
  expect_identical(zz[["bin_size"]], 50)

  expect_setequal(unique(as.character(dc$month1)), month.abb)
  expect_setequal(unique(as.character(dc$month2)), month.abb)
  expect_setequal(unique(as.character(pc$month1)), month.abb)
  expect_setequal(unique(as.character(pc$month2)), month.abb)

  # there are 78 unique combinations of months, including identical pairs
  expect_identical(dim(dc), as.integer(c(78*(200-50+1), 3)))

  expect_identical(dim(pc), as.integer(c(78, 3)))

  # cor with self should be 1
  expect_equal(
    dplyr::filter(pc, month1 == month2)$cor,
    rep(1, 12)
  )
  expect_equal(
    dplyr::filter(dc, month1 == month2)$cor,
    rep(1, 12*(200-50+1))
  )
})

# works with unnamed input --------------
flow_mat <- cbind(c(2000:2005), c(1400, 1567, 1325, 2200, 900, 1500))
# made up historical data to use as index years
ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
# make up monthly flow for two sites
mon_flow <- cbind(
  rnorm(80 * 12, mean = 20, sd = 5),
  rnorm(80 * 12, mean = 120, sd = 45),
  rnorm(80 * 12, mean = 44, sd = 7)
)
xx <- knn_space_time_disagg(flow_mat, ind_flow, mon_flow, 1, scale_sites = 1:2)

test_that("knnst_temporal_cor() works with unnamed input.", {
  expect_is(
    zz <- knnst_temporal_cor(xx, "S3", 3),
    "knnst_tmpcor"
  )

  expect_setequal(
    names(zz),
    c("disagg_cor", "pattern_cor", "bin_size", "site")
  )

  expect_is(dc <- zz[["disagg_cor"]], "data.frame")
  expect_is(pc <- zz[["pattern_cor"]], "data.frame")
  expect_identical(zz[["site"]], "S3")
  expect_identical(zz[["bin_size"]], 3)

  expect_setequal(unique(as.character(dc$month1)), month.abb)
  expect_setequal(unique(as.character(dc$month2)), month.abb)
  expect_setequal(unique(as.character(pc$month1)), month.abb)
  expect_setequal(unique(as.character(pc$month2)), month.abb)

  # there are 78 unique combinations of months, including identical pairs
  expect_identical(dim(dc), as.integer(c(78*(6-3+1), 3)))

  expect_identical(dim(pc), as.integer(c(78, 3)))

  # cor with self should be 1
  expect_equal(
    dplyr::filter(pc, month1 == month2)$cor,
    rep(1, 12)
  )
  expect_equal(
    dplyr::filter(dc, month1 == month2)$cor,
    rep(1, 12*(6-3+1))
  )

  # and try it with one other site
  expect_is(knnst_temporal_cor(xx, "S1", 4), "knnst_tmpcor")
})
