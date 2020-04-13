
# errors --------------
test_that("knnst_sp_cor() errors correctly.", {
  expect_error(knnst_sp_cor(data.frame(1:10), "Cameo", 1))
  expect_error(knnst_sp_cor(ex_disagg, "Cameo", c(1, 2)))
  expect_error(knnst_sp_cor(ex_disagg, "Cameo", "weird"))
})

# works w/named input -------------
test_that("knnst_sp_cor() works with named input.", {
  expect_is(
    zz <- knnst_sp_cor(ex_disagg, c("Cameo", "Hoover"), 50),
    "knnst_spcor"
  )

  expect_setequal(names(zz), c("disagg_cor", "pattern_cor", "orig_sites"))
  expect_setequal(colnames(ex_disagg$mon_flow), zz[["orig_sites"]])
  expect_is(dc <- zz[["disagg_cor"]], "data.frame")
  expect_is(pc <- zz[["pattern_cor"]], "data.frame")

  expect_identical(unique(dc$site_from), c("Cameo", "Hoover"))
  expect_setequal(unique(dc$site_to), colnames(ex_disagg$mon_flow))
  expect_identical(dim(dc), as.integer(c(2*29*(200-50+1), 3)))

  expect_identical(unique(pc$site_from), c("Cameo", "Hoover"))
  expect_setequal(unique(dc$site_to), colnames(ex_disagg$mon_flow))
  expect_identical(dim(pc), as.integer(c(2 * 29, 3)))

  # cor with self should be 1
  expect_equal(
    dplyr::filter(pc, site_from == "Cameo", site_to == "Cameo")$cor,
    1
  )
  expect_equal(
    dplyr::filter(dc, site_from == "Cameo", site_to == "Cameo")$cor,
    rep(1, 200-50+1)
  )
})

# works w/unnamed input --------------------------
flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
# made up historical data to use as index years
ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
# make up monthly flow for two sites
mon_flow <- cbind(
  rnorm(80 * 12, mean = 20, sd = 5),
  rnorm(80 * 12, mean = 120, sd = 45),
  rnorm(80 * 12, mean = 44, sd = 7)
)
xx <- knn_space_time_disagg(flow_mat, ind_flow, mon_flow, 1, scale_sites = 1:2)

test_that("knnst_sp_cor() works with named input.", {
  expect_is(
    zz <- knnst_sp_cor(xx, c("S1", "S3"), 1),
    "knnst_spcor"
  )

  expect_setequal(names(zz), c("disagg_cor", "pattern_cor", "orig_sites"))
  expect_setequal(colnames(xx$mon_flow), zz[["orig_sites"]])
  expect_is(dc <- zz[["disagg_cor"]], "data.frame")
  expect_is(pc <- zz[["pattern_cor"]], "data.frame")

  expect_identical(unique(dc$site_from), c("S1", "S3"))
  expect_setequal(unique(dc$site_to), colnames(xx$mon_flow))
  expect_identical(dim(dc), as.integer(c(2*3*(3-1+1), 3)))

  expect_identical(unique(pc$site_from), c("S1", "S3"))
  expect_setequal(unique(dc$site_to), colnames(xx$mon_flow))
  expect_identical(dim(pc), as.integer(c(2 * 3, 3)))

  # cor with self should be 1
  expect_equal(
    dplyr::filter(pc, site_from == "S1", site_to == "S1")$cor,
    1
  )
  expect_equal(
    dplyr::filter(dc, site_from == "S3", site_to == "S3")$cor,
    rep(1, 3-1+1)
  )
})
