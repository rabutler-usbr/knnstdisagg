
# errors --------------
test_that("knnst_sp_cor() errors correctly.", {
  expect_error(knnst_sp_cor(data.frame(1:10), "Cameo", 1))
  expect_error(knnst_sp_cor(ex_disagg, "Cameo", c(1, 2)))
  expect_error(knnst_sp_cor(ex_disagg, "Cameo", "weird"))
})

# TODO: add tests for unnamed disagg data
# it works -------------
test_that("knnst_sp_cor() works.", {
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
