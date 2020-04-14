test_that("plot.knnst_tmpcor() works", {
  expect_is(
    zz <- knnst_temporal_cor(ex_disagg, "Hoover", 50),
    "knnst_tmpcor"
  )
  expect_is(gg1 <- plot(zz), c("gg", "ggplot"))
  gb1 <- ggplot2::ggplot_build(gg1)
  expect_identical(gb1$layout$layout$COL, rep(1L, 12))
  expect_setequal(gb1$layout$layout$ROW, 1:12)
})
