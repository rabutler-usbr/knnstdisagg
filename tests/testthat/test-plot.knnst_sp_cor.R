test_that("plot.knnst_sp_cor() works", {
  expect_is(
    zz <- knnst_sp_cor(ex_disagg, c("Cameo", "Hoover"), 50),
    "knnst_spcor"
  )
  expect_is(gg1 <- plot(zz), c("gg", "ggplot"))
  expect_is(gg2 <- plot(zz, x_names = TRUE), c("gg", "ggplot"))
  gb1 <- ggplot2::ggplot_build(gg1)
  gb2 <- ggplot2::ggplot_build(gg2)
  expect_equal(gb1$data, gb2$data)

  expect_is(gg3 <- plot(zz, ncol = 2), c("gg", "ggplot"))
  gb3 <- ggplot2::ggplot_build(gg3)
  expect_equal(gb1$data, gb3$data)

  expect_equal(unique(gb1$layout$layout$COL), 1)
  expect_equal(unique(gb1$layout$layout$ROW), c(1, 2))

  expect_equal(unique(gb3$layout$layout$COL), c(1,2))
  expect_equal(unique(gb3$layout$layout$ROW), 1)
})
