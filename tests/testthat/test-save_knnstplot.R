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
  start_month = 1,
  nsim = 10,
  scale_sites = 1:2
)


p1 <- plot(disagg, site = "S2")
p2 <- plot(disagg, site = "S1", which = 1:15, base_units = "kaf")
p3 <- plot(disagg, which = 2, site = "S1")

# setup dir ------------------------
temp_dir <- file.path(tempdir(), "plots")

setup(dir.create(temp_dir))
teardown(unlink(temp_dir, recursive = TRUE))

# errors ----------------------
test_that("save_knnstplot() errors", {
  expect_error(save_knnstplot(p1, "some/path/doesnt/exist.png"))
  expect_error(save_knnstplot(p2, "this.jpg"))
  expect_error(save_knnstplot(disagg, "this.png"))
})

# files are created --------------------------
test_that("save_knnstplot() creates correct files", {
  # p1 ---------------
  expect_identical(save_knnstplot(p1, file.path(temp_dir, "p1.png")), p1)
  # should be 3 files
  for (i in 1:3) {
    expect_true(file.exists(file.path(
      temp_dir,
      paste0("p1",sprintf("%02d", i) , ".png")
    )))
  }
  expect_identical(save_knnstplot(p1, file.path(temp_dir, "p1.pdf")), p1)
  expect_true(file.exists(file.path(temp_dir, "p1.pdf")))

  # p2 ------------
  expect_identical(save_knnstplot(p2, file.path(temp_dir, "plot.png")), p2)
  # should be 15 files
  for (i in 1:15) {
    expect_true(file.exists(file.path(
      temp_dir,
      paste0("plot",sprintf("%02d", i) , ".png")
    )))
  }
  expect_identical(save_knnstplot(p2, file.path(temp_dir, "plot.pdf")), p2)
  expect_true(file.exists(file.path(temp_dir, "plot.pdf")))

  # p3 -------------------
  expect_identical(save_knnstplot(p3, file.path(temp_dir, "ok.png")), p3)
  # should be 1 file
  expect_true(file.exists(file.path(temp_dir, "ok01.png")))

  expect_identical(
    save_knnstplot(p3, file.path(temp_dir, "p3.pdf"), width = 8, height = 6),
    p3
  )
  expect_true(file.exists(file.path(temp_dir, "p3.pdf")))
})
