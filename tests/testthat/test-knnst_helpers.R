# agg_year_to_cal_year ----------------------
test_that("knnstdisagg:::agg_year_to_cal_year works", {
  func <- knnstdisagg:::agg_year_to_cal_year
  expect_equal(func(10, 2020, 10), 2019)
  expect_equal(func(10, 2020, 1), 2020)
  expect_equal(
    func(rep(1:12, 2), c(rep(2019, 12), rep(2020, 12)), 1),
    c(rep(2019, 12), rep(2020, 12))
  )
  expect_equal(
    func(rep(1:12, 2), c(rep(2019, 12), rep(2020, 12)), 10),
    c(rep(2019, 9), rep(2018, 3), rep(2020, 9), rep(2019, 3))
  )
  expect_equal(
    func(c(7:12, 1:6), rep(2020, 12), 7),
    c(rep(2019, 6), rep(2020, 6))
  )
})
