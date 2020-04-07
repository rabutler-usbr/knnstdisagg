
# compare to previous results ----------------------------
tmpDir <- "../dp/tmp"

dir.create(tmpDir)
teardown(unlink(tmpDir, recursive = TRUE))

# unzip the example zip file
unzip(file.path("../dp", "dp_to_compare.zip"), exdir = tmpDir)

x <- matrix(scan("../dp/Meko.txt", quiet = TRUE), ncol = 2, byrow = TRUE)
# intervening natural flow mon_flw - monthly WY text file
mon_flw <- as.matrix(read.table(
  "../dp/NFfullbasinWY0608intervening.txt",
  sep = "\t"
))

# observed annual flow for picking analog disag yr
ann_flw <- as.matrix(read.table("../dp/LFWYTotal.txt"))

# ** can I check old CRSS packages to see direct paleo files when these data
# were used. Is the mon_flw file correct for the 1906-2008 natural flows?

# ** this contains weird numbers for the Grand Canyon reach
# zz <- as.matrix(read.table("../dp/MatrixSimDataCRBwithObsLB_DP.txt"))
zz <- as.matrix(read.csv(
  file.path(tmpDir, "MatrixSimDataCRBwithObsLB_DP_rab20180620.csv")
))

attr(zz, "dimnames") <- NULL

index_yrs <- matrix(scan("../dp/indexpick.txt", quiet = TRUE), ncol = 1)

# knn_space_time_disagg <- function(x,
#                          ann_flw,
#                          mon_flw,
#                          nsite = 29,
#                          scale_sites = 1:20,
#                          nsim = 1,
#                          ofolder = NULL,
#                          index_years = NULL,
#                          k_weights = knn_params_default(n))

test_that("disagg matches previous code's results", {
  # using equivalent so the dimnames are not compared
  expect_equivalent(
    tmp <- knnst_get_disagg_data(
      expect_message(knn_space_time_disagg(
        x,
        ann_index_flow = ann_flw,
        mon_flow = mon_flw,
        start_month = 10,
        index_years = index_yrs,
        scale_sites = 1:20
      )),
      1
    ),
    zz
  )
  # now remove the dimnames and test again with equal
  attr(tmp, "dimnames") <- NULL
  # check to 5 decimals of precision
  expect_equal(round(tmp, 5), round(zz, 5))

  expect_equal(range(tmp - zz), c(0, 0))
  expect_true(max(abs(range(tmp - zz))) < 1e-5)
})

# compare random selection -----------------------------

orig_index <- as.matrix(read.csv("../dp/index_years_rseed408.csv"))
dimnames(orig_index) <- NULL
set.seed(403) # this was the first entry of .Random.seed when implementing this

tmp2 <- knn_space_time_disagg(
  x,
  ann_flw,
  mon_flw,
  start_month = 10,
  scale_sites = 1:20
)

test_that("current random selection matches original random selection", {
  expect_equal(tmp2$disagg_sims[[1]]$index_years, as.vector(orig_index))
  expect_equal(knnst_index_years(tmp2), orig_index)
  set.seed(403)
  expect_equal(knn_get_index_year(x, ann_flw), orig_index)
})

test_that("nsim and index_years are correct", {
  expect_equal(knnst_nsim(tmp2), 1)
  expect_equal(dim(knnst_index_years(tmp2)), c(nrow(x), 1))
})

# check knn_space_time_disagg errros -----------------------------
test_that("`knn_space_time_disagg()` errors correctly", {
  expect_error(knn_space_time_disagg(x[,1], ann_flw, mon_flw, start_month = 1))
  expect_error(knn_space_time_disagg(x, ann_flw[,2], mon_flw, start_month = 1))
  expect_error(
    knn_space_time_disagg(x, ann_flw, mon_flw[1:60,], start_month = 1),
    "`ann_index_flow` and `mon_flow` must have the same number of years.",
    fixed = TRUE
  )
  expect_error(
    knn_space_time_disagg(x, ann_flw[1:60,], mon_flw, start_month = 10),
    "`ann_index_flow` and `mon_flow` must have the same number of years.",
    fixed = TRUE
  )
  expect_error(
    knn_space_time_disagg(x, ann_flw, mon_flw[1:143,], start_month = 10),
    "`mon_flow` needs to have an even year's worth of data",
    fixed = TRUE
  )
  expect_error(
    knn_space_time_disagg(x, ann_flw, mon_flw),
    "argument \"start_month\" is missing, with no default",
    fixed = TRUE
  )
  expect_error(
    knn_space_time_disagg(x, ann_flw, mon_flw, start_month = c(1,10)),
    "`start_month` should be a single integer from 1 to 12",
    fixed = TRUE
  )
  expect_error(
    knn_space_time_disagg(x, ann_flw, mon_flw, start_month = 15),
    "`start_month` should be a single integer from 1 to 12",
    fixed = TRUE
  )
})


# knnstdisagg:::get_scale_factor() -----------------------------
index_flow <- cbind(2000:2002, c(1000, 1100, 900))

test_that("`knnstdisagg:::get_scale_factor()` errors correctly", {
  expect_error(knnstdisagg:::get_scale_factor(2000, c(1000,2000), index_flow))
  expect_error(knnstdisagg:::get_scale_factor(2000:2001, 1000, index_flow))
  expect_error(knnstdisagg:::get_scale_factor(2000, 950, 2000:2002))
  expect_error(knnstdisagg:::get_scale_factor(1999, 950, index_flow))
})

test_that("`knnstdisagg:::get_scale_factor()` returns correctly", {
  expect_type(
    tmp <- knnstdisagg:::get_scale_factor(2000, 950, index_flow),
    "list"
  )
  expect_identical(tmp, list(pos = as.integer(1), SF = 950/1000))
  expect_identical(
    knnstdisagg:::get_scale_factor(2002, 950, index_flow),
    list(pos = as.integer(3), SF = 950/900)
  )
  expect_identical(
    knnstdisagg:::get_scale_factor(2001, 1100, index_flow),
    list(pos = as.integer(2), SF = 1100/1100)
  )
})
