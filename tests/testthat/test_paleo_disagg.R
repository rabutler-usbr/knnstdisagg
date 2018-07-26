context("check paleo space time disagg code")

# compare to previous results ----------------------------

tmpDir <- "../dp/tmp"

dir.create(tmpDir)
teardown(unlink(tmpDir, recursive = T))

# unzip the example nc file
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
 
index_yrs <- matrix(scan("../dp/indexpick.txt", quiet = TRUE), ncol = 1)

# paleo_disagg <- function(x, 
#                          ann_flw, 
#                          mon_flw, 
#                          nsite = 29, 
#                          sf_sites = 1:20,
#                          nsim = 1,
#                          ofolder = NULL, 
#                          index_years = NULL,
#                          k_weights = NULL)

test_that("disagg matches previous code's results", {
  expect_equivalent(
    tmp <- paleo_disagg(
      x, 
      ann_index_flow = ann_flw, 
      mon_flw = mon_flw, 
      index_years = index_yrs)$paleo_disagg[[1]],
    zz,
    tolerance = 0.00001
  )
  expect_equivalent(round(tmp, 0), round(zz, 0))
  expect_equal(range(tmp - zz), c(0, 0))
})

# compare random selection -----------------------------

orig_index <- as.matrix(read.csv("../dp/index_years_rseed408.csv"))
dimnames(orig_index) <- NULL
set.seed(403) # this was the first entry of .Random.seed when implementing this

test_that("current random selection matches original random selection", {
  expect_equal(paleo_disagg(x, ann_flw, mon_flw)$index_years, orig_index)
  set.seed(403)
  expect_equal(knn_get_index_year(x, ann_flw), orig_index)
})

# check paleo_disagg errros -----------------------------
test_that("`paleo_disagg()` errors correctly", {
  expect_error(
    paleo_disagg(
      x, 
      ann_flw, 
      mon_flw, 
      index_years = orig_index, 
      k_weights = list(k = 1, weights = 1)
    ),
    "If specifying `index_years`, there is no need to specify `k_weights`",
    fixed = TRUE
  )
  expect_error(paleo_disagg(x[,1], ann_flw, mon_flw))
  expect_error(paleo_disagg(x, ann_flw[,2], mon_flw))
  expect_error(
    paleo_disagg(x, ann_flw, mon_flw[1:60,]),
    "`ann_index_flow` and `mon_flw` must have the same number of years.",
    fixed = TRUE
  )
  expect_error(
    paleo_disagg(x, ann_flw[1:60,], mon_flw),
    "`ann_index_flow` and `mon_flw` must have the same number of years.",
    fixed = TRUE
  )
  expect_error(
    paleo_disagg(x, ann_flw, mon_flw[1:143,]),
    "`mon_flw` needs to have an even year's worth of data",
    fixed = TRUE
  )
  expect_error(
    paleo_disagg(x, ann_flw, mon_flw[, 1:28]),
    "`mon_flow` needs to have `nsite` columns.",
    fixed = TRUE
  )
  expect_error(
    paleo_disagg(x, ann_flw, mon_flw, nsite = 30),
    "`mon_flow` needs to have `nsite` columns.",
    fixed = TRUE
  )
  expect_error(
    paleo_disagg(x, ann_flw, mon_flw, nsite = 20),
    "`mon_flow` needs to have `nsite` columns.",
    fixed = TRUE
  )
})


# check get_scale_factor() -----------------------------

context("`CRSSIO:::get_scale_factor()`")

index_flow <- cbind(2000:2002, c(1000, 1100, 900))

test_that("`get_scale_factor()` errors correctly", {
  expect_error(CRSSIO:::get_scale_factor(2000, c(1000,2000), index_flow))
  expect_error(CRSSIO:::get_scale_factor(2000:2001, 1000, index_flow))
  expect_error(CRSSIO:::get_scale_factor(2000, 950, 2000:2002))
  expect_error(CRSSIO:::get_scale_factor(1999, 950, index_flow))
})

test_that("`get_scale_factor()` returns correctly", {
  expect_type(tmp <- CRSSIO:::get_scale_factor(2000, 950, index_flow), "list")
  expect_identical(tmp, list(pos = as.integer(1), SF = 950/1000))
  expect_identical(
    CRSSIO:::get_scale_factor(2002, 950, index_flow), 
    list(pos = as.integer(3), SF = 950/900)
  )
  expect_identical(
    CRSSIO:::get_scale_factor(2001, 1100, index_flow), 
    list(pos = as.integer(2), SF = 1100/1100)
  )
})


# ***** still need to make function much safer to the format of incoming data,
# i.e., which input need years associated with them, and which don't, matrices, 
# vs. vectors, etc.

# Should also consider round to nearest AF, but what are the effects of that on 
# matching the inut Lees Ferry value

# should check that multiple simulations work; also need to check that multiple
# simulations when specifying index_years works

# should check that the mon_flow is either specified on water year vs. cy, or 
# somehow check that
# - not sure if I can; might be up to the user

# should error if index_years and k_weights are specified by user

# need to check that the monthly data for all gages sums to the annual data for 
# the flow to disaggregate, but must check the appropriate gage.