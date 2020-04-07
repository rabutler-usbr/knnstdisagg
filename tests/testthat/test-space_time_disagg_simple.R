# small scale check -----------------------------------
# small scale, but full disagg with current data, and for a subset of the gages
# disagg based on total flow for bluff, green river ut, and cisco, and int for
# mead
library(xts)

load(file = "nf_test_data.rda")

index_flow <- nf_index_flow
mon_flow <- nf_mon_flow

lf <- cbind(2019:2021, c(9000000, 15000000, 12500000))
nsim <- 5
ym <- zoo::as.yearmon("2019-01") + 0:35/12
ym <- paste(format(ym, "%Y"), format(ym, "%m"), sep = "-")

# ** check specifying index years, and make sure values match exactly
# ** check specifiying 1, and no scale_sites
setup(dir.create("tmp_disagg"))
teardown(unlink("tmp_disagg", recursive = TRUE))

ann_sum <- function(x)
{
  do.call(
    cbind,
    lapply(
      seq_len(ncol(x)),
      function(xx) apply(matrix(x[,xx], ncol = 12, byrow = TRUE), 1, sum)
    )
  )
}

# test output ---------------------------
test_that("`knn_space_time_disagg()` output is properly created for nsim = 5", {
  expect_is(
    tmp <- knn_space_time_disagg(
      lf,
      index_flow,
      mon_flow,
      start_month = 1,
      scale_sites = 1:20,
      nsim = nsim
    ),
    "knnst"
  )
  expect_identical(write_knnst(tmp, "tmp_disagg"), tmp)

  for (i in seq_len(nsim)) {
    f1 <- file.path("tmp_disagg", paste0("disagg_", i, ".csv"))
    expect_true(file.exists(f1))
    t1 <- read.csv(f1)
    expect_identical(dim(t1), as.integer(c(36, 29)))

    # all 5 files should not be the same at the monthly level
    j <- ifelse(i == nsim, 1, i + 1)
    expect_false(
      identical(knnst_get_disagg_data(tmp, i), knnst_get_disagg_data(tmp, j)),
      info = paste(i, "compared to", j)
    )

    # but they should all sum to the same annual value for lees ferry (not LB)
    t1 <- knnst_get_disagg_data(tmp, i)
    t1 <- ann_sum(t1)
    t2 <- knnst_get_disagg_data(tmp, j)
    t2 <- ann_sum(t2)
    expect_equal(
      apply(t1[,1:20], 1, sum),
      apply(t2[,1:20], 1, sum),
      info = paste(i, "compared to", j)
    )
    # and those should sum to the input
    temp_ann <- cbind(
      c(2019:2021),
      round(apply(t1[,1:20], 1, sum), 0)
    )
    expect_identical(lf, temp_ann, info = paste("sim:", i))

    # and LB should match the natural flow data exactly
    lb <- rbind(
      as.matrix(
        mon_flow[as.character(tmp$disagg_sims[[i]]$index_years[1]), 21:29]
      ),
      as.matrix(
        mon_flow[as.character(tmp$disagg_sims[[i]]$index_years[2]), 21:29]
      ),
      as.matrix(
        mon_flow[as.character(tmp$disagg_sims[[i]]$index_years[3]), 21:29]
      )
    )
    dimnames(lb) <- NULL
    rownames(lb) <- ym
    colnames(lb) <- colnames(mon_flow)[21:29]

    expect_equal(knnst_get_disagg_data(tmp, i)[,21:29], lb)
  }


  # check index_years

  index_out <- as.matrix(read.csv(file.path("tmp_disagg", "index_years.csv")))
  expect_identical(dim(index_out), as.integer(c(3, nsim)))
  expect_true(!anyNA(index_out))
  expect_true(!anyNA(knnst_index_years(tmp)))
  expect_true(all(index_out %in% index_flow[,1]))
  expect_equal(dim(knnst_index_years(tmp)), c(nrow(lf), nsim))
  # sim
  expect_equal(knnst_nsim(tmp), nsim)
  expect_equal(expect_output(print(tmp)), tmp)
})

ind_yrs <- cbind(c(2000, 1906, 1936), c(1999, 1976, 2010), c(2000, 1909, 1954))
nsim <- 3

# specified index_years ---------------------------
test_that("`knn_space_time_disagg()` works for index years for nsim != 1", {
  expect_is(
    expect_message(tmp <- knn_space_time_disagg(
      lf,
      index_flow,
      mon_flow,
      start_month = 1,
      scale_sites = 1:20,
      nsim = nsim,
      index_years = ind_yrs
    )),
    "knnst"
  )

  expect_equal(knnst_index_years(tmp), ind_yrs)
  expect_equal(dim(knnst_index_years(tmp)), c(nrow(lf), nsim))
  # sim
  expect_equal(knnst_nsim(tmp), nsim)
  # print
  expect_equal(expect_output(print(tmp)), tmp)

  for (i in seq_len(nsim)) {
    # all sims should not be the same at the monthly level
    j <- ifelse(i == nsim, 1, i + 1)
    expect_false(
      identical(knnst_get_disagg_data(tmp, i), knnst_get_disagg_data(tmp, j)),
      info = paste(i, "compared to", j)
    )

    # but they should all sum to the same annual value for lees ferry (not LB)
    t1 <- knnst_get_disagg_data(tmp, i)
    t1 <- ann_sum(t1)
    t2 <- knnst_get_disagg_data(tmp, j)
    t2 <- ann_sum(t2)
    expect_equal(
      apply(t1[,1:20], 1, sum),
      apply(t2[,1:20], 1, sum),
      info = paste(i, "compared to", j)
    )

    # and LB should match the natural flow data exactly
    lb <- rbind(
      as.matrix(mon_flow[as.character(ind_yrs[1, i]), 21:29]),
      as.matrix(mon_flow[as.character(ind_yrs[2, i]), 21:29]),
      as.matrix(mon_flow[as.character(ind_yrs[3, i]), 21:29])
    )
    dimnames(lb) <- NULL
    rownames(lb) <- ym
    colnames(lb) <- colnames(mon_flow)[21:29]

    expect_equal(knnst_get_disagg_data(tmp, i)[,21:29], lb)
  }

  expect_equivalent(
    knnst_get_disagg_data(tmp, 1)[1:12, 15] /
      sum(knnst_get_disagg_data(tmp, 1)[1:12, 15]),
    as.vector(mon_flow[as.character(ind_yrs[1,1]), 15] /
      sum(mon_flow[as.character(ind_yrs[1,1]), 15]))
  )

  expect_equivalent(
    knnst_get_disagg_data(tmp, 2)[25:36, 18] /
      sum(knnst_get_disagg_data(tmp, 2)[25:36, 18]),
    as.vector(mon_flow[as.character(ind_yrs[3, 2]), 18] /
      sum(mon_flow[as.character(ind_yrs[3, 2]), 18]))
  )

  expect_equivalent(
    knnst_get_disagg_data(tmp, 3)[13:24, 1] /
      sum(knnst_get_disagg_data(tmp, 3)[13:24, 1]),
    as.vector(mon_flow[as.character(ind_yrs[2, 3]), 1] /
      sum(mon_flow[as.character(ind_yrs[2, 3]), 1]))
  )
})

# knnstdisagg:::full_year -------------------
test_that("knnstdisagg:::full_year() works", {
  expect_identical(knnstdisagg:::full_year(1), 1:12)
  expect_identical(knnstdisagg:::full_year(10), c(10:12, 1:9))
  expect_identical(knnstdisagg:::full_year(12), c(as.integer(12), 1:11))
  expect_identical(knnstdisagg:::full_year(6), c(6:12, 1:5))
})

# knnstdisagg:::ym_labels <- function(years, start_month) ----------------
test_that("knnstdisagg:::ym_labels works", {
  expect_identical(
    knnstdisagg:::ym_labels(2000:2001, 1),
    paste(
      c(rep(2000, 12), rep(2001, 12)),
      sprintf("%02d", rep(1:12, 2)),
      sep = "-"
    )
  )
  expect_identical(
    knnstdisagg:::ym_labels(2000:2001, 10),
    paste(
      c(rep(1999, 3), rep(2000, 12), rep(2001, 9)),
      sprintf("%02d", c(10:12, 1:12, 1:9)),
      sep = "-"
    )
  )
})

# start_year ------------------
# selecting the nearest neighbor, with the same data, but a different start_year
# should result in same output data with different rownames

flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
# made up historical data to use as index years
ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
# make up monthly flow for two sites
mon_flow <- cbind(
  rnorm(80 * 12, mean = 20, sd = 5),
  rnorm(80 * 12, mean = 120, sd = 45)
)

test_that("changing start_month does not change data", {
  expect_is(
    d1 <- knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 1,
      scale_sites = 1:2,
      k_weights = knn_params(1, 1)
    ),
    "knnst"
  )
  expect_is(
    d2 <- knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 10,
      scale_sites = 1:2,
      k_weights = knn_params(1, 1)
    ),
    "knnst"
  )
  expect_identical(knnst_index_years(d1), knnst_index_years(d2))

  t1 <- d1$disagg_sims[[1]]$disagg_flow
  expect_identical(
    rownames(t1),
    paste(
      c(rep(2000, 12), rep(2001, 12), rep(2002, 12)),
      sprintf("%02d", rep(1:12, 3)),
      sep = "-"
    )
  )

  t2 <- d2$disagg_sims[[1]]$disagg_flow
  expect_identical(
    rownames(t2),
    paste(
      c(rep(1999, 3), rep(2000, 12), rep(2001, 12), rep(2002, 9)),
      sprintf("%02d", c(10:12, 1:12, 1:12, 1:9)),
      sep = "-"
    )
  )

  rownames(t1) <- NULL
  rownames(t2) <- NULL
  expect_identical(t1, t2)
})

# scale_sites ----------------------
test_that("scale_sites works with different specifications", {
  expect_identical(
    knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 1,
      scale_sites = TRUE,
      k_weights = knn_params(1, 1)
    ),
    knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 1,
      scale_sites = 1:2,
      k_weights = knn_params(1, 1)
    )
  )
  # check that indexed sites are == between disaggs
  expect_is(
    expect_message(d1 <- knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 1,
      scale_sites = FALSE,
      k_weights = knn_params(1, 1)
    )),
    "knnst"
  )
  expect_is(
    expect_message(d2 <- knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 1,
      scale_sites = 1,
      k_weights = knn_params(1, 1)
    )),
    "knnst"
  )
  expect_is(
    expect_message(d3 <- knn_space_time_disagg(
      flow_mat,
      ind_flow,
      mon_flow,
      start_month = 1,
      scale_sites = 2,
      k_weights = knn_params(1, 1)
    )),
    "knnst"
  )
  expect_identical(
    d1$disagg_sims[[1]]$disagg_flow[,1],
    d3$disagg_sims[[1]]$disagg_flow[,1]
  )
  expect_identical(
    d1$disagg_sims[[1]]$disagg_flow[,2],
    d2$disagg_sims[[1]]$disagg_flow[,2]
  )

  # all scaled flow should exist in mon_flow
  expect_true(all(d1$disagg_sims[[1]]$disagg_flow %in% mon_flow))
  expect_true(all(d2$disagg_sims[[1]]$disagg_flow[, 2] %in% mon_flow))
  expect_true(all(d3$disagg_sims[[1]]$disagg_flow[, 1] %in% mon_flow))
  expect_false(all(d2$disagg_sims[[1]]$disagg_flow[, 1] %in% mon_flow))
  expect_false(all(d3$disagg_sims[[1]]$disagg_flow[, 2] %in% mon_flow))
})
