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
