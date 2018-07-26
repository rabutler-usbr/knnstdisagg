# small scale check -----------------------------------
# small scale, but full disagg with current data, and for a subset of the gages
# disagg based on total flow for bluff, green river ut, and cisco, and int for 
# mead
context("small, but full scale test of `paleo_disagg()`")

index_flow <- cbind(1906:2015, as.matrix(CoRiverNF::cyAnnTot$LeesFerry))

mon_flow <- CoRiverNF::monthlyInt["1906/"]

lf <- cbind(2019:2021, c(9000000, 15000000, 12500000))
nsim <- 5

# ** check specifying index years, and make sure values match exactly
# ** check specifiying 1, and no sf_sites
setup(dir.create("tmp_disagg"))
teardown(unlink("tmp_disagg", recursive = TRUE))

test_that("`paleo_disagg()` output is properly created for nsim = 5", {
  expect_type(
    tmp <- paleo_disagg(
      lf, 
      index_flow, 
      mon_flow, 
      nsite = 29, 
      sf_sites = 1:20, 
      nsim = nsim, 
      ofolder = "tmp_disagg"
    ),
    "list"
  )
  for (i in seq_len(nsim)) {
    f1 <- file.path("tmp_disagg", paste0("paleo_disagg_", i, ".csv"))
    expect_true(file.exists(f1))
    t1 <- read.csv(f1)
    expect_identical(dim(t1), as.integer(c(36, 29)))
    
    # all 5 files should not be the same at the monthly level
    j <- ifelse(i == nsim, 1, i + 1)
    expect_false(
      identical(tmp$paleo_disagg[[i]], tmp$paleo_disagg[[j]]),
      info = paste(i, "compared to", j)
    )
    
    # but they should all sum to the same annual value for lees ferry (not LB)
    t1 <- tmp$paleo_disagg[[i]]
    attr(t1, "timespan") <- c(start = "2019-1-31", end = "2021-12-31")
    t1 <- RWDataPlyr::rwslot_annual_sum(t1)
    t2 <- tmp$paleo_disagg[[j]]
    attr(t2, "timespan") <- c(start = "2019-1-31", end = "2021-12-31")
    t2 <- RWDataPlyr::rwslot_annual_sum(t2)
    expect_equal(
      apply(t1[,1:20], 1, sum),
      apply(t2[,1:20], 1, sum),
      info = paste(i, "compared to", j)
    )
    
    # and LB should match the natural flow data exactly
    lb <- rbind(
      as.matrix(mon_flow[as.character(tmp$index_years[1, i]), 21:29]),
      as.matrix(mon_flow[as.character(tmp$index_years[2, i]), 21:29]),
      as.matrix(mon_flow[as.character(tmp$index_years[3, i]), 21:29])
    )
    dimnames(lb) <- NULL
    
    expect_equal(tmp$paleo_disagg[[i]][,21:29], lb)
  }
  
  
  # check index_years
  
  index_out <- as.matrix(read.csv(file.path("tmp_disagg", "index_years.csv")))
  expect_identical(dim(index_out), as.integer(c(3, nsim)))
  expect_true(!anyNA(index_out))
  expect_true(!anyNA(tmp$index_years))
  expect_true(all(index_out %in% index_flow[,1]))
})

ind_yrs <- cbind(c(2000, 1906, 1936), c(1999, 1976, 2010), c(2000, 1909, 1954))
nsim <- 3

test_that("`paleo_disagg()` works for specifying index years for nsim != 1", {
  expect_type(
    tmp <- paleo_disagg(
      lf, 
      index_flow, 
      mon_flow, 
      nsite = 29, 
      sf_sites = 1:20, 
      nsim = nsim, 
      index_years = ind_yrs
    ),
    "list"
  )
  
  expect_equal(tmp$index_years, ind_yrs)
  
  for (i in seq_len(nsim)) {
    # all sims should not be the same at the monthly level
    j <- ifelse(i == nsim, 1, i + 1)
    expect_false(
      identical(tmp$paleo_disagg[[i]], tmp$paleo_disagg[[j]]),
      info = paste(i, "compared to", j)
    )
    
    # but they should all sum to the same annual value for lees ferry (not LB)
    t1 <- tmp$paleo_disagg[[i]]
    attr(t1, "timespan") <- c(start = "2019-1-31", end = "2021-12-31")
    t1 <- RWDataPlyr::rwslot_annual_sum(t1)
    t2 <- tmp$paleo_disagg[[j]]
    attr(t2, "timespan") <- c(start = "2019-1-31", end = "2021-12-31")
    t2 <- RWDataPlyr::rwslot_annual_sum(t2)
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
    
    expect_equal(tmp$paleo_disagg[[i]][,21:29], lb)
  }
  
  expect_equivalent(
    tmp$paleo_disagg[[1]][1:12, 15] / sum(tmp$paleo_disagg[[1]][1:12, 15]),
    as.vector(mon_flow[as.character(ind_yrs[1,1]), 15] / 
      sum(mon_flow[as.character(ind_yrs[1,1]), 15]))
  )
  
  expect_equivalent(
    tmp$paleo_disagg[[2]][25:36, 18] / sum(tmp$paleo_disagg[[2]][25:36, 18]),
    as.vector(mon_flow[as.character(ind_yrs[3, 2]), 18] / 
      sum(mon_flow[as.character(ind_yrs[3, 2]), 18]))
  )
  
  expect_equivalent(
    tmp$paleo_disagg[[3]][13:24, 1] / sum(tmp$paleo_disagg[[3]][13:24, 1]),
    as.vector(mon_flow[as.character(ind_yrs[2, 3]), 1] / 
      sum(mon_flow[as.character(ind_yrs[2, 3]), 1]))
  )
})
