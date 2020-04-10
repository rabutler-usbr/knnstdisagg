# create a sample data set for use in examples and tests

library(knnstdisagg)
library(CoRiverNF)

# setup annual data
annual_index <- CoRiverNF::wyAnnTot$LeesFerry
yrs <- as.numeric(format(index(wyAnnTot$LeesFerry), "%Y"))
annual_index <- as.matrix(annual_index)
annual_index <- cbind(yrs, annual_index)

# setup monthly data
last_month <- paste0("/", max(yrs), "-09")
monthly_data <- CoRiverNF::monthlyInt[last_month]

# trim to only have 200 years of data (not 1244)
meko <- meko[1:200,]

ex_disagg <- knn_space_time_disagg(
  ann_flow = meko,
  ann_index_flow = annual_index,
  mon_flow = monthly_data,
  start_month = 10,
  nsim = 1,
  scale_sites = 1:20,
  k_weights = knn_params_default(nrow(annual_index))
)

usethis::use_data(ex_disagg)
