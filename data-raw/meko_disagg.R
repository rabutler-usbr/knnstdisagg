# script to disagg meko data and plot all graphs for all sites

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

disagg <- knn_space_time_disagg(
  ann_flow = meko,
  ann_index_flow = annual_index,
  mon_flow = monthly_data,
  start_month = 10,
  nsim = 1,
  scale_sites = 1:20,
  k_weights = knn_params_default(nrow(annual_index))
)

write_knnst(disagg, "data-raw/meko_output")

# plot and save all sites

for (site in names(monthly_data)) {
  message("site: ", site)
  p <- plot(
    disagg,
    site = site,
    base_units = "acre-feet",
    which = 1:15,
    show = FALSE
  )
  ofile <- file.path("data-raw/meko_output", paste0(site, ".pdf"))
  save_knnstplot(p, ofile, width = 8, height = 6)
}
