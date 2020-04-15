# knnstdisagg 0.1.0.9000

**In development**

* `ex_disagg` is now an exported data object. It contains an example `knnst` object for use in examples and tests.
* Changed how unnamed input `mon_flow` is treated. Now the columns (sites) are renamed in `knn_space_time_disagg()`, instead of in `as.data.frame.knnst()`. #33
* Added computation of (`knnst_spatial_cor()`) and plotting for (`plot.knnst_spcor()`) spatial correlation between sites. #29
* Added computation of (`knnst_temporal_cor()`) and plotting for (`plot.knnst_tmpcor()`) temporal correlation between months at a site. #30
* Fixed bug related to how lag-1 correlation was computed for the historical/pattern data. #27
* Added `random_seed` argument to `knn_space_time_disagg()` and `knn_get_index_year()` to assist with reproducibility. #28

# knnstdisagg 0.1.0

**Released April 7, 2020**

First release of package. Includes annual to monthly disaggregation and plots for QA/QC + diagnostics.
