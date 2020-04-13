# knnstdisagg 0.1.0.9000

**In development**

* `ex_disagg` is now an exported data object. It contains an example `knnst` object for use in examples and tests.
* Changed how unnamed input `mon_flow` is treated. Now the columns (sites) are renamed in `knn_space_time_disagg()`, instead of in `as.data.frame.knnst()`. #33
* Added computation of (`knnst_sp_cor()`) and plotting for (`plot.knnst_spcor()`) spatial correlation between sites. #29

# knnstdisagg 0.1.0

**Released April 7, 2020**

First release of package. Includes annual to monthly disaggregation and plots for QA/QC + diagnostics.
