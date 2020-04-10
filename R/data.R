#' Upper Colorado River Flow Reconstruction.
#'
#' A dataset containing paleo reconstructed hydrology for the Colorado River at
#' Lees Ferry for 762 - 2005.
#'
#' @format A matrix with 1244 rows and 2 columns:
#'   - column 1 = water_year: the water year (October - September)
#'   - column 2 = lees_ferry: reconstructed flow at Lees Ferry in acre-feet
#'
#' @references Meko, D.M., Woodhouse, C.A., Baisan, C.A., Knight, T., Lukas,
#' J.J., Hughes, M.K., and Salzer, M.W. 2007. Medieval Drought in the Upper
#' Colorado River Basin. Geophysical Research Letters 34, L10705.
#'
#' @source Description of data:
#'   \url{https://www.treeflow.info/content/colorado-r-lees-ferry-az-meko}
#'
#' @source Data download:
#'   \url{http://www.riversimulator.org/Resources/ClimateData/Meko762-2005.xls}
"meko"

#' Example disaggregated data
#'
#' An example [`knnst`] object containing results of disaggregating the first
#' 200 years in the [`meko`] data. Data were disaggregated using the
#' [CoRiverNF](https://github.com/BoulderCodeHub/CoRiverNF) package for the
#' historical annual data and monthly pattern data. Including this object as it
#' is useful for using in examples and tests.
#'
#' @format A [`knnst`] object.
#'
#' @references See
#' [data-raw](https://github.com/rabutler-usbr/knnstdisagg/data-raw) for code
#' used to generate data.
"ex_disagg"
