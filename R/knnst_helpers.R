#' Test if the object inherits from  `knnst` class
#'
#' @param x An object
#'
#' @return `TRUE` if the object inherits from the `knnst` class.
#' @export

is_knnst <- function(x)
{
  inherits(x, "knnst")
}

validate_knnst <- function(x)
{
  assertthat::assert_that(
    length(x) > 0,
    msg = "Should have length > 0"
  )

  assertthat::assert_that(
    all(sapply(x, length) %in% 2),
    msg = "Each entry should only have a length of 2."
  )

  assertthat::assert_that(
    all(sapply(x, names) %in% c("disagg_flow", "index_years")),
    msg = "All names of entries should be disagg_flow and index_years."
  )

  # check that each matrix has 12*number of index years
  nmonths <- sapply(seq_len(length(x)), function(i) nrow(x[[i]]$disagg_flow))
  nyears <- sapply(seq_len(length(x)), function(i) length(x[[i]]$index_years))

  assertthat::assert_that(
    all(nyears * 12 == nmonths),
    msg = "Months in `disagg_flow` should equal 12 * number of `index_years`"
  )

  x
}

#' Get the number of diaggregation simulations from `knn_st` objects
#'
#' @param disagg_flow A `knn_st` object
#'
#' @return The number of diaggregation simulations
#'
#' @examples
#' flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
#' # made up historical data to use as index years
#' ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
#' # make up monthly flow for two sites
#' mon_flow <- cbind(
#'   rnorm(80 * 12, mean = 20, sd = 5),
#'   rnorm(80 * 12, mean = 120, sd = 45)
#' )
#' x <- knn_space_time_disagg(
#'   flow_mat, ind_flow, mon_flow, sf_sites = 1:2, nsim = 5
#' )
#'
#' # will return 5
#' knnst_nsim(x)
#'
#' @export
knnst_nsim <- function(disagg_flow)
{
  assertthat::assert_that(
    is_knnst(disagg_flow),
    msg = "`disagg_flow` should be a `knnst` object"
  )

  length(disagg_flow)
}

#' Get all the index years from `knn_st` objects
#'
#' `knnst_index_years()` gets all of the index years from all of the
#' disaggregation simulations.
#'
#' @inheritParams knnst_nsim
#'
#' @return A matrix where each column is the index years selected for that
#' simulation, i.e., there are [knnst_nsim()] columns.
#'
#' @examples
#' flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
#' # made up historical data to use as index years
#' ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
#' # make up monthly flow for two sites
#' mon_flow <- cbind(
#'   rnorm(80 * 12, mean = 20, sd = 5),
#'   rnorm(80 * 12, mean = 120, sd = 45)
#' )
#' x <- knn_space_time_disagg(
#'   flow_mat, ind_flow, mon_flow, sf_sites = 1:2, nsim = 5
#' )
#'
#' # will be a 3 x 5 matrix:
#' knnst_index_years(x)
#'
#' @export
knnst_index_years <- function(disagg_flow)
{
  assertthat::assert_that(
    is_knnst(disagg_flow),
    msg = "`disagg_flow` should be a `knnst` object"
  )

  do.call(
    cbind,
    lapply(
      seq_len(knnst_nsim(disagg_flow)),
      function(x) disagg_flow[[x]]$index_years
    )
  )
}
