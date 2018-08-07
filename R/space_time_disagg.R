
#' Spatial and temporal diaggregattion of paleo flow data
#'
#' The default parameter values are setup to perform the typical disaggregation
#' for CRSS, based on scaling the Upper Basin inflows, and performing a
#' direct resampling of Lower Basin tributaries, i.e., no scale factor is
#' applied to the LB tributaries. Sites 1-20 are scaled (`sf_sites = 1:20`);
#' therefore the remaining sites (21:29) are not scaled.
#'
#' @param x The annual paleo data to disaagregate.
#' @param ann_index_flow Observed annual flow data used for picking index year.
#' @param mon_flow Intervening monthly natural flow. Used for spatially and
#'   temporaly disaggregating paleo data based on the index year. Each column
#'   should be a differnt site to disaggregate the flow to. If there are three
#'   columns in this matrix, then the values in `x` will be disaggregated to
#'   three sites. `mon_flow` should have the same years as `ann_index_flow`
#' @param sf_sites The site numbers (column indeces), that will scale the
#'   index year's volume based on the annual flow being disaggregated. The
#'   remaining sites will select the index year directly. See **Details**.
#' @param nsim Number of times to repeat the space/time disaggregation.
#' @param ofolder Optional. If specified, the disaggregated flow data and the
#'   selected index years are saved to this folder as csv files. There will be
#'   one csv file for each time the disaggregation is repeated (`nsim`). This
#'   file will contain one column for each site (`nsite`), and one row for each
#'   month of data (12 * number of years in `x`).
#' @param index_years Optional. If specified, these index years will be used
#'   instead of selecting years based on weights and sampling.
#' @param k_weights If `NULL`, parameters are set based on definitions in Nowak
#'   et al. (2010). Users may force `k` and the `weights` by specifiying this
#'   argument. It should be a list with two named entries: `k` and `weights`.
#'
#' @author Ken Nowak
#'
#' @references Nowak, K., Prairie, J., Rajagopalan, B., Lall, U. (2010).
#'   A nonparametric stochastic approach for multisite disaggregation of
#'   annual to daily streamflow. *Water Resources Research.*
#'
#' @examples
#' \dontrun{
#' # read in annual synthetic mon_flow for disag
#' x <- matrix(scan("data-raw/Meko.txt"), ncol = 2, byrow = TRUE)
#' # intervening natural flow mon_flow - monthly CY text file
#' mon_flow <- as.matrix(read.table(
#'   "tests/dp/NFfullbasinWY0608intervening.txt",
#'   sep = "\t"
#' ))
#'
#' # observed annual flow for picking analog disag yr
#' ann_index_flow <- as.matrix(read.table("tests/dp/LFWYTotal.txt"))
#' zz <- paleo_disagg(x, ann_index_flow, mon_flow, 29, 1)
#' }
#'
#' @export
knn_space_time_disagg <- function(x,
                         ann_index_flow,
                         mon_flow,
                         nsim = 1,
                         sf_sites = NULL,
                         ofolder = NULL,
                         index_years = NULL,
                         k_weights = NULL)
{
  n_disagg_yrs <- nrow(x)

  # how many yrs of observed mon_flow
  assert_that(
    nrow(mon_flow) %% 12 == 0,
    msg = "`mon_flow` needs to have an even year's worth of data"
  )

  n_obs_yrs <- nrow(mon_flow)/12

  assert_that(
    n_obs_yrs == nrow(ann_index_flow),
    msg = "`ann_index_flow` and `mon_flow` must have the same number of years."
  )

  assert_that(ncol(ann_index_flow) == 2)
  assert_that(ncol(x) == 2)

  nsite <- ncol(mon_flow)

  if (!is.null(index_years)) {
    if (ncol(index_years) != nsim)
      stop(
        "`index_years` must be specified for all simulations.\n",
        " So, `nsim` must equal the number of columns in `index_years`.",
        call. = FALSE
      )

    if (nrow(index_years) != n_paleo_yrs)
      stop(
        "`index_years` must be specified for every year in the paleo record.",
        call. = FALSE
      )
  }

  if (!is.null(index_years) && !is.null(k_weights)) {
    stop(
      "If specifying `index_years`, there is no need to specify `k_weights`",
      call. = FALSE
    )
  }

  if (max(sf_sites) > nsite) {
    stop(
      "max(`sf_sites`), must be <= the number of sites (`site`).",
      call. = FALSE
    )
  }

  if (!all(1:nsite %in% sf_sites)) {
    # set ind_sites to the remaining sites
    # ind_sites are selected directly
    ind_sites <- 1:nsite
    ind_sites <- ind_sites[!(1:nsite %in% sf_sites)]
    message(
      "Sites ", toString(ind_sites), "\n",
      "will be selected directly from the index years, i.e., not scaled."
    )
  } else {
    ind_sites <- NULL
  }

  # matrix for observed values - row(yr), col (month), index (site)
  dat_a <- array(data = NA, dim=c(n_obs_yrs, 12, nsite))

  # matrix for disag values - row(yr), col (month), index1 (site), index2(sim #)
  disag <- array(data = NA, dim = c(n_paleo_yrs, 12, nsite, nsim))

  # matrix for recording yr index for disag (optional)
  index_mat <- matrix(ncol = nsim, nrow = n_paleo_yrs)

  # matrix for recording scale factors used (optional)
  sf_mat <- matrix(ncol = nsim, nrow = n_paleo_yrs)

  # this loop moves observed monthly mon_flow from 2d matrix to 3d array
  mgn <- length(dat_a[1,1,])

  for (j in seq_len(mgn)) {

    s <- 1
    e <- 12

    for (i in seq_len(n_obs_yrs)) {

      dat_a[i, , j] <- mon_flow[s:e, j]

      s <- s + 12
      e <- e + 12
    }
  }

  # loop through the number of simulations ---------------------
  for(j in seq_len(nsim)){

    # this picks the 1st year for disag based only on the annual flow

    if (is.null(index_years)) {
      ind_yrs <- knn_get_index_year(x, ann_index_flow, k_weights)
      index_mat[, j] <- ind_yrs[, 1]
    } else {
      ind_yrs <- index_years[, j, drop = FALSE]
      index_mat[, j] <- ind_yrs[, 1]
    }

    # loop through all years that need disaggregated ---------------
    for(h in seq_len(n_paleo_yrs)) {

      # select the index year and scaling factor
      index_atts <- get_scale_factor(ind_yrs[h, 1], x[h, 2], ann_index_flow)

      sf_mat[h, j] <- index_atts$SF

      disag[h, , sf_sites, j] <- dat_a[index_atts$pos, , sf_sites] *
        index_atts$SF
      disag[h, , ind_sites, j] <- dat_a[index_atts$pos, , ind_sites]
    }
  }


  # convert from 4-d array to list of 2-d arrays
  disag_out <- lapply(seq_len(nsim), function(ii) {
    do.call(
      cbind,
      lapply(seq_len(nsite), function(jj) as.vector(t(disag[,,jj,ii])))
    )
  })

  # output to "flat" file

  if (!is.null(ofolder)) {
    write_knn_disagg(disag_out, index_mat, ofolder = ofolder)
  }

  invisible(list(paleo_disagg = disag_out, index_years = index_mat))
}

#' Compute the scaling factor for the current year's flow from the index year
#'
#' Selects the magnitude of flow from `ann_index_flow` for the `index_year`.
#' Then scaling factor is computed as `flow` / `ann_index_flow[index_year]`.
#'
#' @param index_year The index year to select from ann_index_flow
#' @param flow The current flow to disaggregate (length == 1)
#' @param ann_index_flow Matrix of index years and flow. n x 2 matrix. First
#'   column is years.
#'
#' @noRd
#'
get_scale_factor <- function(index_year, flow, ann_index_flow)
{
  assert_that(length(index_year) == 1)
  assert_that(length(flow) == 1)
  assert_that(ncol(ann_index_flow) == 2)
  assert_that(index_year %in% ann_index_flow[,1])

  # index for selected yr
  pos <- match(index_year, ann_index_flow[,1])

  # scaling factor to apply for disag
  SF <- flow/(ann_index_flow[pos, 2])

  list(pos = pos, SF = SF)
}

write_knn_disagg <- function(disag_out, index_mat, ofolder = ".")
{
  nsim <- length(disag_out)

  lapply(seq_len(nsim), function(ii)
    utils::write.csv(
      disag_out[[ii]],
      file = file.path(ofolder, paste0("paleo_disagg_", ii, ".csv")),
      row.names = FALSE
    )
  )
  utils::write.csv(
    index_mat,
    file = file.path(ofolder, "index_years.csv"),
    row.names = FALSE
  )
}
