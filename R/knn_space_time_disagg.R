
#' Spatial and temporal disaggregation of flow data
#'
#' `knn_space_time_disagg()` disaggregates annual flow data spatially and
#' temporally (to monthly), using a spatial and monthly flow pattern selected
#' from an "index year". The index year is selected using a k nearest-neighbor
#' approach, (Knowak et al., 2010).
#'
#' The method is described in detail in *Knowak et al.* (2010). The methodology
#' disaggregates annual flow data (`ann_flow`) by selecting an index year from
#' `ann_index_flow` using [knn_get_index_year()]. After the index year is
#' selected, values from `ann_flow` are disaggregated spatially, and temporally
#' based on `mon_flow`. The spatial pattern is reflected by including different
#' sites as columns in `mon_flow`, and the monthly disaggregation, uses the
#' monthly pattern in `mon_flow` to disaggregate the data temporally.
#' Summability is preserved using this method, if the values selected in
#' `mon_flow` are scaled and if the columns (or a subset of columns) in
#' `mon_flow` sum together to equal `ann_index_flow`.
#'
#' In some cases, it is desirable to select monthly flow directly, instead of
#' scaling it. This can be performed by only scaling certain sites, using
#' `scale_sites`. `scale_sites` should be a boolean, or a vector of numerics. If
#' `TRUE`, then all sites are scaled. If `FALSE`, all sites monthly values
#' are selected directly. Otherwise, `scale_sites` should be a vector of the
#' sites that should be scaled, based on their column index from `mon_flow`. For
#' example, if `mon_flow` is a matrix with 4 columns, then setting `mon_flow` to
#' `c(2, 3)` will scale the values in sites 2 and 3 (columns 2 and 3), while
#' selecting flow values directly in sites 1 and 2.
#'
#' @param mon_flow Monthly natural flow. Used for spatially and
#'   temporally disaggregating the flow data (`ann_flow`) based on the index
#'   year selected from `ann_index_flow`, by [knn_get_index_year()]. Each column
#'   represents a different site, and the annual flow at the index gage will be
#'   disaggregated to each of these sites at he monthly level. If there are
#'   three columns in this matrix, then the values in `ann_flow` will be
#'   disaggregated to three sites. `mon_flow` should have the same years as
#'   `ann_index_flow`, therefore, it should contain 12 times more rows than
#'   `ann_index_flow`. The flow data in `mon_flow` should also contain values
#'   for the same years as `ann_index_flow`, though there are no checks
#'   performed to check this, since this is expected to be a dimensionless
#'   matrix.
#'
#' @param start_month The start month of the `mon_flow` as an integer. 1 =
#'   January, 2 = February, etc. Used to correctly label the output data.
#'
#' @param scale_sites The site numbers (column indices), that will scale the
#'   index year's volume based on the annual flow being disaggregated. The
#'   remaining sites will select the index year directly. See **Details**.
#'
#' @param nsim Number of times to repeat the space/time disaggregation.
#'
#' @param index_years Optional. If specified, these index years will be used
#'   instead of selecting years based on weighted sampling via
#'   [knn_get_index_year()].
#'
#' @return A [`knnst`] object.
#'
#' @inheritParams knn_get_index_year
#'
#' @author Ken Nowak
#'
#' @references Nowak, K., Prairie, J., Rajagopalan, B., Lall, U. (2010).
#'   A nonparametric stochastic approach for multisite disaggregation of
#'   annual to daily streamflow. *Water Resources Research.*
#'
#' @seealso [`knnst`], [knn_get_index_year()]
#'
#' @examples
#'
#' # a sample of three years of flow data
#' flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
#' # made up historical data to use as index years
#' ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
#' # make up monthly flow for two sites
#' mon_flow <- cbind(
#'   rnorm(80 * 12, mean = 20, sd = 5),
#'   rnorm(80 * 12, mean = 120, sd = 45)
#' )
#' knn_space_time_disagg(flow_mat, ind_flow, mon_flow, 1, scale_sites = 1:2)
#'
#' @export
knn_space_time_disagg <- function(ann_flow,
                         ann_index_flow,
                         mon_flow,
                         start_month,
                         nsim = 1,
                         scale_sites = NULL,
                         index_years = NULL,
                         k_weights = knn_params_default(nrow(ann_index_flow)))
{
  n_disagg_yrs <- nrow(ann_flow)

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
  assert_that(ncol(ann_flow) == 2)

  assert_that(
    length(start_month) == 1 && is.numeric(start_month) && start_month %in% 1:12,
    msg = "`start_month` should be a single integer from 1 to 12"
  )

  nsite <- ncol(mon_flow)

  if (!is.null(index_years)) {
    assert_that(
      ncol(index_years) == nsim,
      msg = paste(
        "`index_years` must be specified for all simulations.\n",
        " So, `nsim` must equal the number of columns in `index_years`.",
      )
    )

    assert_that(
      nrow(index_years) == n_disagg_yrs,
      msg =
        "`index_years` must be specified for every year in the paleo record."
    )
  }

  if (!missing(index_years)) {
    message(
      "`index_years` is specified, so `k_weights` will be ignored."
    )
  }

  if (!is.null(scale_sites)) {
    assert_that(
      max(scale_sites) <= nsite,
      msg = "max(`scale_sites`), must be <= the number of sites (`site`)."
    )
  }

  if (!all(1:nsite %in% scale_sites)) {
    # set ind_sites to the remaining sites
    # ind_sites are selected directly
    ind_sites <- 1:nsite
    ind_sites <- ind_sites[!(1:nsite %in% scale_sites)]
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
  disag <- array(data = NA, dim = c(n_disagg_yrs, 12, nsite, nsim))

  # matrix for recording yr index for disag (optional)
  index_mat <- matrix(ncol = nsim, nrow = n_disagg_yrs)

  # matrix for recording scale factors used (optional)
  sf_mat <- matrix(ncol = nsim, nrow = n_disagg_yrs)

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
      ind_yrs <- knn_get_index_year(ann_flow, ann_index_flow, k_weights)
      index_mat[, j] <- ind_yrs[, 1]
    } else {
      ind_yrs <- index_years[, j, drop = FALSE]
      index_mat[, j] <- ind_yrs[, 1]
    }

    # loop through all years that need disaggregated ---------------
    for(h in seq_len(n_disagg_yrs)) {

      # select the index year and scaling factor
      index_atts <- get_scale_factor(
        ind_yrs[h, 1],
        ann_flow[h, 2],
        ann_index_flow
      )

      sf_mat[h, j] <- index_atts$SF

      disag[h, , scale_sites, j] <- dat_a[index_atts$pos, , scale_sites] *
        index_atts$SF
      disag[h, , ind_sites, j] <- dat_a[index_atts$pos, , ind_sites]
    }
  }

  # create rownames for yyyy-mm
  yy <- ym_labels(ann_flow[,1], start_month)
  site_names <- colnames(mon_flow)

  # convert from 4-d array to list of 2-d arrays
  disag_out <- lapply(seq_len(nsim), function(ii) {
    disagg_flow <- do.call(
      cbind,
      lapply(seq_len(nsite), function(jj) as.vector(t(disag[,,jj,ii])))
    )
    rownames(disagg_flow) <- yy
    colnames(disagg_flow) <- site_names
    list(
      disagg_flow = disagg_flow,
      index_years = index_mat[,ii]
    )
  })

  # add in the historical data and index gage data, and move all the simulation
  # data in one layer.

  disag_out <- list(
    disagg_sims = disag_out,
    index_data = ann_index_flow,
    mon_flow = mon_flow,
    start_month = start_month
  )

  disag_out <- structure(disag_out, class = c("knnst", "list"))
  validate_knnst(disag_out)

  disag_out
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

# given a start month, returns a single year of months
full_year <- function(start_month)
{
  x <- rep(1:12, 2)
  x <- x[start_month:(start_month + 11)]

  x
}

# creates row name labels
# returns in yyyy-mm format
ym_labels <- function(years, start_month)
{
  # adjust the years for start_month not == January, because for example, a
  # start_monnth of October actually belongs to the previous year.
  # assumes years correspond with last month in year, not first month in year
  yy <- expand.grid(full_year(start_month), years)
  mm <- yy[,1]
  yy <- yy[,2]
  if (start_month != 1)
    yy[mm %in% start_month:12] <-  yy[mm %in% start_month:12] - 1

  yy <- paste(
    yy,
    formatC(mm, width = 2, format = "d", flag = "0"),
    sep = "-"
  )

  yy
}
