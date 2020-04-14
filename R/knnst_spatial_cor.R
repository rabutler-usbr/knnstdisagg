#' Compute spatial correlation after KNN space-time disaggregation
#'
#' `knnst_spatial_cor()` computes the spatial correlation for all monthly data
#' between the specifed site(s) (`sites`) and all other sites.
#'
#' @inheritParams knnst_nsim
#'
#' @param sites One or more sites to compute correlations from.
#'
#' @param bin_size Number of years for each bin.
#'
#' @return A `knnst_spcor` object.
#'
#' @seealso [plot.knnst_spcor()], [knnst_temporal_cor()]
#'
#' @examples
#' knnst_spatial_cor(ex_disagg, "Cameo", 100)
#' knnst_spatial_cor(ex_disagg, c("Cameo", "Hoover"), 100)
#'
#' @export
knnst_spatial_cor <- function(disagg, sites, bin_size)
{
  #TODO: update to handle multiple simulations
  assert_that(
    knnst_nsim(disagg) == 1,
    msg = "knnst_sp_cor() is currently only setup to work with one simulation"
  )

  #TODO: check that sites name exists in data

  assert_that(is_knnst(disagg))
  assert_that(is.numeric(bin_size) && length(bin_size) == 1)

  nyrs <- nrow(disagg$disagg_sims[[1]]$disagg_flow) / 12
  assert_that(bin_size <= nyrs)

  df <- as.data.frame(disagg)

  # simulated data as data frame
  # TODO: only need this if we are trying to compute annual values
  # df <- df %>%
  #   dplyr::mutate_at("ym", list(~get_agg_year(., disagg[["start_month"]])))

  # xx is site(s) to compute correlation with all toher sites
  # yy is all other sites (including cameo)

  # pattern flow cor ---------------
  i <- match(sites, colnames(disagg$mon_flow))
  xx <- as.matrix(disagg$mon_flow[,i])
  yy <- as.matrix(disagg$mon_flow)
  hist_cor <- stats::cor(xx, yy)

  # disagg results cor -------------
  # compute cor for all sites
  xx <- dplyr::select_at(df, sites)
  yy <- dplyr::select_at(
    df,
    dplyr::vars(
      -tidyselect::one_of("month", "simulation", "index_year", "ym", "year")
    )
  )

  # this is monthly data
  n <- nrow(yy) / 12 - bin_size + 1

  # preallocate results matrix
  # number of bins x sites to correlate from x sites to correlate to
  res <- array(
    -99999,
    dim = c(n, ncol(xx), ncol(yy)),
    dimnames = list(1:n, colnames(xx), colnames(yy))
  )

  bin_size_month <- bin_size * 12
  for (i in 1:n) {
    offset <- (i - 1) * 12
    rr <- (1 + offset):(bin_size_month + offset)
    tmp <- stats::cor(xx[rr,], yy[rr,])
    res[i,,] <- tmp
  }

  # convert to data frame ----------------
  res <- as.data.frame(res)
  res <- tidyr::pivot_longer(
    res,
    tidyselect::everything(),
    names_to = "site",
    values_to = "cor"
  )
  # split the combined name to get sit_from and site_to
  site_names <- simplify2array(strsplit(res[["site"]], ".", fixed = TRUE))
  res[["site_from"]] <- site_names[1,]
  res[["site_to"]] <- site_names[2,]
  res[["site"]] <- NULL

  # convert historical corelation to df
  hist_cor <- as.data.frame(hist_cor)
  hist_cor[["site_from"]] <- rownames(hist_cor)

  hist_cor <- tidyr::pivot_longer(
    hist_cor,
    -tidyselect::one_of("site_from"),
    names_to = "site_to",
    values_to = "cor"
  )

  res <- list(
    disagg_cor = res,
    pattern_cor = hist_cor,
    orig_sites = colnames(yy),
    bin_size = bin_size
  )

  structure(
    res,
    class = "knnst_spcor"
  )
}
