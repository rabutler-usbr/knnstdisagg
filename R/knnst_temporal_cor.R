#' Compute temporal correlation after KNN space-time disaggregation
#'
#' `knnst_temporal_cor()` computes the temporal correlation between all months
#' at the specified `site`.
#'
#' @inheritParams knnst_spatial_cor
#'
#' @param site Site to compute the correlations at. Scalar character.
#'
#' @return A `knnst_tmpcor` object.
#'
#' @seealso [plot.knnst_tmpcor()], [knnst_spatial_cor()]
#'
#' @examples
#' # use a 50-year bin size to compute the stats at Cameo
#' tmp_cor <- knnst_temporal_cor(ex_disagg, "Cameo", 50)
#'
#' # use a 100-year bin size for computing the stats at Hoover
#' tmp_cor <- knnst_temporal_cor(ex_disagg, "Hoover", 100)
#'
#' @export
knnst_temporal_cor <- function(disagg, site, bin_size)
{
  # assertions -------------
  assert_that(length(site == 1) && is.character(site))
  assert_that(
    site %in% colnames(disagg[["mon_flow"]]),
    msg = "`site` does not exist in the `knnst` object."
  )

  assert_that(is_knnst(disagg))
  #TODO: update to handle multiple simulations
  assert_that(
    knnst_nsim(disagg) == 1,
    msg = "knnst_sp_cor() is currently only setup to work with one simulation"
  )

  assert_that(is.numeric(bin_size) && length(bin_size) == 1)

  nyrs <- nrow(disagg$disagg_sims[[1]]$disagg_flow) / 12
  assert_that(
    bin_size <= nyrs && bin_size > 1,
    msg = "`bin_size` should be > 1 and < the number of years of disaggregated data"
  )

  # setup data --------------------
  df <- as.data.frame(disagg) %>%
    dplyr::select_at(c("ym", "year", "month", site)) %>%
    dplyr::mutate_at("ym", list(~get_agg_year(., disagg[["start_month"]]))) %>%
    dplyr::select_at(dplyr::vars(-dplyr::one_of("year"))) %>%
    dplyr::mutate_at("month", list(~month.abb[.])) %>%
    tidyr::pivot_wider(names_from = "month", values_from = site)

  # observed data as data frame
  # TODO: year is agg_year. Need to verify this!!!
  obs_df <- get_pattern_flow_data_df(disagg, site) %>%
    dplyr::select_at(c("year", "month", site)) %>%
    dplyr::mutate_at("month", list(~month.abb[.])) %>%
    tidyr::pivot_wider(names_from = "month", values_from = site)


  # all unique month combinations of months
  # TODO: change from month.abb to get_full_year()? then it would order based
  # on the user specified year order? If so, replace month.abb with it for the
  # rest of the function
  comb_month <- utils::combn(month.abb, 2)
  # combine with all same month combos ex: Jan-Jan
  comb_month <- cbind(comb_month, t(cbind(month.abb, month.abb)))

  # pattern month cor --------------
  obs_res <- matrix(
    NA, nrow = 12, ncol = 12,
    dimnames = list(month.abb, month.abb)
  )

  for (cc in seq_len(ncol(comb_month))) {
    month1 <- comb_month[1, cc]
    month2 <- comb_month[2, cc]

    tmp_cor <- stats::cor(obs_df[[month1]], obs_df[[month2]])

    i = match(month1, month.abb)
    j = match(month2, month.abb)

    obs_res[j, i] <- tmp_cor
  }

  # convert to a data frame
  obs_res <- data.frame(obs_res)
  obs_res[["month2"]] <- rownames(obs_res)
  obs_res <- tidyr::pivot_longer(
    obs_res,
    -tidyselect::one_of("month2"),
    names_to = "month1", values_to = "cor"
  ) %>%
    dplyr::filter_at("cor", dplyr::all_vars(!is.na(.)))

  # and make the months factors
  obs_res$month1 <- factor(obs_res$month1, levels = month.abb)
  obs_res$month2 <- factor(obs_res$month2, levels = month.abb)

  # disagg data cor --------------------
  # try rolling bins for the modeled data
  n <- nrow(df) - bin_size + 1

  rolling_res <- array(
    dim = c(12, 12, n),
    dimnames = list(month.abb, month.abb, NULL)
  )

  for (i in seq_len(n)) {
    end_n <- i + bin_size - 1
    for (cc in seq_len(ncol(comb_month))) {
      month1 <- comb_month[1, cc]
      month2 <- comb_month[2, cc]

      tmp_cor <- stats::cor(df[[month1]][i:end_n], df[[month2]][i:end_n])

      rolling_res[match(month2, month.abb), match(month1, month.abb), i] <- tmp_cor
    }
  }

  # convert to data frame
  rolling_res <- as.data.frame(rolling_res)
  rolling_res[["month2"]] <- rownames(rolling_res)
  rolling_res <- tidyr::pivot_longer(
    rolling_res,
    -tidyselect::one_of("month2"),
    names_to = "month1", values_to = "cor"
  ) %>%
    dplyr::filter_at("cor", dplyr::all_vars(!is.na(.))) %>%
    dplyr::mutate_at(
      "month1",
      list(~simplify2array(strsplit(., ".", fixed = TRUE))[1,])
    )

  rolling_res$month1 <- factor(rolling_res$month1, levels = month.abb)
  rolling_res$month2 <- factor(rolling_res$month2, levels = month.abb)

  res <- list(
    disagg_cor = rolling_res,
    pattern_cor = obs_res,
    bin_size = bin_size,
    site = site
  )

  structure(
    res,
    class = "knnst_tmpcor"
  )
}
