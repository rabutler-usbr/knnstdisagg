#' `plot.knnst` implements the [plot] method for `knnst` objects, and relies on
#' ggplot2
#'
#' @param x An object inheriting from class `knnst`
#'
#' @param ... arguments to be passed to subsequent methods
#'
#' @param site The site to plot. Site name as a character.
#'
#' @export
plot.knnst <- function(x, ..., site = "S1")
{
  assert_that(
    length(site) == 1 && is.character(site),
    msg = "In `plot.knnst()`, `site` should be a character with length of 1."
  )

  x_df <- as.data.frame(x)

  assert_that(
    site %in% names(x_df),
    msg = "In `plot.knnst()`, `site` should be a valid site name."
  )

  keep_cols <- c("ym", "year", "month", site, "simulation")
  vars_group <- c("month", "simulation")

  x_df <- x_df %>%
    # subset to site
    dplyr::select_at(keep_cols) %>%
    dplyr::group_by_at(vars_group) %>%
    # means, standard deviation, max, min
    # TODO: skew, lag-1 correlation
    dplyr::summarise_at(site, dplyr::funs(mean, stats::var, max, min)) %>%
    dplyr::mutate_at("var", dplyr::funs(.^0.5)) %>%
    dplyr::rename_at("var", function(.){"sd"}) %>%
    tidyr::gather_(
      "Variable",
      "Value",
      tidyselect::vars_select(names(.), -tidyselect::one_of(vars_group))
    )

  # TODO: get historical data as a data frame, organize, and compute same stats
  # as on the simulated data

  # monthly means
  gg <- ggplot(x_df, aes_string("month", "Value")) +
    geom_boxplot(aes_string(group = "month")) +
    facet_wrap("Variable", ncol = 2, scales = "free_y")

  # monthly pdf

  # annual pdf

  gg
}
