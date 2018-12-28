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
  all_cols <- names(x_df)

  assert_that(
    site %in% names(x_df),
    msg = "In `plot.knnst()`, `site` should be a valid site name."
  )

  x_df <- get_plot_stats(x_df, site)

  # get historical data as a data frame, organize, and compute same stats
  # as on the simulated data
  x_mon <- x %>%
    get_pattern_flow_data_df(site) %>%
    get_plot_stats(site)

  # monthly means
  gg <- ggplot(x_df, aes_string("month", "Value")) +
    geom_boxplot(aes_string(group = "month")) +
    facet_wrap("Variable", ncol = 2, scales = "free_y") +
    geom_point(
      data = x_mon,
      aes_string("month", "Value"),
      color = "red",
      shape = 18
    ) +
    labs(
      x = NULL
    )

  # TODO: monthly pdf

  # TODO: annual pdf

  gg
}

get_plot_stats <- function(x_df, site)
{
  keep_cols <- c("ym", "year", "month", site, "simulation")
  vars_group <- c("month", "simulation")
  var_name_order <- c(
    "mean" = "Mean",
    "stats::var" = "Variance",
    "max" = "Maximum",
    "min" = "Minimum",
    "stats::cor" = "Lag-1 Correlation",
    "skew" = "Skew"
  )

  x_df %>%
    # subset to site
    dplyr::select_at(keep_cols) %>%
    dplyr::group_by_at("simulation") %>%
    dplyr::arrange_at("ym") %>%
    dplyr::mutate_at(site, dplyr::funs("tmp" = dplyr::lag(.))) %>%
    dplyr::group_by_at(vars_group) %>%
    # means, standard deviation, max, min, skew, lag-1 correlation
    dplyr::summarise_at(
      site,
      dplyr::funs(
        mean, stats::var, max, min, skew,
        stats::cor(get(site), get("tmp"), use = "complete.obs")
      )
    ) %>%
    tidyr::gather_(
      "Variable",
      "Value",
      tidyselect::vars_select(names(.), -tidyselect::one_of(vars_group))
    ) %>%
    dplyr::mutate_at(
      "Variable",
      dplyr::funs(factor(var_name_order[.], levels = var_name_order))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(
      "month",
      dplyr::funs(factor(month.abb[.], levels = month.abb))
    )
}
