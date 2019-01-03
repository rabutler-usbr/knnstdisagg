#' Plot resulting statistics for knn space-time disaggregation
#'
#' `plot.knnst()` implements the [plot()] method for `knnst` objects, and relies on
#' ggplot2. It plots the key statistics the knn space-time disaggregation method
#' should be preserving.
#'
#' `...` is passed to [geom_point()] and [labs()].
#'
#' @param x An object inheriting from class `knnst`
#'
#' @param ... arguments to be passed to subsequent methods.
#'
#' @param site The site to plot. Site name as a character.
#'
#' @param base_units The (input) units of the flow that was disaggregated. These
#'   units will be shown as the y-axis label of the plot.
#'
#' @export
plot.knnst <- function(x, site = "S1", base_units = NULL, ...)
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

  x_plot_data <- get_plot_stats(x_df, site)

  # get historical data as a data frame, organize, and compute same stats
  # as on the simulated data
  x_mon <- x %>%
    get_pattern_flow_data_df(site)

  x_mon_stats <- x_mon %>%
    get_plot_stats(site)

  # monthly means
  gg <- ggplot(x_plot_data, aes_string("month", "Value")) +
    geom_boxplot(aes_string(group = "month")) +
    facet_wrap("Variable", ncol = 2, scales = "free_y") +
    geom_point(
      data = x_mon_stats,
      aes_string("month", "Value"),
      shape = 18,
      ...
    ) +
    labs(
      x = NULL,
      title = site,
      y = paste("Base units =", base_units),
      ...
    )

  # TODO: monthly pdf - move to function
  # TODO: do we need to re-do this based on Balaji/Ken's code? Using density
  # instead of hist()
  # compute histograms for all simulations
  nsim <- knnst_nsim(x)
  # 1) call sm.density() initially, with breaks = breaks on all data
  mon_breaks <- lapply(1:12, function(mm) {
    tmp_hist <- dplyr::filter_at(x_mon, "month", dplyr::any_vars(. == mm))[[site]]
    tmp <- c(
      tmp_hist,
      dplyr::filter_at(x_df, "month", dplyr::any_vars(. == mm))[[site]]
    )

    # adaptabed from Knowak
    # set the normal bandwidth
    bandd <- sm::hnorm(tmp)

    # go one bandwidth from the min and max of the data..
    xlow <- min(tmp) - bandd

    if(min(tmp_hist) >= 0) {
      xlow <- max(0, xlow)
    }

    xhigh <- max(tmp) + bandd

    #create 50 points equally spaced between xlow and xhigh..
    seq(xlow, xhigh, length = 50)
  })

  # 2) then call hist() with breaks = output of above
  sim_pdf <- lapply(seq_len(nsim), function(n1) {
    tmp <- list()

    for (mm in 1:12) {
      t2 <- x_df %>%
        dplyr::filter_at("simulation", dplyr::any_vars(. == n1)) %>%
        dplyr::filter_at("month", dplyr::any_vars(. == mm))
      tmp[[mm]] <- sm::sm.density(
        t2[[site]],
        eval.points = mon_breaks[[mm]],
        display = "none"
      )
    }

    tmp
  })

  # 2b) and call hist() on historical data
  hist_pdf <- lapply(1:12, function(mm) {
    tmp <- dplyr::filter_at(x_mon, "month", dplyr::any_vars(. == mm))[[site]]
    sm::sm.density(tmp, display = "none", eval.points = mon_breaks[[mm]])
  })

  # 3) then create df with $density of all calls to hist and $breaks as flow (x)
  hist_pdf <- do.call(rbind, lapply(1:12, function(mm) {
    data.frame(
      month = mm,
      x = mon_breaks[[mm]],
      density = hist_pdf[[mm]]$estimate
    )
  }))

  sim_pdf <- do.call(rbind, lapply(seq_len(nsim), function(n1) {
    tmp <- c()

    for (mm in 1:12) {
      tmp <- rbind(tmp, data.frame(
        month = mm,
        x = mon_breaks[[mm]],
        density = sim_pdf[[n1]][[mm]]$estimate
      ))
    }

    tmp
  }))

    # 4) then plot with ggplot() + geom_boxplot()
  mon_labels <- month.abb
  names(mon_labels) <- 1:12
  gg2 <- ggplot(sim_pdf, aes_string("x", "density")) +
    geom_boxplot(aes_string(group = "x")) +
    geom_line(data = hist_pdf, aes_string("x", "density")) +
    facet_wrap(~month, nrow = 4, ncol = 3, labeller = as_labeller(mon_labels)) +
    labs(x = paste0("Flow (", base_units, ")"), y = "Probability Density")

  # TODO: annual pdf

  # TODO: add in control for which figures get plotted and interactive moving to
  # next plot
  #gg
  gg2
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
