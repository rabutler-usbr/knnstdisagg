#' Plot resulting statistics for knn space-time disaggregation
#'
#' `plot.knnst()` implements the [plot()] method for `knnst` objects, and relies
#' on ggplot2. It plots the key statistics the knn space-time disaggregation
#' method should be preserving.
#'
#' `which` controls the plots that are created. There are both monthly and
#' annual plots and two main plot types: a cdf of the flows across all of the
#' different disaggregations, and a panel showing the mean, max, min, variance,
#' lag-1 correlation, and skew across all of the different disaggregations. The
#' numbers correspond to:
#'
#' - `1:12`: the different monthly cdfs.
#' - `13`: annual cdf
#' - `14`: monthly statistics panel
#' - `15`: annual statistics panel
#'
#' `...` is passed to [geom_point()] and [labs()].
#'
#' @param x An object inheriting from class `knnst`
#'
#' @param site The site to plot. Site name as a character.
#'
#' @param base_units The (input) units of the flow that was disaggregated. These
#'   units will be shown as the y-axis label of the plot.
#'
#' @param which The subset of plots to create; specify a subset of the number
#'   `1:15`. See 'Details' for the different plots.
#'
#' @param show Boolean that determines if diagnostic plots are shown or only
#'   returned. If `TRUE` and in interactive mode, will be able to enter through
#'   each of the 1-15 plots.
#'
#' @param ... Arguments to be passed to subsequent methods.
#'
#' @return A `knnstplot` object
#'
#' @export
plot.knnst <- function(x, site = "S1", base_units = NULL, which = c(13, 14, 15),
                       show = FALSE, ...)
{
  assert_that(
    length(site) == 1 && is.character(site),
    msg = "In `plot.knnst()`, `site` should be a character with length of 1."
  )

  assert_that(
    length(which) > 0 && is.numeric(which) && all(which %in% 1:15),
    msg = "In `plot.knnst()`, `which` should be numeric values in 1:15"
  )

  assert_that(
    is.logical(show) && length(show) == 1,
    msg = "In `plot.knnst()`, `show` should be a logical scalar."
  )

  nsim <- knnst_nsim(x)
  x_df <- as.data.frame(x)
  all_cols <- names(x_df)

  assert_that(
    site %in% names(x_df),
    msg = "In `plot.knnst()`, `site` should be a valid site name."
  )

  x_plot_data <- get_mon_plot_stats(x_df, site)
  x_ann_plot_data <- get_ann_plot_stats(x_df, site)
  x_ann_sim_data <- x_df %>%
    dplyr::group_by_at(c("year", "simulation")) %>%
    dplyr::summarise_at(site, sum)

  # get historical stats -------------------
  # get historical data as a data frame, organize, and compute same stats
  # as on the simulated data
  x_mon <- x %>%
    get_pattern_flow_data_df(site)

  x_mon_stats <- x_mon %>%
    get_mon_plot_stats(site)

  # annual historical data
  x_ann <- x_mon %>%
    dplyr::group_by_at(c("year", "simulation")) %>%
    dplyr::summarise_at(site, sum)
  x_ann_stats <- get_ann_plot_stats(x_mon, site)

  # TODO: set n as a package option

  gg1 <- gg2 <- gg3 <- gg4 <- NULL
  # monthly plots ----------------
  if (14 %in% which)
    gg1 <- create_mon_bxp(x_plot_data, x_mon_stats, site, base_units, ...)

  if (any(1:12 %in% which))
    gg2 <- create_mon_cdf(x_df, x_mon, nsim, site, base_units, which, ...)

  # annual plots --------------------
  if (15 %in% which)
    gg3 <- create_ann_bxp(x_ann_plot_data, x_ann_stats, site, base_units, ...)

  if (13 %in% which)
    gg4 <- create_ann_cdf(x_ann_sim_data, x_ann, nsim, site, base_units, ...)

  gg_out <- c(
    list(
      "monthly-stats" = gg1,
      "annual-stats" = gg3,
      "annual-cdf" = gg4
    ),
    gg2
  )
  gg_out <- structure(gg_out, class = "knnstplot")

  if (show && interactive())
    print(gg_out)

  invisible(gg_out)
}

get_mon_plot_stats <- function(x_df, site)
{
  keep_cols <- c("ym", "year", "month", site, "simulation")
  vars_group <- c("month", "simulation")

  x_df %>%
    # subset to site
    dplyr::select_at(keep_cols) %>%
    dplyr::group_by_at("simulation") %>%
    dplyr::arrange_at("ym") %>%
    # compute stats for all months
    get_plot_stats(var_mutate = site, vars_group = vars_group) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(
      "month",
      list(~ factor(month.abb[.], levels = month.abb))
    )
}

get_ann_plot_stats <- function(x_df, site)
{

  x_df %>%
    # sum to annual values
    dplyr::select_at(c("year", site, "simulation")) %>%
    dplyr::group_by_at(c("year", "simulation")) %>%
    dplyr::summarise_at(site, list("ann" = sum)) %>%
    # compute the same stats as monthly
    dplyr::group_by_at("simulation") %>%
    dplyr::arrange_at("year") %>%
    get_plot_stats(var_mutate = "ann", vars_group = "simulation")

}

# for a single variable `var_mutate`, compute the mean, variance, max, min,
# lag-1 correlation, and skew
get_plot_stats <- function(x_df, var_mutate, vars_group)
{
  var_name_order <- c(
    "mean" = "Mean",
    "stats::var" = "Variance",
    "max" = "Maximum",
    "min" = "Minimum",
    "stats::cor" = "Lag-1 Correlation",
    "skew" = "Skew"
  )

  x_df %>%
    dplyr::mutate_at(var_mutate, list("tmp" = dplyr::lag)) %>%

    dplyr::group_by_at(vars_group) %>%
    # means, standard deviation, max, min, skew, lag-1 correlation
    dplyr::summarise_at(
      var_mutate,
      list(
        ~ mean(.), ~ stats::var(.), ~ max(.), ~ min(.), ~ skew(.),
        ~ stats::cor(., get("tmp"), use = "complete.obs")
      )
    ) %>%
    tidyr::gather_(
      "Variable",
      "Value",
      tidyselect::vars_select(names(.), -tidyselect::one_of(vars_group))
    ) %>%
    dplyr::mutate_at(
      "Variable",
      list(~ factor(var_name_order[.], levels = var_name_order))
    )
}

create_ann_bxp <- function(sim_data, hist_data, site, base_units = NULL, ...)
{
  gg <- ggplot(sim_data, aes_string(y = "Value")) +
    geom_boxplot() +
    facet_wrap("Variable", ncol = 2, scales = "free_y") +
    geom_point(
      data = hist_data,
      aes_string(x = 0, y = "Value"),
      shape = 18,
      ...
    ) +
    labs(
      x = NULL,
      title = paste(site, "- Annual Statistics"),
      y = paste("Base units =", base_units),
      ...
    )

  gg
}

create_mon_bxp <- function(sim_data, hist_data, site, base_units = NULL, ...)
{
  gg <- ggplot(sim_data, aes_string("month", "Value")) +
    geom_boxplot(aes_string(group = "month")) +
    facet_wrap("Variable", ncol = 2, scales = "free_y") +
    geom_point(
      data = hist_data,
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

  gg
}

create_ann_cdf <- function(sim_data, hist_data, nsim, site, base_units, ...)
{
  tmp <- c(sim_data[[site]], hist_data[[site]])

  yr_breaks <- stats::density(tmp, n = 50)$x

  # 2) then call density() with x from above
  sim_pdf <- lapply(seq_len(nsim), function(n1) {

    t2 <- sim_data %>%
      dplyr::filter_at("simulation", dplyr::any_vars(. == n1))

    tmp <- stats::density(
      t2[[site]],
      n = 50,
      from = min(yr_breaks),
      to = max(yr_breaks)
    )

    tmp
  })

  # 2b) and call on historical data
  tmp <- hist_data[[site]]
  hist_pdf <- stats::density(
    tmp,
    n = 50,
    from = min(yr_breaks),
    to = max(yr_breaks)
  )


  # 3) then create df with $y of all calls to density and $x as flow (x)
  hist_pdf <- data.frame(
    x = hist_pdf$x,
    density = hist_pdf$y
  )

  sim_pdf <- do.call(rbind, lapply(seq_len(nsim), function(n1) {

    tmp <- data.frame(
      x = sim_pdf[[n1]]$x,
      density = sim_pdf[[n1]]$y
    )

    tmp
  }))

  # 4) then plot with ggplot() + geom_boxplot()

  gg <- ggplot(sim_pdf, aes_string("x", "density")) +
    geom_boxplot(aes_string(group = "x")) +
    geom_line(data = hist_pdf, aes_string("x", "density")) +
    labs(
      x = paste0("Flow (", base_units, ")"), y = "Probability Density",
      title = "Annual CDF"
    )

  gg
}

create_mon_cdf <- function(sim_data, hist_data, nsim, site, base_units, which,
                           ...)
{
  # TODO: do we need to re-do this based on Balaji/Ken's code? Using density
  # instead of hist()
  # compute histograms for all simulations
  # 1) call density() initially
  # find which months are supposed to be processed
  proc_mon <- 1:12
  proc_mon <- proc_mon[proc_mon %in% which]

  mon_breaks <- lapply(proc_mon, function(mm) {
    tmp <- c(
      dplyr::filter_at(hist_data, "month", dplyr::any_vars(. == mm))[[site]],
      dplyr::filter_at(sim_data, "month", dplyr::any_vars(. == mm))[[site]]
    )
    stats::density(tmp, n = 50)$x
  })
  names(mon_breaks) <- month.abb[proc_mon]

  # 2) then call density() with x from above
  sim_pdf <- lapply(seq_len(nsim), function(n1) {
    tmp <- list()

    for (mm in proc_mon) {
      t2 <- sim_data %>%
        dplyr::filter_at("simulation", dplyr::any_vars(. == n1)) %>%
        dplyr::filter_at("month", dplyr::any_vars(. == mm))
      tmp[[month.abb[mm]]] <- stats::density(
        t2[[site]],
        n = 50,
        from = min(mon_breaks[[month.abb[mm]]]),
        to = max(mon_breaks[[month.abb[mm]]])
      )
    }

    tmp
  })

  # 2b) and call on historical data
  hist_pdf <- lapply(proc_mon, function(mm) {
    tmp <- dplyr::filter_at(
      hist_data,
      "month",
      dplyr::any_vars(. == mm)
    )[[site]]

    stats::density(
      tmp,
      n = 50,
      from = min(mon_breaks[[month.abb[mm]]]),
      to = max(mon_breaks[[month.abb[mm]]])
    )
  })

  names(hist_pdf) <- month.abb[proc_mon]

  # 3) then create df with $y of all calls to density and $x as flow (x)
  hist_pdf <- do.call(rbind, lapply(proc_mon, function(mm) {
    data.frame(
      "month" = mm,
      "x" = hist_pdf[[month.abb[mm]]]$x,
      "density" = hist_pdf[[month.abb[mm]]]$y
    )
  }))

  sim_pdf <- do.call(rbind, lapply(seq_len(nsim), function(n1) {
    tmp <- c()

    for (mm in proc_mon) {
      tmp <- rbind(tmp, data.frame(
        "month" = mm,
        "x" = sim_pdf[[n1]][[month.abb[mm]]]$x,
        "density" = sim_pdf[[n1]][[month.abb[mm]]]$y
      ))
    }

    tmp
  }))

  # 4) then plot with ggplot() + geom_boxplot()
  mon_labels <- month.abb
  names(mon_labels) <- 1:12

  gg <- list()

  for (mm in proc_mon) {
    pname <- paste0(month.abb[mm], "-cdf")
    gg[[pname]] <- ggplot(
      dplyr::filter_at(sim_pdf, "month", dplyr::all_vars(. %in% mm)),
      aes_string("x", "density")
    ) +
      geom_boxplot(aes_string(group = "x")) +
      geom_line(
        data = dplyr::filter_at(hist_pdf, "month", dplyr::all_vars(. %in% mm)),
        aes_string("x", "density")
      ) +
      facet_wrap(
        "month",
        ncol = 1,
        labeller = as_labeller(mon_labels),
        scales = "free"
      ) +
      labs(x = paste0("Flow (", base_units, ")"), y = "Probability Density")
  }

  gg
}
