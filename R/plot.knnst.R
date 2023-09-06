#' Plot resulting statistics for knn space-time disaggregation
#'
#' `plot.knnst()` implements the [plot()] method for `knnst` objects, and relies
#' on ggplot2. It plots the key statistics the knn space-time disaggregation
#' method should be preserving.
#'
#' If there are more  than one simulations, the stats are computed for each
#' simulation across all years. If there is only one simulation, then the user
#' must specify the `bin_size` that is used to compute the stats across
#' moving windows of this specified size.
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
#' `...` is passed to [geom_point()] and [geom_line()] to overwrite defaults for
#' `size`, `shape`, and `color`.
#'
#' For the monthly statistics panel, months are ordered based on the
#' `start_month` provided in the original disaggregation, i.e., specified by the
#' user in [knn_space_time_disagg()].
#'
#' @param x An object inheriting from class `knnst`
#'
#' @param site The site to plot. Site name as a character.
#'
#' @param bin_size Number of years for each bin when there is only one
#'   simulation.
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
plot.knnst <- function(x, site, bin_size = NULL, base_units = NULL,
                       which = c(13, 14, 15), show = FALSE, ...)
{
  #TODO: update to handle multiple simulations
  # assert_that(
  #   knnst_nsim(x) == 1,
  #   msg = "plot.knnst() is currently only setup to work with one simulation."
  # )

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
  nyrs <- nrow(x$disagg_sims[[1]]$disagg_flow) / 12

  if (nsim == 1) {
    assert_that(is.numeric(bin_size) && length(bin_size) == 1)
  }

  # if there is more than 1 simulation, then set bin_size to full length of data
  if (nsim > 1) {
    message('More than 1 simulation exists, so correlation computed for all years in each simulation.')
    bin_size <- nyrs
  }

  assert_that(bin_size <= nyrs)

  x_df <- as.data.frame(x)
  all_cols <- names(x_df)

  assert_that(
    site %in% names(x_df),
    msg = "In `plot.knnst()`, `site` should be a valid site name."
  )

  # set flags for needing monthly/annual data
  are_mon <- are_ann <- FALSE
  if (any(which %in% c(1:12, 14)))
    are_mon <- TRUE

  if (any(which %in% c(13, 15)))
    are_ann <- TRUE

  # simulation stats ----------------
  if (are_mon)
    x_plot_data <- get_mon_plot_stats(
      x_df,
      site = site,
      start_month = x$start_month,
      bin_size = bin_size,
      yr = "year"
    )

  if (are_ann) {
    x_ann_plot_data <- get_ann_plot_stats(x_df, site, x$start_month, bin_size)
    x_ann_sim_data <- x_df %>%
      dplyr::group_by_at(c("year", "simulation")) %>%
      dplyr::summarise_at(site, sum)
  }
  # get historical stats -------------------
  # get historical data as a data frame, organize, and compute same stats
  # as on the simulated data
  nhist_yrs <- nrow(x$mon_flow) / 12
  x_mon <- get_pattern_flow_data_df(x, site)

  if (are_mon) {
    x_mon_stats <- get_mon_plot_stats(
      x_mon,
      site = site,
      start_month = x$start_month,
      bin_size = nhist_yrs,
      yr = "agg_year"
    )
  }
  # annual historical data
  if (are_ann) {
    x_ann <- get_historical_annual(x_mon, site, x$start_month)
    x_ann_stats <- get_ann_plot_stats(x_mon, site, x$start_month, nhist_yrs)
  }

  gg1 <- gg2 <- gg3 <- gg4 <- NULL
  # monthly plots ----------------
  if (14 %in% which)
    gg1 <- create_mon_bxp(x_plot_data, x_mon_stats, site, x$start_month,
                          base_units, bin_size, nsim, ...)

  if (any(1:12 %in% which))
    gg2 <- create_mon_cdf(x_df, x_mon, bin_size, site, base_units, which,
                          nsim, ...)

  # annual plots --------------------
  if (15 %in% which)
    gg3 <- create_ann_bxp(x_ann_plot_data, x_ann_stats, site, base_units,
                          bin_size, nsim, ...)

  if (13 %in% which)
    gg4 <- create_ann_cdf(x_ann_sim_data, x_ann, bin_size, site, base_units,
                          nsim, ...)

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

# yr is which year variable to keep.
get_mon_plot_stats <- function(x_df, site, start_month, bin_size, yr = "year")
{
  keep_cols <- c("ym", yr, "month", site, "simulation")
  vars_group <- c("month", "simulation")

  # if yr is year, then convert it to agg_year
  # convert year to agg_year
  if (yr == "year") {
    x_df[["year"]] <- x_df[["ym"]]
    x_df <- dplyr::mutate_at(
      x_df,
      "year",
      list(~get_agg_year(., start_month))
    )
  }

  x_df %>%
    # subset to site
    dplyr::select_at(keep_cols) %>%
    dplyr::group_by_at("simulation") %>%
    dplyr::arrange_at("ym") %>%
    # compute stats for all months
    get_plot_stats(
      var_mutate = site, vars_group = vars_group, bin_size = bin_size, yr = yr
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(
      "month",
      list(~ factor(month.abb[.], levels = month.abb))
    )
}

get_ann_plot_stats <- function(x_df, site, start_month, bin_size)
{

  x_df[["agg_year"]] <- x_df[["ym"]]

  x_df %>%
    # get the "agg_year" from ym
    dplyr::mutate_at("agg_year", list(~ get_agg_year(., start_month))) %>%
    # sum to annual values
    dplyr::select_at(c("agg_year", site, "simulation")) %>%
    dplyr::group_by_at(c("agg_year", "simulation")) %>%
    dplyr::summarise_at(site, list("ann" = sum)) %>%
    # compute the same stats as monthly
    dplyr::group_by_at("simulation") %>%
    dplyr::arrange_at("agg_year") %>%
    get_plot_stats(var_mutate = "ann", vars_group = "simulation", bin_size, "agg_year")

}

# for a single variable `var_mutate`, compute the mean, variance, max, min,
# lag-1 correlation, and skew
get_plot_stats <- function(x_df, var_mutate, vars_group, bin_size, yr)
{
  var_name_order <- c(
    "mean" = "Mean",
    "stats::var" = "Variance",
    "max" = "Maximum",
    "min" = "Minimum",
    "stats::cor" = "Lag-1 Correlation",
    "skew" = "Skew"
  )

  # it is already arranged by ym, so get the agg_year from the first and last
  # entries to compute number of years
  start_year <- min(x_df[[yr]])
  nbin <- max(x_df[[yr]]) - start_year + 1 - bin_size + 1

  res <- data.frame()

  # TODO: make this more efficient!!!!!
  # pre-allocating an array and then converting to df took 1.75 seconds in a
  # simple test, will staying as a data frame and using bind_rows to 3.08
  # seconds

  for (i in seq_len(nbin)) {
    yr_st <- start_year + i - 1
    yr_end <- yr_st + bin_size - 1
    tmp <- x_df %>%
      dplyr::filter_at(yr, dplyr::all_vars(. %in% yr_st:yr_end)) %>%
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

    tmp[["bin"]] <- i

    res <- dplyr::bind_rows(res, tmp)
  }

  res
}

create_ann_bxp <- function(sim_data, hist_data, site, base_units = NULL,
                           bin_size, nsim, ...)
{
  shape <- plot_ops("shape", ...)
  color <- plot_ops("color", ...)
  size <- plot_ops("size", ...)

  gg <- ggplot(sim_data, aes_string(y = "Value")) +
    geom_boxplot() +
    facet_wrap("Variable", ncol = 2, scales = "free_y") +
    geom_point(
      data = hist_data,
      aes_string(x = 0, y = "Value"),
      shape = shape,
      color = color,
      size = size
    ) +
    labs(
      x = NULL,
      title = paste(site, "- Annual Statistics"),
      y = paste("Base units =", base_units),
      caption = caption_text(bin_size, nsim, "points")
    ) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
    scale_x_continuous(breaks = 0) +
    coord_cartesian( xlim = c(-1, 1))

  gg
}

create_mon_bxp <- function(sim_data, hist_data, site, start_month,
                           base_units = NULL, bin_size, nsim, ...)
{
  shape <- plot_ops("shape", ...)
  color <- plot_ops("color", ...)
  size <- plot_ops("size", ...)

  sim_data[["month"]] <- factor(
    sim_data[["month"]],
    levels = month.abb[full_year(start_month)]
  )

  hist_data[["month"]] <- factor(
    hist_data[["month"]],
    levels = month.abb[full_year(start_month)]
  )

  gg <- ggplot(sim_data, aes_string("month", "Value")) +
    geom_boxplot(aes_string(group = "month")) +
    facet_wrap("Variable", ncol = 2, scales = "free_y") +
    geom_point(
      data = hist_data,
      aes_string("month", "Value"),
      shape = shape,
      size = size,
      color = color
    ) +
    labs(
      x = NULL,
      title = paste0(site, " - Monthly Statistics"),
      y = paste("Base units =", base_units),
      caption = caption_text(bin_size, nsim, "points")
    )

  gg
}

create_ann_cdf <- function(sim_data, hist_data, bin_size, site, base_units,
                           nsim, ...)
{
  cdf_data <- compute_cdf_data(sim_data, hist_data, bin_size, site)

  gg <- cdf_plot(cdf_data, base_units, title = paste(site, "- Annual CDF"),
                 bin_size, nsim, ...)

  gg
}

create_mon_cdf <- function(sim_data, hist_data, bin_size, site, base_units,
                           which, nsim, ...)
{
  # TODO: do we need to re-do this based on Balaji/Ken's code? Using density
  # instead of hist()
  # compute histograms for all simulations
  # 1) call density() initially
  # find which months are supposed to be processed
  proc_mon <- 1:12
  proc_mon <- proc_mon[proc_mon %in% which]

  mon_labels <- month.abb
  names(mon_labels) <- 1:12

  sim_data <- dplyr::arrange_at(sim_data, "ym")
  hist_data <- dplyr::arrange_at(hist_data, "ym")

  gg <- list()

  for (mm in proc_mon) {
    tmp <- compute_cdf_data(
      dplyr::filter_at(sim_data, "month", dplyr::all_vars(. == mm)),
      dplyr::filter_at(hist_data, "month", dplyr::all_vars(. == mm)),
      bin_size,
      site
    )

    # TODO: should boxplot width be computed?
    pname <- paste0(month.abb[mm], "-cdf")
    gg[[pname]] <- cdf_plot(
      tmp,
      base_units,
      title = paste0(site, " - ", month.name[mm], " CDF"),
      bin_size,
      nsim,
      ...
    )
  }

  gg
}

# computes cdf data for one site and one month/year. Assumes sim_data and
# hist_data have already been filtered to correct month/years
compute_cdf_data <- function(sim_data, hist_data, bin_size, site)
{
  # compute the breaks ----
  tmp <- c(hist_data[[site]], sim_data[[site]])
  mon_breaks <- stats::density(tmp, n = 25)$x

  # create all bins --------
  # this is essentially annual data
  all_bins <- get_all_bins(sim_data[[site]], bin_size, monthly = FALSE)

  # call stats::density on each bin -------
  dd <- apply(
    all_bins,
    2,
    stats::density,
    n = 25,
    from = min(mon_breaks),
    to = max(mon_breaks)
  )

  # get all the x, y values from every column and
  sim_data_res <- do.call(rbind, lapply(1:ncol(all_bins), function(i) {
    data.frame(
      "x" = dd[[i]]$x,
      "density" = dd[[i]]$y
    )
  }))

  hist_data_res <- stats::density(
    hist_data[[site]],
    n = 25,
    from = min(mon_breaks),
    to = max(mon_breaks)
  )
  hist_data_res <- data.frame(
    x = hist_data_res$x,
    density = hist_data_res$y
  )

  list(sim_cdf = sim_data_res, hist_cdf = hist_data_res)
}

# assumes x is alreadly order correctly. Given data x, and the bin_size,
# return a matrix of all the data which would have bin_size rows (*12 for
# monthly), and nrow(x) - bin_size + 1 columns
get_all_bins <- function(x, bin_size, monthly = TRUE)
{
  mult <- ifelse(monthly, 12, 1)
  nbins <- length(x) / mult - bin_size + 1

  select_data <- function(start_index, zz, bin_size, monthly) {
    if (monthly) {
      start_index <- start_index * 12 - 11
      zz <- zz[start_index:(bin_size * 12 + start_index - 1)]
    } else {
      # annual data
      zz <- zz[start_index:(bin_size + start_index - 1)]
    }

    zz
  }

  all_bins <- simplify2array(
    lapply(1:nbins, select_data, x, bin_size, monthly)
  )

  all_bins
}

# given a yyyy-mm and start_month, determine its "aggregation" year. ex water
# year or fiscal year.
get_agg_year <- function(x, start_month)
{
  x <- simplify2array(strsplit(x, "-", fixed = TRUE))
  yy <- as.numeric(x[1,])
  mm <- as.numeric(x[2,])

  if (start_month != 1)
    yy[mm %in% start_month:12] <-  yy[mm %in% start_month:12] + 1

  yy
}

get_historical_annual <- function(x, site, start_month)
{
  x[["agg_year"]] <- x[["ym"]]
  x %>%
    dplyr::mutate_at("agg_year", list(~ get_agg_year(., start_month))) %>%
    dplyr::group_by_at(c("agg_year", "simulation")) %>%
    dplyr::summarise_at(site, sum)
}

cdf_plot <- function(cdf_data, base_units, title, bin_size, nsim, ...)
{
  color = plot_ops("color", ...)

  ggplot(cdf_data$sim_cdf, aes_string("x", "density")) +
    geom_boxplot(aes_string(group = "x")) +
    geom_line(
      data = cdf_data$hist_cdf,
      aes_string("x", "density"),
      color = color
    ) +
    labs(
      title = title,
      x = paste0("Flow (", base_units, ")"),
      y = "Probability Density",
      caption = caption_text(bin_size, nsim, "line")
    )
}

caption_text <- function(bin_size, nsim, pl = "point")
{

  if (nsim == 1) {
    paste0("Colored ", pl, " = input/pattern; boxplots = ", bin_size,
           "-year moving window on disaggregated data")
  } else {
    paste0("Colored ", pl, " = input/pattern; boxplots = stats computed across entire period for each simulation.")
  }
}

# checks ... to see if something was specified, otherwise sets it to default
plot_ops <- function(op, ...)
{
  args <- list(...)
  if (exists(op, where = args)) {
    x <- args[[op]]
  } else {
    defaults <- list(
      color = "#51B2FF",
      size = 2,
      shape = 17
    )

    x <- defaults[[op]]
  }

  x
}
