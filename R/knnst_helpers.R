#' `knnst` Class
#'
#' The `knnst` class contains all output after performing the knn space-time
#' streamflow disaggregation
#'
#' The knn space-time disaggregation is performed by [knn_space_time_disagg()].
#' The `knnst` object contains all of the monthly data for the specified
#' number of simulations. The `index_years` that were selected for each
#' simulation are also stored in the `knnst` object.
#'
#' The data in the `knnst` object can be accessed through the following
#' functions:
#'   - [knnst_nsim()] will return the number of simulations, while
#'   - [knnst_index_years()] will return all of the selected index years.
#'
#' `knnst` objects can be converted to a `data.frame`: [as.data.frame.knnst()]
#'
#' @seealso [knn_space_time_disagg()], [knnst_nsim()], [knnst_index_years()]
#' @name knnst
NULL

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
    length(x) == 4,
    msg = "Should have length == 4"
  )

  disagg_names <- c("disagg_sims", "index_data", "mon_flow", "start_month")
  assert_that(
    all(names(x) %in% disagg_names),
    msg = paste(
      "Object should only have these named entries:",
      cat(disagg_names)
    )
  )

  assertthat::assert_that(
    all(
      sapply(
        seq_len(knnst_nsim(x)),
        function(ii) length(x$disagg_sims[[ii]])
      ) %in% 2
    ),
    msg = "Each disagg_flow entry should only have a length of 2."
  )

  knnst_names <- c("disagg_flow", "index_years")
  assertthat::assert_that(
    all(
      sapply(
        seq_len(knnst_nsim(x)),
        function(ii) names(x$disagg_sims[[ii]])
      ) %in% knnst_names
    ),
    msg = "Unexpected names in knnst object's `disagg_sims` list were found."
  )

  # check that each matrix has 12*number of index years
  nmonths <- sapply(
    seq_len(knnst_nsim(x)),
    function(i) nrow(x$disagg_sims[[i]]$disagg_flow)
  )
  nyears <- sapply(
    seq_len(knnst_nsim(x)),
    function(i) length(x$disagg_sims[[i]]$index_years)
  )

  assertthat::assert_that(
    all(nyears * 12 == nmonths),
    msg = "Months in `disagg_flow` should equal 12 * number of `index_years`"
  )

  x
}

#' Get the number of disaggregation simulations from `knnst` objects
#'
#' @param disagg A `knnst` object
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
#'   flow_mat, ind_flow, mon_flow, start_month = 1, scale_sites = 1:2, nsim = 5
#' )
#'
#' # will return 5
#' knnst_nsim(x)
#'
#' @export
knnst_nsim <- function(disagg)
{
  assertthat::assert_that(
    is_knnst(disagg),
    msg = "`disagg` should be a `knnst` object"
  )

  length(disagg$disagg_sims)
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
#'   flow_mat, ind_flow, mon_flow, start_month = 1, scale_sites = 1:2, nsim = 5
#' )
#'
#' # will be a 3 x 5 matrix:
#' knnst_index_years(x)
#'
#' @export
knnst_index_years <- function(disagg)
{
  assertthat::assert_that(
    is_knnst(disagg),
    msg = "`disagg` should be a `knnst` object"
  )

  do.call(
    cbind,
    lapply(
      seq_len(knnst_nsim(disagg)),
      function(x) disagg$disagg_sims[[x]]$index_years
    )
  )
}

#' @export
print.knnst <- function(x, ...)
{
  nsim <- knnst_nsim(x)
  nyears <- nrow(knnst_index_years(x))
  nsites <- ncol(knnst_get_disagg_data(x, 1))

  cat(
    "`knnst`:",
    paste("  ", nsim, "disaggregation simulations"),
    paste("  ", nsites, "sites"),
    paste("  ", nyears, "years of disaggregated data"),
    sep = "\n"
  )

  invisible(x)
}

#' Convert a `knnst` object to a `data.frame`
#'
#' `knnst` objects can be converted to `data.frame`s. When doing so, the
#' disaggregated monthly data are combined with the a simulation number, and
#' the index years, are repeated for each month. The rownames of the
#' disaggregated data (yyyy-mm) format are kept, and additional columns are
#' added for year and month, by themselves. Each site (column) in the
#' disaggregated flow data are named S1, S2, S3, ...
#'
#' @param x A `knnst` object.
#'
#' @param ... additional arguments to be passed to or from methods.
#'
#' @export

as.data.frame.knnst <- function(x, ...)
{
  nsim <- knnst_nsim(x)
  index_yrs <- knnst_index_years(x)

  do.call(
    rbind,
    lapply(seq_len(nsim), function(i) {
      tmp_m <- knnst_get_disagg_data(x, i)
      if (is.null(colnames(tmp_m)))
        colnames(tmp_m) <- paste0("S", 1:ncol(tmp_m))

      # rownames are yyyy-mm
      ym <- rownames(tmp_m)
      rownames(tmp_m) <- NULL

      # get year and month by themselves
      yy <- simplify2array(strsplit(ym, "-"))
      mm <- as.numeric(yy[2,])
      yy <- as.numeric(yy[1,])

      # repeat the index year for each monthly entry
      tmp_iy <- as.vector(matrix(
        rep(index_yrs[,i], 12),
        ncol = nrow(index_yrs),
        byrow = TRUE
      ))

      # now create the data.frame
      data.frame(
        ym = ym,
        year = yy,
        month = mm,
        as.data.frame(tmp_m, stringsAsFactors = FALSE),
        simulation = i,
        index_year = tmp_iy,
        stringsAsFactors = FALSE
      )
    })
  )
}

#' Get the disaggregated data
#'
#' `knnst_get_disagg_data()` gets the disaggregated streamflow data from a
#' `knnst` object.
#'
#' As `knnst` objects can contain multiple simulations of disaggregated data,
#' `sim_num` specifies which simulation of data to return.
#'
#' @inheritParams knnst_nsim
#'
#' @param sim_num The simulation number.
#'
#' @return A matrix.
#'
#' @export

# TODO: need to add tests for this function by itsefl
knnst_get_disagg_data <- function(disagg, sim_num = 1)
{
  check_sim_num(sim_num, disagg, "knnst_get_disagg_data")

  disagg$disagg_sims[[sim_num]]$disagg_flow
}

check_sim_num <- function(sim_num, disagg_flow, called_from)
{
  assert_that(
    length(sim_num) == 1 && is.numeric(sim_num) &&
      sim_num <= knnst_nsim(disagg_flow) && sim_num > 0,
    msg = paste0(
      "In `", called_from,
      "()`, `sim_num` should be a postivie numeric <= `knnst_nsim(x)`."
    )
  )
}

get_pattern_flow_data_df <- function(x, site)
{
  drop_cols <- c("ym", "year", "month", "simulation", "index_year")
  all_cols <- names(as.data.frame(x))

  tmp <- all_cols[!(all_cols %in% drop_cols)]

  # assume years correspond to the years from the annual index data
  # assume that data start in January
  yy <- x$index_data[,1]

  ym <- expand.grid(1:12, yy)
  mm <- ym[,1]
  yy <- ym[,2]
  ym <- paste(
    ym[,2],
    formatC(ym[,1], width = 2, format = "d", flag = "0"),
    sep = "-"
  )

  x_mon <- data.frame(
    ym = ym,
    year = yy,
    month = mm,
    simulation = 1
  )

  # as.numeric, in case the monthly index data is xts
  x_mon[[site]] <- as.numeric(x$mon_flow[, match(site, tmp)])

  x_mon
}
