#' `plot.knnst` implements the [plot] method for `knnst` objects, and relies on
#' ggplot2
#'
#' @param x An object inheriting from class `knnst`
#'
#' @param ... arguments to be passed to subsequent methods
#'
#' @param site The site to plot. Site name as a character.
#'
#' @param sim_num The simulation to plot. Simulation number (numerical).
#'
#' @export
plot.knnst <- function(x, ..., site = "S1", sim_num = 1)
{
  # TODO: how to handle multiple simulations
  # for now, will only select one simulation number;
  # in the future, may accept multiple simulation numbers that will be
  # aggregated together before plotting

  check_sim_num(sim_num, x, "plot.knnst")

  assert_that(
    length(site) == 1 && is.character(site),
    msg = "In `plot.knnst()`, `site` should be a character with length of 1."
  )

  x_df <- as.data.frame(x)

  assert_that(
    site %in% names(x_df),
    msg = "In `plot.knnst()`, `site` should be a valid site name."
  )

  # monthly means

  # monthly standard deviation

  # monthly skew

  # monthly lag-1 correlation

  # monthly max

  # monthly min

  # monthly pdf

  # annual pdf
}
