#' @details
#' `plot.knnst_tmpcor()` implements the `plot()` method for `knnst_tmpcor`
#' objects relying on ggplot2. See [knnst_temporal_cor()] to create
#' `knnst_tmpcor` objects.
#'
#' @rdname plot.knnst_spcor
#'
#' @examples
#' # use a 50-year bin size to compute the stats at Cameo
#' tmp_cor <- knnst_temporal_cor(ex_disagg, "Cameo", 50)
#' plot(tmp_cor)
#'
#' @export
plot.knnst_tmpcor <- function(x, ...)
{
  # ggplot(rolling_res, aes(x = 1, y = cor)) +
  #   geom_boxplot(width = 0.5) +
  #   facet_grid(rows = vars(month1), cols = vars(month2), switch = "y") +
  #   theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  #   geom_point(data = obs_res, color = "red") +
  #   scale_y_continuous(position = "right")

  ggplot(x[["disagg_cor"]], aes_string(x = "month2", y = "cor")) +
    geom_boxplot(aes_string(group = "month2"), width = 0.5) +
    facet_grid(rows = "month1", switch = "y") +
    theme(axis.ticks.x = element_blank()) +
    geom_point(data = x[["pattern_cor"]], color = "red") +
    scale_y_continuous(position = "right") +
    labs(
      title = paste("Temporal correlation at", x[["site"]]),
      y = "correlation", x = NULL,
      caption = paste0(
        "Red = input/pattern; boxplots = ",
        x[["bin_size"]],
        "-year moving window on disaggregated data"
      )
    ) +
    scale_x_discrete(position = "top")
}
