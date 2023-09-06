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
  shape <- plot_ops("shape", ...)
  color <- plot_ops("color", ...)
  size <- plot_ops("size", ...)

  ggplot(x[["disagg_cor"]], aes(x = .data[["month2"]], y = .data[["cor"]])) +
    geom_boxplot(aes(group = .data[["month2"]]), width = 0.5) +
    facet_grid(rows = "month1", switch = "y") +
    theme(axis.ticks.x = element_blank()) +
    geom_point(
      data = x[["pattern_cor"]],
      color = color, size = size, shape = shape
    ) +
    scale_y_continuous(position = "right") +
    labs(
      title = paste("Temporal correlation at", x[["site"]]),
      y = "correlation", x = NULL,
      caption = caption_text(x[["bin_size"]], "points")
    ) +
    scale_x_discrete(position = "top")
}
