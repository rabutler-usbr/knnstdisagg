#' Plot resulting correlation values after KNN space-time disaggregation
#'
#' Implements the `plot()` method for `knnst_spcor` and `knnst_tmpcor` objects
#' relying on ggplot2.
#'
#' `plon.knnst_spcor()` implements the `plot()` method for `knnst_spcor` objects
#' relying on ggplot2. See [knnst_spatial_cor()] to create `knnst_spcor` objects.
#' Each site a correlation was computed from will be its own facet in the plot.
#'
#' `...` is passed to [geom_point()] and [geom_line()] to overwrite defaults for
#' `size`, `shape`, and `color`.
#'
#' @param x An object inheriting from class `knnst_spcor` or `knnst_tmpcor`.
#'
#' @param x_names Boolean. If `TRUE` use names for the x axis labels, otherwise
#'   use site numbers.
#'
#' @param ncol,nrow Number of columns and rows to use in
#'   [ggplot2::facet_wrap()].
#'
#' @param ... Arguments to be passed to subsequent methods.
#'
#' @examples
#' sp_cor <- knnst_spatial_cor(ex_disagg, c("Cameo", "Hoover"), 100)
#' # show as 1 column and 2 rows
#' plot(sp_cor)
#'
#' # show as 2 columns and 1 row
#' plot(sp_cor, ncol = 2)
#'
#' # include the site names in the x axis labels
#' plot(sp_cor, x_names = TRUE)
#'
#' @export
plot.knnst_spcor <- function(x, x_names = FALSE, ncol = 1, nrow = NULL, ...) {
  #TODO also add in option to add legend if using numbers

  assert_that(is.logical(x_names) && length(x_names) == 1)

  res <- x[["disagg_cor"]]
  hist_cor <- x[["pattern_cor"]]
  sites <- x[["orig_sites"]]

  if (x_names) {
    res$site_to <- factor(res$site_to, levels = sites)
    hist_cor$site_to <- factor(hist_cor$site_to, levels = sites)
  } else {
    res$site_to <- as.factor(match(res$site_to, sites))
    hist_cor$site_to <- as.factor(match(hist_cor$site_to, sites))
  }

  res$site_from <- factor(res$site_from, levels = sites)
  hist_cor$site_from <- factor(hist_cor$site_from, levels = sites)

  shape <- plot_ops("shape", ...)
  color <- plot_ops("color", ...)
  size <- plot_ops("size", ...)

  # user parameters - ncol, nrow, max sites per page
  ggplot(res, aes(.data[["site_to"]], .data[["cor"]])) +
    geom_boxplot() +
    geom_point(
      data = hist_cor,
      aes(.data[["site_to"]], .data[["cor"]]),
      color = color, size = size, shape = shape
    ) +
    facet_wrap("site_from", ncol = ncol, nrow = nrow) +
    labs(
      title  = "Spatial correlation from:",
      x = "with site", y = "correlation",
      caption = caption_text(x[["bin_size"]], "points")
    )
}
