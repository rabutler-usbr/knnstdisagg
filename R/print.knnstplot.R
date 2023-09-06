#' Print a knnstplot
#'
#' Interactively prints the `knnstplot`. If printing in a script, use
#' [save_knnstplot()] instead.
#'
#' @param x An object of type `knnstplot`, i.e., object returned from
#'   [plot.knnst()].
#'
#' @param ... other arguments not used by this method
#'
#' @return Invisibly returns `x`
#'
#' @export

print.knnstplot <- function(x, ...)
{
  if (!inherits(x, "knnstplot"))
    stop("`x` must inherit from knnstplot.")

  oask <- grDevices::devAskNewPage(TRUE)
  on.exit(grDevices::devAskNewPage(oask))

  plot_order <- c(
    paste0(month.abb, "-pdf"),
    "monthly-stats", "annual-pdf", "annual-stats"
  )

  for (p in plot_order) {
    if (exists(p, where = x) && !is.null(x[[p]])) {
      grDevices::dev.hold()
      print(x[[p]])
      grDevices::dev.flush()
    }
  }

  invisible(x)
}
