#' Save a knnstplot
#'
#' `save_knnstplot()` is a convenient function for saving the knn space-time
#' disaggregation diagnostic plots returned by [plot.knnst()] as either a pdf or
#' collection of png files.
#'
#' @param knnstplot An object of type `knnstplot`, i.e., object returned from
#'   [plot.knnst()].
#'
#' @param filename File name to create on disk. Should be a .png or .pdf file.
#'   If it is a .png file, then it will create multiple files with `%02d`
#'   inserted at the end of the file to ensure unique file names. Directory
#'   should already exist.
#'
#' @param width,height Plot size in inches. If not specified, defaults to 7.
#'
#' @return Invisibly returns `knnstplot`
#'
#' @export

save_knnstplot <- function(knnstplot, filename, width = NA, height = NA)
{
  assert_that(inherits(knnstplot, "knnstplot"))

  # check that base directory exists
  assert_that(dir.exists(dirname(filename)))

  # check that file extension is .png or .pdf
  ftype <- tools::file_ext(filename)
  assert_that(
    ftype %in% c("png", "pdf"),
    msg = "`filename` should be either a png or pdf."
  )

  if (is.na(width))
    width <- 7

  if (is.na(height))
    height <- 7

  plot_order <- c(
    paste0(month.abb, "-pdf"),
    "monthly-stats", "annual-pdf", "annual-stats"
  )

  if (ftype == "png") {
    # add in the number after the provided file name
    filename <- gsub(paste0(".", ftype), "", filename)
    i <- 1

    for (p in plot_order) {
      if (exists(p, where = knnstplot) && !is.null(knnstplot[[p]])) {
        tmp_f <- paste0(filename, sprintf("%02d", i), ".png")
        ggsave(tmp_f, plot = knnstplot[[p]], width = width, height = height,
               units = "in")

        i <- i + 1
      }

    }

  } else {
    # its a pdf - one file
    grDevices::pdf(filename, width = width, height = height)
    on.exit(grDevices::dev.off())
    for (p in plot_order) {
      if (exists(p, where = knnstplot) && !is.null(knnstplot[[p]]))
        print(knnstplot[[p]])
    }
  }

  invisible(knnstplot)
}
