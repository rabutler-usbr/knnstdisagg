#' Write all the knnst disagg data to disk
#'
#' `write_knnst()` writes all of the disaggregated data and the index years
#' selected for the disaggregation for each simulation to disk.
#'
#' The disaggregated data and the index years are saved to separate files.
#' The disaggregated data has one file for each simulation, while all index
#' years are saved to one file. The disaggregated data are saved as
#' "disagg_\[n\].csv", where n is the simulation number. All index years are
#' saved to index_years.csv. A README.txt file is also created which includes
#' meta data associated with how/when the data were generated.
#'
#' @param disagg A knnst object
#'
#' @param path A character scalar containing the path to the folder to write the
#'   disaggregated data.
#'
#' @examples
#' \dontrun{
#' write_knnst(ex_disagg, ".")
#' }
#'
#' @export

write_knnst <- function(disagg, path)
{
  assertthat::assert_that(
    is_knnst(disagg),
    msg = "`disagg` should be a `knnst` object."
  )

  assert_that(
    length(path) == 1 && is.character(path),
    msg = "`path` should be a character scalar."
  )

  assert_that(dir.exists(path), msg = "`path` should exist.")

  nsim <- knnst_nsim(disagg)

  lapply(seq_len(nsim), function(ii)
    utils::write.csv(
      knnst_get_disagg_data(disagg, ii),
      file = file.path(path, paste0("disagg_", ii, ".csv")),
      row.names = TRUE
    )
  )

  index_mat <- knnst_index_years(disagg)
  utils::write.csv(
    index_mat,
    file = file.path(path, "index_years.csv"),
    row.names = FALSE
  )

  write(format_meta(disagg[["meta"]]), file = file.path(path, "README.txt"))

  invisible(disagg)
}

format_meta <- function(meta)
{
  paste(
    "Associated index_years.csv and disagg_[n].csv files in this folder were",
    "generated/disaggregated using the knnstdisagg package.",
    "========================================================================",
    paste("user:", meta[["user"]]),
    paste("date:", meta[["date"]]),
    paste("version:",  meta[["version"]]),
    paste("created by calling:", meta[["call"]]),
    sep = "\n"
  )
}
