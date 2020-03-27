#' Write all the knnst disagg data to disk
#'
#' `write_knnst()` writes all of the disaggregated data and the index years
#' selected for the disaggregation for each simulation to disk.
#'
#' The disagggregated data and the index years are saved to seperate files.
#' The disaggregated data has one file for each simulation, while all index
#' years are saved to one file. The dissaggregated data are saved as
#' "disagg_\[n\].csv", where n is the simulation number. All index years are
#' saved to index_years.csv.
#'
#' @param disagg A knnst object
#'
#' @param path A character scalar containing the path to the folder to write the
#'   disaggregated data.
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
      row.names = FALSE
    )
  )

  index_mat <- knnst_index_years(disagg)
  utils::write.csv(
    index_mat,
    file = file.path(path, "index_years.csv"),
    row.names = FALSE
  )

  invisible(disagg)
}
