
#' The default knn parameters
#'
#' `knn_params_default()` creates a [knn_params] object based on the parameter
#' definitions in Nowak et al. (2010).
#'
#' Nowak et al. (2010) selects from `k` nearest neighbors, where `k` is equal to
#' the squareroot of the number of observations in the index data set. The
#' `k` neighbors use a weighting function that decreases based on distance.
#' The weights are eqaul to (1 / `n`) / sum(1 / `i`) where `i` goes from 1 to
#' `k`.
#'
#' @param n The number of observations in the index data set
#'
#' @examples
#' # there are 100 observations
#' knn_params_default(100)
#' # there are 20 observations in the data
#' knn_params_default(20)
#'
#' @seealso
#' [knn_params]
#'
#' @export
knn_params_default <- function(n)
{
  k <- floor(sqrt(n)) # floor was never used before, but that's the implication
  # of indexing into a vector with a decimal

  # defines matrix for weights
  w <- 1 / matrix(seq_len(k), ncol = 1)
  w <- w / sum(w)

  knn_params(k, w)
}
