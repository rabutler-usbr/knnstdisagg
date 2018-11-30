#' Class to specify the parameters for the knn selection
#'
#' `knn_params()` creates an object, which lets the user specify values for
#' the weights, and the number of neighbors to use when selecting the
#' nearest neighbor.
#'
#' @param k A single integer specifying the number of neighbors to
#'   select from.
#'
#' @param weights a vector with length equal to `k`, specifying the weights to
#'   apply to the 1-`k` neighbors before selecting the neighbor.
#'
#' @examples
#' # uniformly select from the 5 nearest neighbors
#' knn_params(5, rep(1/5, 5))
#'
#' @export

knn_params <- function(k, weights)
{
  validate_knn_params(new_knn_params(k, weights))
}

new_knn_params <- function(k, w)
{
  structure(list(k = k, weights = w), class = c("knn_params", "list"))
}

validate_knn_params <- function(x)
{
  assertthat::assert_that(
    length(x) == 2 && all(names(x) %in% c("k", "weights")),
    msg = "Should only have names `k` and `weights`"
  )

  k <- x$k
  assertthat::assert_that(
    all(length(k) == 1, k >= 1, is_wholenumber(k)),
    msg = "`k` should be a single whole number >= 1"
  )

  weights <- x$weights
  assertthat::assert_that(
    sum(weights) == 1,
    msg = "`weights` should sum to 1"
  )

  assertthat::assert_that(
    length(weights) == k,
    msg = "`length(weights)` should equal `k`"
  )

  x
}

is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

#' Test if the object inherits from  `knn_params` class
#'
#' @param x An object
#'
#' @return `TRUE` if the object inherits from the `knn_params` class.
#' @export
is_knn_params <- function(x)
{
  inherits(x, "knn_params")
}

#' The default knn parameters
#'
#' `knn_params_default()` creates a `knn_params` object based on the parameter
#' definitions in Nowak et al. (2010).
#'
#' Nowak et al. (2010) selects from `k` nearest neighbors, where `k` is equal to
#' the squareroot of the number of observations in the index data set. The
#' `k` neighbors use a weighting function that decreases based on distance.
#' The weights are eqaul to (1 / `n`) / sum(1 / i) where i goes from 1 to `k`.
#'
#' @param n The number of observations in the index data set
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
