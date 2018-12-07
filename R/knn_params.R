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
#'   apply to the 1-`k` neighbors before selecting the neighbor. The weights
#'   must sum to 1.
#'
#' @examples
#' # uniformly select from the 5 nearest neighbors
#' knn_params(5, rep(1/5, 5))
#' # select from 4 neigbors, with decreasing likelihood of being selected
#' knn_params(4, c(.4, .3, .2, .1))
#' # select the nearest neighbor
#' knn_params(1, 1)
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
    round(sum(weights), 10) == 1,
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

#' @export
print.knn_params <- function(x, ...)
{
  k <- seq(1, x$k)
  print(data.frame("neighbor" = k, "weight" = x$weights))
  invisible(x)
}
