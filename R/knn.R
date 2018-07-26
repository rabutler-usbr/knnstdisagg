
#' Get the index years using k-nearest neighbor method
#' 
#' For an index gage, select the years that are closest to a flow that needs to
#' be disaggregated, based on k-nearest neighbor selection. 
#' 
#' @param ann_flow A 2-column matrix, with years in column 1, and an annual flow
#'   in column 2. This is the annual flow that needs to be disaggregated.
#'   
#' @param ann_index_flow A 2-column matrix, with years in column 1, and an 
#'   annual flow in column 2. This is the index gage that the flows in 
#'   `ann_flow` will be compared to. After the comparison, the nearest neighbor
#'   year is selected.
#' 
#' @param k_weights If `NULL`, parameters are set based on definitions in Nowak
#'   et al. (2010). Users may force `k` and the `weights` by specifiying this 
#'   argument. It should be a list with two named entries: `k` and `weights`.
#'   `k` should be a single integer specifying the number of neightbors to 
#'   select from. `weights` should be a vector with length equal to `k`, 
#'   specifying the weights to apply to the 1-`k` neighbors for selecting the 
#'   neighbor.
#'   
#' @return N x 1 matrix of index years, with the number of rows, equal to the 
#'   number of rows in `ann_flow`.
#'   
#' @examples 
#' # a sample of three years of flow data
#' flow_mat <- cbind(c(2000, 2001, 2002), c(1400, 1567, 1325))
#' # made up historical data to use as index years
#' ind_flow <- cbind(1901:1980, rnorm(80, mean = 1500, sd = 300))
#' knn_get_index_year(flow_mat, ind_flow)
#' 
#' @references Nowak, K., Prairie, J., Rajagopalan, B., Lall, U. (2010).
#'   A nonparametric stochastic approach for multisite disaggregation of
#'   annual to daily streamflow. *Water Resources Research.*
#'
#' @export
knn_get_index_year <- function(ann_flow, ann_index_flow, k_weights = NULL)
{
  # check inputs -------------------
  assert_that(
    is.matrix(ann_flow) && ncol(ann_flow) == 2, 
    msg = "`ann_flow` should be a 2-column matrix"
  )
  
  if (!is.matrix(ann_index_flow) || ncol(ann_index_flow) != 2)
    stop("`ann_index_flow` should be a 2-column matrix", call. = FALSE)
  
  n_index_yrs <- nrow(ann_index_flow)
  
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }  
  
  # knn parameters (k) and weights
  if (is.null(k_weights)) {
    tmp <- knn_params(n_index_yrs)
    k <- tmp$k
    weights <- tmp$weights
  } else {
    stopifnot(is.list(k_weights))
    if (length(k_weights) != 2 || !all(names(k_weights) %in% c("k", "weights")))
      stop("`k_weights` should have only names `k` and `weights`")
    
    k <- k_weights$k
    if (length(k) != 1 || k < 1 || !is.wholenumber(k))
      stop("`k_weights$k` should be a single whole number >= 1", call. = FALSE)
    
    if (k > n_index_yrs)
      stop(
        "`k` should be <= the number of potential index years.", 
        call. = FALSE
      )
    
    weights <- k_weights$weights
    if (sum(weights) != 1)
      stop("`k_weights$weights` should sum to 1", call. = FALSE)
    
    if (length(weights) != k)
      stop("`length(weights)` should equal `k`", call. = FALSE)
  }
  
  # disagg all ann_flow values --------------
  x <- ann_flow[, 2, drop = FALSE]
  index_yrs <- apply(x, 1, knn_get_nn, ann_index_flow, k, weights)
  
  matrix(index_yrs, ncol = 1)
}

#' Returns "k" and weights for the KNN algorithm
#' 
#' @param n The number of observations
#' 
#' @noRd
knn_params <- function(n)
{
  k <- floor(sqrt(n)) # floor was never used before, but that's the implication
  # of indexing into a vector with a decimal
  
  # defines matrix for weights
  w <- 1 / matrix(seq_len(k), ncol = 1)
  w <- w / sum(w)
  
  list(k = k, weights = w)
}


knn_get_nn <- function(ann_flow, ann_index_flow, k, w)
{
  stopifnot(length(ann_flow) == 1)
  
  D <- abs(ann_flow - ann_index_flow[, 2])
  
  # combines difference and corresponding year into one matrix
  delta <- cbind(ann_index_flow[, 1], D) 
  
  # reorders the delta matrix based on distances
  delta_sort <- delta[sort.int(delta[,2], index.return = TRUE)$ix, ]

  if (k != 1) {
    # selects the "k-nearest-neighbors" from Delta_sort 
    kmatrix <- delta_sort[1:k, 1:2, drop = FALSE] 
    ind_year <- sample(
      kmatrix[, 1, drop = FALSE], 
      size = 1, 
      replace = TRUE, 
      prob = w
    )
  } else {
    ind_year <- delta_sort[1, 1, drop = FALSE]
  }
  
  ind_year
}
