#' @author Ken Nowak, Balaji Rajagopalan
#' @noRd
skew <- function(x)
{
  x1 <- x[!is.nan(x)]
  n <- length(x1)
  nfact <- n / ((n - 1) * (n - 2))
  xm <- mean(x, na.rm = TRUE)
  x_sd <- stats::sd(x, na.rm = TRUE)
  skew = sum((x1 - xm)^3)

  (nfact * skew) / x_sd^3
}
