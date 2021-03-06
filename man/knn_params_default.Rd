% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knn_params_default.R
\name{knn_params_default}
\alias{knn_params_default}
\title{The default knn parameters}
\usage{
knn_params_default(n)
}
\arguments{
\item{n}{The number of observations in the index data set}
}
\description{
\code{knn_params_default()} creates a \link{knn_params} object based on the parameter
definitions in Nowak et al. (2010).
}
\details{
Nowak et al. (2010) selects from \code{k} nearest neighbors, where \code{k} is equal to
the square root of the number of observations in the index data set. The
\code{k} neighbors use a weighting function that decreases based on distance.
The weights are equal to (1 / \code{n}) / sum(1 / \code{i}) where \code{i} goes from 1 to
\code{k}.
}
\examples{
# there are 100 observations
knn_params_default(100)
# there are 20 observations in the data
knn_params_default(20)

}
\seealso{
\link{knn_params}
}
