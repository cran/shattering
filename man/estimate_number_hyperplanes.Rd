% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_number_hyperplanes.R
\name{estimate_number_hyperplanes}
\alias{estimate_number_hyperplanes}
\title{Function to estimate the number of hyperplanes required to classify such a data sample.}
\usage{
estimate_number_hyperplanes(
  X,
  Y,
  length = 20,
  quantile.percentage = 0.05,
  epsilon = 1e-07
)
}
\arguments{
\item{X}{matrix indentifying the input space of variables}

\item{Y}{numerical vector indentifying the output space of variables}

\item{length}{number of data points used to estimate the shattering coefficient}

\item{quantile.percentage}{real number to define the quantile of distances to be considered (e.g. 0.1 means 10\%)}

\item{epsilon}{a real threshold to be removed from distances in order to measure the open balls in the underlying topology}
}
\value{
A data frame whose columns are: (1) the original sample size; (2) the reduced sample size after connecting homogeneous space regions; (3) the lower bound for the number of hyperplanes required to shatter the input space; and (4) the upper bound for the number of hyperplanes required to shatter the input space
}
\description{
This function estimates the number of hyperplanes
}
\examples{

# Generating some random dataset with 2 classes:
# 50 examples in class 1 and 50 in class 2 (last column)
data = cbind(rnorm(mean=1, sd=1, n=50), rnorm(mean=1, sd=1, n=50), rep(1, 50))
data = rbind(data, cbind(rnorm(mean=-1, sd=1, n=50), rnorm(mean=-1, sd=1, n=50), rep(2, 50)))

# Building up the input and output sets
X = data[,1:2]
Y = data[,3]

# Plotting our dataset using classes as colors
plot(X, col=Y, main="Original dataset", xlab="Attribute 1", ylab="Attribute 2")

# Here we estimate the number of hyperplanes required to shatter (divide) the given sample
# in all possible ways according to the organization of points in the input space
Hyperplanes = estimate_number_hyperplanes(X, Y, length=10, quantile.percentage=0.1, epsilon=1e-7)

}
\keyword{estimate}
\keyword{hyperplanes}
\keyword{number}
