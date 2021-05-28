#' Function to estimate the number of hyperplanes required to classify such a data sample.
#'
#' This function estimates the number of hyperplanes
#' @param X matrix indentifying the input space of variables
#' @param Y numerical vector indentifying the output space of variables
#' @param length number of data points used to estimate the shattering coefficient
#' @param quantile.percentage real number to define the quantile of distances to be considered (e.g. 0.1 means 10%)
#' @param epsilon a real threshold to be removed from distances in order to measure the open balls in the underlying topology
#' @return A data frame whose columns are: (1) the original sample size; (2) the reduced sample size after connecting homogeneous space regions; (3) the lower bound for the number of hyperplanes required to shatter the input space; and (4) the upper bound for the number of hyperplanes required to shatter the input space
#' @keywords estimate number hyperplanes
#'
#' @examples
#'
#' # Generating some random dataset with 2 classes:
#' # 50 examples in class 1 and 50 in class 2 (last column)
#' data = cbind(rnorm(mean=1, sd=1, n=50), rnorm(mean=1, sd=1, n=50), rep(1, 50))
#' data = rbind(data, cbind(rnorm(mean=-1, sd=1, n=50), rnorm(mean=-1, sd=1, n=50), rep(2, 50)))
#'
#' # Building up the input and output sets
#' X = data[,1:2]
#' Y = data[,3]
#'
#' # Plotting our dataset using classes as colors
#' plot(X, col=Y, main="Original dataset", xlab="Attribute 1", ylab="Attribute 2")
#'
#' # Here we estimate the number of hyperplanes required to shatter (divide) the given sample
#' # in all possible ways according to the organization of points in the input space
#' Hyperplanes = estimate_number_hyperplanes(X, Y, length=10, quantile.percentage=0.1, epsilon=1e-7)
#'
#' @export
estimate_number_hyperplanes <- function(X, Y, length=20, quantile.percentage=0.05, epsilon=1e-7) {

	if (!is.matrix(X)) {
		return ("Parameter X must be a matrix.")
	}

	if (!is.vector(Y)) {
		return ("Parameter Y must be a vector.")
	}

	if (nrow(X) < 100) {
		return ("The sample must contain more than 100 instances.")
	}

	if (floor(nrow(X) / length) < 10) {
		return ("Each sample assessed must contain at least 10 instances.")
	}

	ids = sample(1:nrow(X))
	X = as.data.frame(X[ids,])
	Y = as.numeric(Y[ids])

	f = NULL
	for (i in floor(seq(10, nrow(X), length=length))) {
		# Computing the equivalence relations
		M = equivalence_relation(X[1:i,], Y[1:i], quantile.percentage, epsilon)

		# Compressing the input space
		C = compress_space(M$relations, Y[1:i])

		# Computing the Big Omega and Big O bounds for the number of hyperplanes
		# according to the theoretical results by Har-Peled and Jones
		lower = length(C)^(2 / (ncol(X[1:i,]) + 1)) * log(log(length(C))) / log(length(C))
		upper = ncol(X[1:i,]) * length(C)^(2 / (ncol(X[1:i,]) + 1))

		f = rbind(f, c(nrow(X[1:i,]), length(C), lower, upper))
	}

	f = as.data.frame(f)
	colnames(f) = c("n", "reduced", "Big.Omega", "Big.O")
	regression = stats::lm(reduced ~ n, data=f) 
						    
	ret = list()
	ret$regression = regression
	ret$estimation = f

	return (ret)
}
