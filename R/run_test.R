#' Function to test the shattering coefficient estimation
#'
#' This is a simple test function
#' @param mean 2-d numeric vector with Gaussian averages
#' @param n number of instances to be synthetically generated
#' @param length number of instances per class
#' @param quantile.percentage real number to define the quantile of distances to be considered (e.g. 0.1 means 10%)
#' @param plot should we plot?
#' @param epsilon a real threshold to be removed from distances in order to measure the open balls in the underlying topology
#' @return A list including the number of hyperplanes and the shattering coefficient function
#' @keywords run shattering estimation test
#'
#' @examples
#' # A first basic example:
#' run_test(mean=c(-1,1), n=100, length=5, quantile.percentage=0.1, plot=TRUE, epsilon=1e-7)
#'
#' # Changing the averages of the 2D Gaussian functions:
#' run_test(mean=c(-5,5), n=100, length=5, quantile.percentage=0.1, plot=TRUE, epsilon=1e-7)
#'
#' # Considering all points of the space (see parameter quantile.percentage):
#' run_test(mean=c(-5,5), n=50, length=5, quantile.percentage=1, plot=TRUE, epsilon=1e-7)
#'
#' # Considering 5% of the closest points of the space (see parameter quantile.percentage):
#' run_test(mean=c(-5,5), n=100, length=5, quantile.percentage=0.05, plot=TRUE, epsilon=1e-7)
#'
#' # Changing the number of random examples to 300
#' # (the positive class with 150 and the negative with the remaining 150 examples)
#' run_test(mean=c(-5,5), n=150, length=5, quantile.percentage=0.05, plot=TRUE, epsilon=1e-7)
#'
#' # Parameter epsilon defines some value to be removed from the closest point of the positive
#' # class to its nearest neighbor of the negative class (and vice-versa):
#' run_test(mean=c(-5,5), n=50, length=5, quantile.percentage=0.05, plot=TRUE, epsilon=1e-7)
#'
#' @export
run_test <- function(mean=c(-1,1), n=100, length=5, quantile.percentage=1, plot=TRUE, epsilon=1e-7) {

	oldpar <- par(no.readonly = TRUE)
	on.exit(par(oldpar))

	# Creating a synthetic dataset
	data = cbind(stats::rnorm(mean=mean[1], sd=1, n=n), stats::rnorm(mean=mean[1], sd=1, n=n), rep(1, n))
	data = rbind(data, cbind(stats::rnorm(mean=mean[2], sd=1, n=n), stats::rnorm(mean=mean[2], sd=1, n=n), rep(2, n)))

	# Building up the input and output sets
	X = data[,1:2]
	Y = data[,3]

	oldpar = NULL
	if (plot) {
        	oldpar = graphics::par(no.readonly = TRUE)
		graphics::par(mfrow=c(1,3))
		plot(X, col=Y, main="Original dataset", xlab="Attribute 1", ylab="Attribute 2")
	}

	Hyperplanes = estimate_number_hyperplanes(X, Y, length, quantile.percentage, epsilon)
	Shattering = estimate_shattering(Hyperplanes$estimation)

	ret = list()
	ret$number.hyperplanes = Hyperplanes
	ret$shattering.coefficient = Shattering

	if (plot) {
		plot(Hyperplanes$estimation[,1:2], main="Number of Homogeneous Regions (y axis) versus the Original dataset size (x axis)", xlab="Sample size", ylab="Reduced sample size", t="l")
		plot(cbind(Hyperplanes$estimation[,1], ret$shattering.coefficient[,1]), main="Shattering along the original dataset size (lower bound in black and upper bound in red)", xlab="Sample size", ylab="Estimated Shattering value", t="l", ylim=range(ret$shattering.coefficient))
		graphics::lines(cbind(Hyperplanes$estimation[,1], ret$shattering.coefficient[,2]), col=2)
       		on.exit(graphics::par(oldpar))
	}

	return (ret)
}
