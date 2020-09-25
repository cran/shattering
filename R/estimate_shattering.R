#' Function to estimate the shattering coefficient function
#'
#' This function estimates the shattering coefficient
#' @param estimation This is the result from function estimate_number_hyperplanes
#' @return A data frame containing: (1) the lower bound for the shattering coefficient function; and (2) the upper bound for the shattering coefficient function
#' @keywords estimate shattering
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
#' # This is the function used to estimate the Shattering coefficient for this particular dataset
#' Shattering = estimate_shattering(Hyperplanes$estimation)
#'
#' # Plotting the number of homogeneous regions found in the input space
#' plot(Hyperplanes$estimation[,1:2],
#'    main="Number of Homogeneous Regions (y axis) versus the Original dataset size (x axis)",
#'    xlab="Sample size", ylab="Reduced sample size", t="l")
#'
#' # Plotting the number of hyperplanes necessary to shatter the input space
#' # (worst case in red and best case in black)
#' plot(cbind(Hyperplanes$estimation[,1], Shattering[,1]),
#'    main="Shattering along the original dataset size (lower bound in black and upper bound in red)",
#'    xlab="Sample size", ylab="Estimated Shattering value", t="l", ylim=range(Shattering))
#' lines(cbind(Hyperplanes$estimation[,1], Shattering[,2]), col=2)
#'
#' @export
estimate_shattering <- function(estimation) {

	if (is.null(estimation) || !is.data.frame(estimation)) {
		return ("Parameter estimation must be a data frame returned by function estimate_number_hyperplanes.")
	}

	if (nrow(estimation) <= 1) {
		return ("Parameter estimation must have more than a single row.")
	}

	if (sum(names(estimation) == c("n", "reduced", "Big.Omega", "Big.O")) != 4) {
		return ("Parameter estimation must contain four columns under the following names: n, reduced, Big.Omega, Big.O.")
	}

	all.lower = c()
	all.upper = c()

	for (i in 1:nrow(estimation)) {
		f_lower = 0
		f_upper = 0
		lower = estimation[i,"Big.Omega"]
		upper = estimation[i,"Big.O"]
		reduced = estimation[i,"reduced"]

		for (j in 1:round(2^lower)) { f_lower = f_lower + choose(reduced, j) }
		for (j in 1:round(2^upper)) { f_upper = f_upper + choose(reduced, j) }

		all.lower = c(all.lower, f_lower)
		all.upper = c(all.upper, f_upper)
	}

	shattering = data.frame(lower.bound=all.lower, upper.bound=all.upper)

	return (shattering)
}
