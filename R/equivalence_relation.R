#' Function to assess all equivalence relations.
#'
#' This function computes the greatest as possible open ball connecting a given input example to every other under the same class label, thus homogeneizing space regions.
#' @param X matrix indentifying the input space of variables
#' @param Y numerical vector indentifying the output space of variables
#' @param quantile.percentage real number to define the quantile of distances to be considered (e.g. 0.1 means 10%)
#' @param epsilon a real threshold to be removed from distances in order to measure the open balls in the underlying topology
#' @return A list with the equivalence relations in form of a list
#' @keywords equivalence relation
#' @export
equivalence_relation <- function(X, Y, quantile.percentage=0.05, epsilon=1e-7) {
	distance = -Inf
	radius = rep(0, nrow(X))
	for (c in unique(Y)) {
		ids = which(c == Y)
		radius[ids] = as.numeric(FNN::knnx.dist(X[setdiff(1:nrow(X), ids),], query=X[ids,], k=1, algorithm="kd_tree")) - epsilon
		dist = as.numeric(stats::quantile(radius[ids], quantile.percentage))
		if (dist > distance) { 
			distance = dist
		}
	}

	ids = which(radius <= distance)
	radius = radius[ids]
	X = X[ids,]
	Y = Y[ids]

	M <- 1:nrow(X)
	R = lapply(M, function(id) {
			pos = which(as.numeric(pdist::pdist(X, indices.A=id, indices.B = 1:nrow(X))@dist) < radius[id] & Y[id] == Y)
			r = Matrix::sparseVector(i=pos, length=nrow(X))
			return (r)
		})

	return (R)
}
