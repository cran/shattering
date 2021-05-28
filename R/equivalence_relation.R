#' Function to compute equivalence relations among input space points.
#'
#' This function computes the greatest as possible open ball connecting a given input example to every other under the same class label, thus homogeneizing space regions.
#' @param X matrix indentifying the input space of variables
#' @param Y numerical vector indentifying the output space of variables
#' @param quantile.percentage real number to define the quantile of distances to be considered (e.g. 0.1 means 10%)
#' @param epsilon a real threshold to be removed from distances in order to measure the open balls in the underlying topology
#' @param chunk number of elements to compute the Euclidean distances at once (if you set a large number, you might have memory limitations to perform the operations)
#' @return A list with the equivalence relations in form of a list
#' @keywords equivalence relation
#' @export
equivalence_relation <- function(X, Y, quantile.percentage=1, epsilon=1e-3, chunk=250) {

	radius = rep(0, nrow(X))
	for (c in unique(Y)) {
		ids = which(c == Y)
		radius[ids] = 
		   as.numeric(FNN::knnx.dist(X[setdiff(1:nrow(X), ids),], query=X[ids,], k=1, algorithm="kd_tree")) - epsilon
	}
	# Considering the distances up to a given quantile
	distance = as.numeric(stats::quantile(radius, quantile.percentage))

	# This filters out the closest distances to be considered in our analysis
	ids = which(radius <= distance)

	# Subspace of elements considered in our analysis
	radius = radius[ids]
	X = X[ids,]
	Y = Y[ids]

	R = list()
	for (c in unique(Y)) {
		# which elements in Y are equal to c?
		ids = which(c == Y)
		# for every element in ids
		for (sub in seq(1, length(ids), by=chunk)) {
			# selecting a subchunk
			elements = sub:(sub+chunk-1)
			if (sub+chunk-1 > length(ids)) {
				elements = sub:length(ids)
			}
			# computing distances
			all.dists = matrix(as.numeric(pdist::pdist(X, indices.A=ids[elements], 
								   indices.B=1:nrow(X))@dist), byrow=T, ncol=nrow(X))
			# for every row in the distance matrix
			for (i in 1:nrow(all.dists)) {
				pos = which(all.dists[i,] < radius[ids[elements[i]]]) 
				R[[ ids[elements[i]] ]] = slam::simple_sparse_array(i=pos, v=rep(1, length(pos)), dim=nrow(X))
			}
		}
	}

	#for (i in 1:length(R)) {
	#	plot(X, col=Y+2, cex=3, t="p", pch=20)
	#	points(matrix(X[as.vector(R[[i]]$i),], nrow=length(R[[i]]$i)), col=2, cex=2, pch=20)
	#	locator(1)
	#}

	ret = list()
	ret$X = X
	ret$Y = Y
	ret$relations = R

	return (ret)
}
