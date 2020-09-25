#' Function to compress the space given the equivalence relations.
#'
#' This function compresses the input space according to the equivalence relations, i.e., it compresses whenever an example has other elements inside its open ball but having the same class label as the ball-centered instance.
#' @param M sparse matrix representing all equivalence relations
#' @param Y numerical vector indentifying the output space of variables
#' @return A list containing sparse vectors (from package Matrix) identifying the equivalence relations
#' @keywords compress space
#' @export
compress_space <- function(M, Y) {
	flag = TRUE
	while (flag) {
		flag = FALSE
		row = 1
		while (row < length(M)) {
			if (length(M[[row]]@i) > 1) {
				connect.to = setdiff(M[[row]]@i, row)

				# Matrix reduction
				reduced = lapply(1:length(M), function(id) { 
						if (!is.null(M[[id]])) {
							ids = setdiff(M[[id]]@i, connect.to)
							if (length(ids) > 0) {
								for (j in 1:length(ids)) {
									ids[j] = ids[j] - sum(ids[j] > connect.to)
								}
								return (Matrix::sparseVector(i=ids, length=M[[id]]@length - length(connect.to)))
							}
							return (0)
						}
						return (0)
					  })

				M = list()
				counter = 1
				for (j in 1:length(reduced)) {
					if (!(j %in% connect.to)) {
						M[[ counter ]] = reduced[[ j ]]
						counter = counter + 1
					}
				}

				# Setting this flag to carry on operating
				flag = TRUE
				# Getting back to avoid loosing elements
				row = row - length(connect.to)
				if (row < 1) row = 1
			}
			row = row + 1
		}
	}

	return (M)
}
