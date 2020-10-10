#' Function to compress the space based on the equivalence relations.
#'
#' This function compresses the input space according to the equivalence relations, i.e., it compresses whenever an example has other elements inside its open ball but having the same class label as the ball-centered instance.
#' @param M sparse matrix representing all equivalence relations
#' @param Y numerical vector indentifying the output space of variables
#' @return A list containing sparse vectors (from package slam) identifying the equivalence relations
#' @keywords compress space
#' @export
compress_space <- function(M, Y) {
	flag = TRUE
	while (flag) {
		flag = FALSE
		row = 1
		while (row < length(M)) {
			if (!is.null(M[[row]]) && length(M[[row]]$i) > 1) {

				# row will represent the element and its neighbors
				connect.to = setdiff(M[[row]]$i, row) # [ 2 3 ]

				# Matrix reduction
				reduced = list()
				counter = 1
				for (i in 1:length(M)) { 
					if (!is.null(M[[i]])) {
						ids = setdiff(M[[i]]$i, connect.to)
						if ((length(ids) > 0 && !(i %in% connect.to)) || (length(ids) > 1 && (i %in% connect.to))) {
							red = as.numeric(lapply(ids, function(el) { return (el - sum(el > connect.to)) }))
							len = M[[i]]$dim - length(connect.to)
							reduced[[counter]] = slam::simple_sparse_array(i=red, v=rep(1, length(red)), dim=len)
							counter = counter + 1
						}
					}
				}

				# Copying list
				M = reduced
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
