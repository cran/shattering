#' Computes the maximal number of space regions
#'
#' This function computes the maximal number of regions an R^n space can
#' be divided using m hyperplanes
#' @param m number of hyperplanes
#' @param n space dimensionality
#' @return Maximal number of space regions
#' @keywords complexity analysis of the shattering coefficient for some dataset
#' @section References: de Mello, R.F. (2019) "On the Shattering Coefficient of Supervised Learning Algorithms" arXiv:<https://arxiv.org/abs/1911.05461>
#' @section References: de Mello, R.F., Ponti, M.A. (2018, ISBN: 978-3319949888) "Machine Learning: A Practical Approach on the Statistical Learning Theory"
#' @section References: https://onionesquereality.wordpress.com/2012/11/23/maximum-number-of-regions-in-arrangement-of-hyperplanes/
#'
#' @examples
#'
#' number_regions(m=2, n=2)
#' @export
number_regions <- function(m, n) {
	regions = 1
	for (i in 1:n) {
		regions = regions + choose(m, i)
	}
	return (regions)
}

