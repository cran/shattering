#' Produce a set of SVM classifiers
#'
#' This function outputs a set of SVM classifiers to perform the supervised learning task 
#' based on the topological data analysis
#' @param X matrix defining the input space of your dataset
#' @param Y numerical vector defining the output space (labels/classes) of your dataset 
#' @param train.size fraction of examples used for training
#' @param quantile.percentage real number to define the quantile of distances to be considered (e.g. 0.1 means 10%)
#' @param min.points minimal number of examples per classification region of the input space
#' @param gamma.length number of possible gamma parameters to test the radial kernel for SVM
#' @param cost the cost for the SVM optimization
#' @param weights weights to be used in our SVM optimization
#' @param best.stdev.purity the stdev to compute data purity
#' @return A list of classifiers composing the final classification model
#' @keywords complexity analysis of the shattering coefficient for some dataset
#' @section References: de Mello, R.F. (2019) "On the Shattering Coefficient of Supervised Learning Algorithms" arXiv:<https://arxiv.org/abs/1911.05461>
#' @section References: de Mello, R.F., Ponti, M.A. (2018, ISBN: 978-3319949888) "Machine Learning: A Practical Approach on the Statistical Learning Theory"
#'
#' @examples
#'
#' # require(NMF)
#' # 
#' # X = cbind(rnorm(mean=-1, sd=1, n=200), rnorm(mean=-1, sd=1, n=200))
#' # X = rbind(X, cbind(rnorm(mean=1, sd=1, n=200), rnorm(mean=1, sd=1, n=200)))
#' # Y = c(rep(-1,200), rep(+1,200))
#' # plot(X, col=Y+2, pch=20, cex=3, cex.axis=2)
#' # 
#' # model = build_classifier(X, Y, train.size=0.5, quantile.percentage=1, 
#' #		min.points=10, gamma.length=15, cost=10000)
#' # result = apply_classifier(model, X)
#' # points(X, col=as.numeric(result$classification.ensembled)+2, pch=20, cex=1.5)
#' # 
#' # x = seq(min(X), max(X), length=100)
#' # z = outer(x, x, function(x,y) { 
#' #	apply_classifier(model, as.matrix(cbind(x,y)))$classification.ensembled } )
#' # filled.contour(x,x,z)
#' # 
#' # x = seq(min(X), max(X), length=100)
#' # z = outer(x, x, function(x,y) { 
#' #	apply_classifier(model, as.matrix(cbind(x,y)), 
#' #		only.best.classifiers=TRUE)$classification.ensembled } )
#' # locator(1)
#' # filled.contour(x,x,z)
#' @export
build_classifier <- function(X, Y, train.size=0.7, quantile.percentage=1, min.points=3, gamma.length=50, cost=10000, weights=c(0.25,0.75), best.stdev.purity=0) {

	if (!is.matrix(X)) {
		return ("X must be a matrix.")
	}

	if (!is.vector(Y) || !is.numeric(Y)) {
		return ("Y must be a numerical vector.")
	}

	if (nrow(X) != length(Y)) {
		return ("The number of elements in X and Y must be equal.")
	}

	if (min.points <= 0 || min.points >= nrow(X)) {
		return ("min.points must be at least 1 and smaller than the number of examples under classification.")
	}

	# Selecting training examples by id
	ids = sample(1:nrow(X), size=floor(train.size*nrow(X)))

	# Computing the equivalence relations by using open balls
	erel = equivalence_relation(X[ids,], Y[ids], quantile.percentage=quantile.percentage) #, epsilon=0.1)

	# Training set
	X.train = erel$X
	Y.train = erel$Y
	M = erel$relations

	# Initializing variable sets to contain class-homogeneous region ids
	classifiers = list()
	in.set.sizes = c()
	overfitting.level.nSV = c()
	class.info = c()

	# Inducing a classifier per class-homogeneous region
	dists = as.matrix(stats::dist(X.train))
	counter = 1
	for (i in 1:length(M)) {
		# Setting class labels
		inset = as.vector(M[[i]]$i)
		if (length(inset) > min.points) {

			min.dist = min(setdiff(dists[inset, inset],0))
			max.dist = max(dists[inset, inset])

			cat("Building up classifier ", counter, ". Using gamma in [", min.dist, ", ", max.dist, "].........")
			labels = rep(-1, nrow(X.train))
			labels[inset] = +1
			class.info = c(class.info, unique(Y.train[inset]))
			in.set.sizes = c(in.set.sizes, length(inset))

			# Training the classification model
			tc = e1071::tune.control(sampling = "cross", cross = 10, best.model = TRUE)
			wts = 100 / table(labels)

			# Inducing the classifier based on 10-fold cross validation
			classifiers[[counter]] = e1071::best.svm(x=X.train, tunecontrol = tc, y=as.factor(labels), 
						       gamma=seq(min.dist, max.dist, length=gamma.length), cost=cost, class.weights=wts)
			cat("DONE.\n")
			overfitting.level.nSV = c(overfitting.level.nSV, classifiers[[counter]]$tot.nSV/in.set.sizes[counter])
			counter = counter + 1
		}
	}

	# Starting the prunning stage
	X.test = X[-ids,]
	Y.test = Y[-ids]

	purity = c()
	for (i in 1:length(classifiers)) {
		Y.test.obs = stats::predict(classifiers[[i]], X.test)
		pos.id = which(Y.test.obs == +1)
		purity = c(purity, NMF::purity(as.factor(Y.test.obs[pos.id]), Y.test[pos.id]))
	}

	best.classifiers = which(purity >= mean(purity) + best.stdev.purity * stats::sd(purity))

	cat("Finishing.\n")
	ret = list()
	ret$X.train = X.train
	ret$Y.train = Y.train
	ret$best.classifiers = best.classifiers
	ret$classifiers = classifiers
	ret$class.info = class.info
	ret$in.set.sizes = in.set.sizes
	ret$overfitting.level.nSV = overfitting.level.nSV
	# Ranking the classifiers according to the number of points in the positive class divided by the
	# number of support vectors in attempt to measure the overfitting. As we use 1-overfitting, this
	# means the greater this is, the more the classifier generalizes
	ret$ranking = cbind(1-overfitting.level.nSV, purity)%*%weights # The greater it is, the better it is!
	ret$purity = purity

	return (ret)
}

