#' Apply a classifier induced with function build_classifier
#'
#' This function applies the set of SVM classifiers to perform the supervised learning task 
#' based on the topological data analysis
#' @param model model built using function build_classifier
#' @param X matrix defining the input space of your test set
#' @param only.best.classifiers if TRUE, only the most performing classification functions will be considered
#' @return prediction results
#' @keywords complexity analysis of the shattering coefficient for some dataset
#' @section References: de Mello, R.F. (2019) "On the Shattering Coefficient of Supervised Learning Algorithms" arXiv:<https://arxiv.org/abs/1911.05461>
#' @section References: de Mello, R.F., Ponti, M.A. (2018, ISBN: 978-3319949888) "Machine Learning: A Practical Approach on the Statistical Learning Theory"
#'
#' @export
apply_classifier <- function(model, X, only.best.classifiers=FALSE) {

	if (!is.matrix(X)) {
		return ("X must be a matrix.")
	}

	Preds = matrix(NA, nrow=nrow(X), ncol=length(model$classifiers))
	Classif = matrix(NA, nrow=nrow(X), ncol=length(model$classifiers))

	ids = sort.list(model$ranking, decreasing=TRUE)
	if (only.best.classifiers) {
		ids = model$best.classifiers
	}

	# Testing the classifiers according to their relative quality ranking
	for (i in ids) {
		preds = stats::predict(model$classifiers[[i]], X)
		preds.num = as.numeric(as.character(preds))
		pos.ids = which(preds.num >= 0)
		neg.ids = which(preds.num < 0)
		clf = rep(NA, length(preds.num))
		for (j in 1:length(preds.num)) {
			if (preds.num[j] >= 0) { # positive
				clf[j] = model$class.info[i]
			} else { # negative
				clf[j] = setdiff(model$Y.train, model$class.info[i])
			}
		}
		Preds[,i] = preds.num
		Classif[,i] = clf
	}

	classification.ensembled = c()
	for (i in 1:nrow(Classif)) {
		pattern.classification = Classif[i,]
		votes = table(pattern.classification[!is.na(pattern.classification)])
		classification.ensembled = c(classification.ensembled, as.numeric(names(votes)[which.max(votes)]))
	}

	ret = list()
	ret$classes = unique(model$Y.train)
	ret$classifier.weight = model$ranking
	#ret$prediction = Preds
	ret$classification.votes = Classif
	ret$classification.ensembled = classification.ensembled

	return (ret)
}
