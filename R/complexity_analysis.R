#' Produce a PDF report analyzing the lower and upper shattering coefficient functions
#'
#' Full analysis on the lower and upper shattering coefficient functions for a given supervised dataset
#' @param X matrix defining the input space of your dataset
#' @param Y numerical vector defining the output space (labels/classes) of your dataset 
#' @param my.delta upper bound for the probability of the empirical risk minimization principle (in range (0,1))
#' @param my.epsilon acceptable divergence between the empirical and (expected) risks (in range (0,1))
#' @param directory directory used to generate the report for your dataset
#' @param file name of the PDF file to be generated (without extension)
#' @param length number of points to divide the sample while computing the shattering coefficient
#' @param quantile.percentage real number to define the quantile of distances to be considered (e.g. 0.1 means 10%)
#' @param epsilon a real threshold to be removed from distances in order to measure the open balls in the underlying topology
#' @return A list including the number of hyperplanes and the shattering coefficient function. A report is generated in the user-defined directory.
#' @keywords complexity analysis of the shattering coefficient for some dataset
#' @section References: de Mello, R.F. (2019) "On the Shattering Coefficient of Supervised Learning Algorithms" arXiv:<https://arxiv.org/abs/1911.05461>
#' @section References: de Mello, R.F., Ponti, M.A. (2018, ISBN: 978-3319949888) "Machine Learning: A Practical Approach on the Statistical Learning Theory"
#'
#' @examples
#'
#' # Analyzing the complexity of the shattering coefficients functions 
#' # 	(lower and upper bounds) for the Iris dataset
#' # require(datasets)
#' # complexity_analysis(X=as.matrix(iris[,1:4]), Y=as.numeric(iris[,5]))
#' @export
complexity_analysis <- function(X=NULL, Y=NULL, my.delta=0.05, my.epsilon=0.05, 
				directory=tempdir(), file="myreport", length=10, 
				quantile.percentage=0.5, epsilon=1e-7) {

	#oldpar <- graphics::par(no.readonly = TRUE)
	#on.exit(graphics::par(oldpar))

	if (is.null(X) || is.null(Y)) {
		return ("Parameters X and Y must be defined.")
	}

	if (!is.matrix(X)) {
		return ("Parameter X must be a matrix.")
	}

	if (!is.numeric(Y) || !is.vector(Y)) {
		return ("Parameter Y must be a numerical vector.")
	}

	if (my.delta <= 0 || my.delta >= 1) {
		return ("Parameter my.delta must be in range (0,1)")
	}

	if (my.epsilon <= 0 || my.epsilon >= 1) {
		return ("Parameter my.epsilon must be in range (0,1)")
	}

	# Building up the input and output sets
	X = as.data.frame(X)
	Y = as.numeric(Y)

	Hyperplanes = estimate_number_hyperplanes(as.matrix(X), Y, length, quantile.percentage, epsilon)
	Shattering = estimate_shattering(Hyperplanes$estimation)

	ret = list()
	ret$number.hyperplanes = Hyperplanes
	ret$shattering.coefficient = Shattering

	######################################################################################################
	# Generating the PDF report file
	######################################################################################################
	## PAGE 1
	conn = base::file(base::paste(directory, "/", file, ".Rmd", sep=""), "w")
	base::writeLines("# Package Shattering\n", conn)
	base::writeLines("This document reports an analysis on the shattering coefficient for your supervised dataset. This is helpful in terms of understanding the complexity of your data, the number of hyperplanes required to perform the classification task, and the minimal training sample size to ensure proper learning bounds (i.e. to ensure your model will perform similarly on unseen examples).\n", conn)
	base::writeLines("**Available at** [\\textcolor{blue}{https://cran.r-project.org/web/packages/shattering}](https://cran.r-project.org/web/packages/shattering)\n", conn)
	base::writeLines("**Maintained by** Rodrigo Fernandes de Mello <<mello@icmc.usp.br>>\n", conn)
	base::writeLines("**Cite this report** [\\textcolor{blue}{https://arxiv.org/abs/1911.05461}](https://arxiv.org/abs/1911.05461)\n\n", conn)
	base::writeLines("\\newpage ## Original dataset\n", conn)
	base::writeLines(base::paste("This dataset contains ", nrow(X), " rows and ", ncol(X), " columns and it is here assessed using the following user-defined parameters:\n", sep=""), conn)
	base::writeLines(base::paste("\\begin{itemize}\n", sep=""), conn)
	base::writeLines(base::paste("\\item length=", length, " sets up the number of points to plot illustrative bounds of the Shattering coefficient functions without any impact on the theoretical assessment provided in this document;\n", sep=""), conn)
	base::writeLines(base::paste("\\item quantile.percentage=", quantile.percentage, " sets up the number of points from class overlapping space regions to be considered in our assessment. In case of doubt, we suggest you to set it up as $1$ so that it will consider all data inputs. However, this will take (eventually) too long to process your dataset and produce this report.\n", sep=""), conn)
	base::writeLines(base::paste("\\end{itemize}\n", sep=""), conn)

	grDevices::jpeg(filename = base::paste(directory, "/dataset.jpg", sep=""), width = 480, height = 480, units = "px", pointsize = 12, quality = 100, bg = "white")
	base::plot(X, col=Y, main="Original dataset")
	grDevices::dev.off()
	base::writeLines(base::paste("![Original dataset](", directory, "/dataset.jpg){width=75%}\n", sep=""), conn)

	## PAGE 2
	base::writeLines("\\newpage ## Number of homogeneous-class regions\n", conn)
	base::writeLines(base::paste("[\\textcolor{blue}{Our approach}](https://arxiv.org/pdf/1911.05461.pdf) estimates the number of homogeneous-class examples at space neighborhoods before computing the minimum number of hyperplanes to separate every point from any other using the theoretical result by [\\textcolor{blue}{Har-Peled and Jones}](https://arxiv.org/abs/1706.02004) which finds the saparability of some dataset as the sample size increases. We then use such growth function of homogeneous-class space regions to:\n", sep=""), conn)
	base::writeLines(base::paste("\\begin{itemize}\n", sep=""), conn)
	base::writeLines(base::paste("\\item understand how the class overlapping or class mixing happens as the sample size increases. For instance, if you have a linear slope equals to $0.1$, it means that $10$\\% of examples appear in such overlapping region so that an average accuracy of $0.9$ or $90$\\% is expected;\n", sep=""), conn)
	base::writeLines(base::paste("\\item estimate the number of hyperplanes required to provide the separability of this dataset. This term separability is used by Har-Peled and Jones to determine the number of hyperplanes necessary to separate each input space element from any other but, in our case, this is related to the separation of homogeneous-class regions.\n", sep=""), conn)
	base::writeLines(base::paste("\\end{itemize}\n", sep=""), conn)
	base::writeLines(base::paste("The number of hyperplanes $h(n)$ required to shatter the input space of this dataset is given by the following equation:\n", sep=""), conn)
	base::writeLines(base::paste("$$h(n) = ", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[2])), " \\times n + ", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[1])), ",$$\n", sep=""), conn)
	base::writeLines(base::paste("as the sample size $n$ increases.\n", sep=""), conn)


	grDevices::jpeg(filename = base::paste(directory, "/hyperplanes.jpg", sep=""), width = 480, height = 480, units = "px", pointsize = 12, quality = 100, bg = "white")
	base::plot(Hyperplanes$estimation[,1:2], main="Number of Homogeneous Regions (y axis)\nversus the Original dataset size (x axis)", xlab="Sample size", ylab="Reduced sample size", t="l")
	regression.function = Hyperplanes$estimation[,1:2]
	regression.function[,2] = as.numeric(ret$number.hyperplanes$regression$coefficients[2])*regression.function[,1]+as.numeric(ret$number.hyperplanes$regression$coefficients[1])
	graphics::lines(regression.function, col=2)
	grDevices::dev.off()

	base::writeLines(base::paste("![Number of homogeneous regions in black and our linear regression in red.](", directory, "/hyperplanes.jpg){width=40%}\n", sep=""), conn)
	base::writeLines(base::paste("From this analisys, we conclude the number of homogeneous regions in your space increases in $", sprintf("%.3f", as.numeric(ret$number.hyperplanes$regression$coefficients[2])*100.0), "$\\% as your sample size grows.\n", sep=""), conn)

	## PAGE 3
	base::writeLines(base::paste("\\newpage ## Lower and Upper bounds for the Number of Hyperplanes", sep=""), conn)
	base::writeLines(base::paste("According to [\\textcolor{blue}{Har-Peled and Jones}](https://arxiv.org/abs/1706.02004), the lower and upper bounds for the number of hyperplanes required to separate every point in a given $d$-dimensional input space from any other is $\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n})$ and $O(d n^\\frac{2}{d+1})$, respectively.\n", sep=""), conn)
	base::writeLines(base::paste("Therefore, the lower bound is given by:\n", sep=""), conn)
	base::writeLines(base::paste("$$\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n}) = \\alpha n^\\frac{2}{d+1} \\log \\log{n} / \\log{n}$$\n", sep=""), conn)
	base::writeLines(base::paste("$$\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n}) = \\alpha h(n)^\\frac{2}{", ncol(X)+1, "} \\log \\log{h(n)} / \\log{h(n)}$$\n", sep=""), conn)
	t_h_n = base::paste(sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[2])), " * n + ", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[1])), sep="")
	h_n = base::paste(sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[2])), " \\times n + ", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[1])), sep="")
	base::writeLines(base::paste("$$\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n}) = \\alpha (", h_n, ")^\\frac{2}{", ncol(X)+1, "} \\log \\log{(", h_n, ")} / \\log{(", h_n, ")}.$$\n", sep=""), conn)
	base::writeLines(base::paste("While the upper bound is given by:\n", sep=""), conn)
	base::writeLines(base::paste("$$O(d n^\\frac{2}{d+1}) = \\beta n^\\frac{2}{d+1} = \\beta h(n)^\\frac{2}{d+1}$$\n", sep=""), conn)
	base::writeLines(base::paste("$$O(d n^\\frac{2}{d+1}) = \\beta (", h_n, ")^\\frac{2}{", ncol(X)+1, "},$$\n", sep=""), conn)
	base::writeLines(base::paste("for constants $\\alpha, \\beta > 0$.\n", sep=""), conn)
	base::writeLines(base::paste("**Observation:** If the maximal number of hyperplanes found after using $h(n)$ is used by your supervised learning algorithm, you will have a great probability of overfitting this dataset. Therefore, we always suggest you to set less hyperplanes in order to avoid shattering (or dividing) this sample in all possible ways according to its homogeneous-class regions. For instance, knowing this dataset has $", nrow(X), "$ examples, and given $h(n)$ for $n=", nrow(X), "$, we find:\n", sep=""), conn)
	base::writeLines(base::paste("$$", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[2])), " \\times ", nrow(X), " + ", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[1])), " = ", 
		    ceiling(ncol(X) * (as.numeric(ret$number.hyperplanes$regression$coefficients[2]) * nrow(X) + as.numeric(ret$number.hyperplanes$regression$coefficients[1]))^(2/(ncol(X)+1))), "$$", sep=""), conn)
	base::writeLines(base::paste("as such maximal number of hyperplanes which we suggest to be avoided. Use at least one hyperplane less because that will bring some reduction in the chance of obtaining an exponential number of distinct classifications.\n", sep=""), conn)

	## PAGE 4
	base::writeLines("\\newpage ## Shattering coefficient estimation\n", conn)
	base::writeLines("From the lower and the upper bounds of the number of hyperplanes, we obtain the number of space regions (half spaces) produced by those hyperplanes as the sample size $n$ increases, in form:\n", conn)
	base::writeLines(base::paste("$$2^{\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n})} = 2^{\\alpha h(n)^\\frac{2}{", ncol(X)+1, "} \\log \\log{h(n)} / \\log{h(n)}}$$\n", sep=""), conn)
	lower_h_n = base::paste("2^Omega", sep="")

	full_lower_h_n = base::paste("2^(1 * (", ncol(X), "*", t_h_n, ")^(2/(", ncol(X)+1, ")) * Ln(Ln(", t_h_n, ")) / Ln(", t_h_n,"))", sep="")
	full_upper_h_n = base::paste("2^(1 * (", ncol(X), "*", t_h_n, ")^(2/(", ncol(X)+1, ")))", sep="") # FIXME: falta a dimensão do espaço aqui

	base::writeLines(base::paste("$$2^{O(d n^\\frac{2}{d+1})} = 2^{\\beta h(n)^\\frac{2}{", ncol(X)+1, "}}$$\n", sep=""), conn)
	upper_h_n = base::paste("2^O", sep="")
	base::writeLines("given each hyperplane divides the input space into two halves.\n", conn)
	base::writeLines("As next step, we compute the Shattering coefficient function using the following inequality, proven in [\\textcolor{blue}{our paper}](https://arxiv.org/pdf/1911.05461.pdf):\n", conn)
	base::writeLines("$$\\mathcal{N}(\\mathcal{F},2n) \\leq \\sum_{c_1=1}^{2^m} \\sum_{c_2=1}^{2^m-\\sum_{i=c_1}^{c_1}{i}} \\ldots \\sum_{c_{C-1}=1}^{2^m-\\sum_{i=c_1}^{c_{C-2}}{i}} \\binom{2^m}{c_1} \\times \\binom{2^m-\\sum_{i=c_1}^{c_1}{i}}{c_2}\\times \\ldots \\times \\binom{2^m-\\sum_{i=c_1}^{c_{C-2}}{i}}{c_{C-1}}$$\n", conn)
	base::writeLines("From that, we define the lower and upper bounds for the Shattering coefficient function as follows:\n", conn)
	base::writeLines("$$\\sum_{c_1=1}^{2^{\\Omega}} \\sum_{c_2=1}^{2^{\\Omega}-\\sum_{i=c_1}^{c_1}{i}} \\ldots \\sum_{c_{C-1}=1}^{2^{\\Omega}-\\sum_{i=c_1}^{c_{C-2}}{i}} \\binom{2^{\\Omega}}{c_1} \\times \\binom{2^{\\Omega}-\\sum_{i=c_1}^{c_1}{i}}{c_2}\\times \\ldots \\times \\binom{2^{\\Omega}-\\sum_{i=c_1}^{c_{C-2}}{i}}{c_{C-1}}$$\n", conn)
	base::writeLines("$$\\leq \\mathcal{N}(\\mathcal{F},2n) \\leq$$\n", conn)
	base::writeLines("$$\\sum_{c_1=1}^{2^{O}} \\sum_{c_2=1}^{2^{O}-\\sum_{i=c_1}^{c_1}{i}} \\ldots \\sum_{c_{C-1}=1}^{2^{O}-\\sum_{i=c_1}^{c_{C-2}}{i}} \\binom{2^{O}}{c_1} \\times \\binom{2^{O}-\\sum_{i=c_1}^{c_1}{i}}{c_2}\\times \\ldots \\times \\binom{2^{O}-\\sum_{i=c_1}^{c_{C-2}}{i}}{c_{C-1}},$$", conn)
	base::writeLines(base::paste("considering $C$ classes in our problem ($C=", length(unique(Y)), "$ for this dataset) and using $\\Omega$ and $O$ for short.\n", sep=""), conn)

	###########################
	# Defining the lower bound
	###########################
	func = base::paste("FactorialSimplify(", sep="")
	t = base::paste("", sep="")
	lower = base::paste(lower_h_n, sep="")
	for (i in 1:(length(unique(Y))-1)) {
		# Sum terms
		func = base::paste(func,"Sum(c",i,",1,", lower, ",", sep="")

		# Internal term
		t = base::paste(t,"Bin(", lower, ", c",i,")", sep="")
		if (i < length(unique(Y))-1) {
			t = base::paste(t," * ", sep="")
		}
	
		lower = base::paste(lower, "-c", i, sep="")
	}
	func = base::paste(func, t, pracma::strcat(rep(")", length(unique(Y)))), sep="")
	shattering_lower = Ryacas::tex(Ryacas::yac_symbol(func))

	###########################
	# Defining the upper bound
	###########################
	func = base::paste("FactorialSimplify(", sep="")
	t = base::paste("", sep="")
	upper = base::paste(upper_h_n, sep="")
	for (i in 1:(length(unique(Y))-1)) {
		# Sum terms
		func = base::paste(func,"Sum(c",i,",1,", upper, ",", sep="")

		# Internal term
		t = base::paste(t,"Bin(", upper, ", c",i,")", sep="")
		if (i < length(unique(Y))-1) {
			t = base::paste(t," * ", sep="")
		}
	
		upper = base::paste(upper, "-c", i, sep="")
	}
	func = base::paste(func, t, pracma::strcat(rep(")", length(unique(Y)))), sep="")
	shattering_upper = Ryacas::tex(Ryacas::yac_symbol(func))

	## PAGE 5
	base::writeLines(base::paste("\\newpage ## Solving the lower and upper bounds for the Shattering coefficient\n", sep=""), conn)
	base::writeLines(base::paste("For this dataset, the lower bound for the Shattering coefficient function is:\n", sep=""), conn)
	base::writeLines(base::paste("$$", shattering_lower, ".$$\n", sep=""), conn)

	base::writeLines(base::paste("While the upper bound for the Shattering coefficient function is:\n", sep=""), conn)
	base::writeLines(base::paste("$$", shattering_upper, ".$$\n", sep=""), conn)
	base::writeLines(base::paste("Having $n > 0$, and $O$ and $\\Omega$ as previously defined.\n", sep=""), conn)
	#base::writeLines(base::paste("We suggest you to use Mathematica or Maxima (or any other similar software) to simplify even more those Shattering coefficient functions defining the lower and the upper bounds.\n", sep=""), conn)

	grDevices::jpeg(filename = base::paste(directory, "/shattering.jpg", sep=""), width = 480, height = 480, units = "px", pointsize = 12, quality = 100, bg = "white")
	base::plot(cbind(ret$number.hyperplanes$estimation[,"n"], ret$shattering.coefficient[,"lower.bound"]), main="Shattering along the original dataset size\n (lower bound in black and upper bound in red)", xlab="Sample size", ylab="Estimated Shattering value", t="l", ylim=range(ret$shattering.coefficient))
	graphics::lines(cbind(ret$number.hyperplanes$estimation[,"n"], ret$shattering.coefficient[,"upper.bound"]), col=2)
	grDevices::dev.off()
	base::writeLines(base::paste("![Shattering evaluated along the sample size](", directory, "/shattering.jpg){width=40%}\n", sep=""), conn)
	base::writeLines(base::paste("This chart (whenever the sample size allows, sometimes it grows too fast) is just an illustration of both lower and upper Shattering coefficient functions.\n", sep=""), conn)

	## PAGE 6
	base::writeLines(base::paste("\\newpage ## Generalization bound\n", sep=""), conn)
	base::writeLines(base::paste("Now you can use the [\\textcolor{blue}{Generalization bound}](https://www.springer.com/gp/book/9780387987804) to understand the minimal training sample sizes to ensure learning guarantees according to the [\\textcolor{blue}{Statistical Learning Theory}](https://www.springer.com/gp/book/9783319949888):\n", sep=""), conn)
	base::writeLines(base::paste("$$R(f) \\leq R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left( \\log{2\\mathcal{N}(\\mathcal{F},2n)} - \\log{\\delta} \\right)},$$\n", sep=""), conn)
	base::writeLines(base::paste("for the (expected) risk $R(f)$, the empirical risk (or sample error) $R_\\text{emp}(f)$, provided some probility $\\delta$ (whose $1-\\delta$ corresponds to the confidence you wish to ensure for the [\\textcolor{blue}{uniform convergece over the space of admissible functions}](https://www.springer.com/gp/book/9783319949888) used by your supervised learning algorithm). Term $f$ corresponds to the classifier found by your algorithm after the learning stage and $\\mathcal{N}(\\mathcal{F},2n)$ is the Shattering coefficient function.\n", sep=""), conn)
	base::writeLines(base::paste("We suggest you to apply the lower and upper bounds for the Shattering coefficient function in this Generalization bound so you can study how the Risk (expected value of the loss function on the joint probability distribution forming your dataset) behaves, as follows:\n", sep=""), conn)
	base::writeLines(base::paste("$$R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left\\{ \\log{\\left(2 ", shattering_lower, "\\right)} - \\log{\\delta} \\right\\}}$$\n", sep=""), conn)
	base::writeLines(base::paste("$$\\leq R(f) \\leq$$\n", sep=""), conn)
	base::writeLines(base::paste("$$R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left\\{ \\log{\\left(2 ", shattering_upper, "\\right)} - \\log{\\delta} \\right\\}},$$\n", sep=""), conn)
	base::writeLines(base::paste("Having:\n", sep=""), conn)
	base::writeLines(base::paste("$$\\Omega = \\alpha h(n)^\\frac{2}{", ncol(X)+1, "} \\log \\log{h(n)} / \\log{h(n)}$$\n", sep=""), conn)
	base::writeLines(base::paste("$$O = \\beta (", h_n, ")^\\frac{2}{", ncol(X)+1, "}.$$\n", sep=""), conn)

	## PAGE 7
	# Newton(Sqrt(4/n*(Ln(2*n^2-2)-Ln(0.05)))-0.1,n,100,0.1);
	base::writeLines(base::paste("\\newpage ## Some practical results\n", sep=""), conn)
	base::writeLines(base::paste("This is an analysis that considers you set up the number of hyperplanes for your learning algorithm using the lower and upper bounds as proposed in [\\textcolor{blue}{our paper}](https://arxiv.org/pdf/1911.05461.pdf):\n"), conn)
	base::writeLines(base::paste("$$\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n}) = \\alpha (", h_n, ")^\\frac{2}{", ncol(X)+1, "} \\log \\log{(", h_n, ")} / \\log{(", h_n, ")}.$$\n", sep=""), conn)
	base::writeLines(base::paste("$$O(d n^\\frac{2}{d+1}) = \\beta (", h_n, ")^\\frac{2}{", ncol(X)+1, "},$$\n", sep=""), conn)
	base::writeLines(base::paste("for constants $\\alpha, \\beta > 0$.\n", sep=""), conn)
       	base::writeLines(base::paste("The training sample sizes necessary to ensure the probability bound $\\delta$:\n", sep=""), conn)
	base::writeLines(base::paste("$$P(\\sup_{f \\in \\mathcal{F}} |R_\\text{emp}(f)-R(f)| > \\epsilon) \\leq \\delta$$\n", sep=""), conn)
       	base::writeLines(base::paste("are illustrated in the following table:\n", sep=""), conn)
	# Creating the table
	base::writeLines(base::paste("\\begin{table}[h!] \\centering \\begin{tabular}{|c|c|c|}\n", sep=""), conn)
	base::writeLines(base::paste("\\hline \\textbf{Parameters} & \\textbf{Lower} & \\textbf{Upper} \\\\ \\hline\\hline\n", sep=""), conn)
	for (epsilon in c(0.01, 0.05, 0.1)) {
		for (delta in c(0.01, 0.05, 0.1)) {
			minimal = as.character(ceiling(as.numeric(Ryacas::tex(Ryacas::yac_symbol(base::paste("Newton(Sqrt(4/n*(Ln(2*(", full_lower_h_n, "))-Ln(", delta, ")))-", epsilon, ",n,100,0.1);", sep=""))))))
			maximal = as.character(ceiling(as.numeric(Ryacas::tex(Ryacas::yac_symbol(base::paste("Newton(Sqrt(4/n*(Ln(2*(", full_upper_h_n, "))-Ln(", delta, ")))-", epsilon, ",n,100,0.1);", sep=""))))))
			base::writeLines(base::paste("$\\delta=", delta, ", \\epsilon=", epsilon, "$ & $", minimal,"$ & $", maximal, "$ \\\\ \n", sep=""), conn)
		}
	}
	base::writeLines(base::paste("\\hline\\end{tabular}\\end{table}\n", sep=""), conn)
	base::writeLines(base::paste("Remember:\n", sep=""), conn)
	base::writeLines(base::paste("\\begin{itemize}\n", sep=""), conn)
	base::writeLines(base::paste("\\item term $\\epsilon$ corresponds to the acceptable divergence between the empirical (sample error) and the (expected) risk for the Joint Probability Distribution $P(X,Y)$;\n", sep=""), conn)
	base::writeLines(base::paste("\\item term $\\delta$ corresponds to the acceptable probability to ensure the uniform convergence to your classifier;\n", sep=""), conn)
	base::writeLines(base::paste("\\item column ``Lower'' corresponds to the training sample size to ensure the lower bound of the Shattering coefficient function;\n", sep=""), conn)
	base::writeLines(base::paste("\\item column ``Upper'' corresponds to the training sample size to ensure the upper bound of the Shattering coefficient function.\n", sep=""), conn)
	base::writeLines(base::paste("\\end{itemize}\n", sep=""), conn)

	## PAGE 8
	# my.delta=0.05, my.epsilon=0.05
	base::writeLines(base::paste("\\newpage ## Practical suggestions for your learning scenario\n", sep=""), conn)
	base::writeLines(base::paste("According to your parametrization, you will:\n"), conn)
	base::writeLines(base::paste("\\begin{itemize}\n", sep=""), conn)
	base::writeLines(base::paste("\\item use $", my.epsilon*100, "$\\% (my.epsilon=$", my.epsilon, "$) to set up the acceptable divergence in between the empirical (in sample) and expected risks while classifying unseen examples;\n", sep=""), conn)
	base::writeLines(base::paste("\\item use $", my.delta*100, "$\\% (my.delta=$", my.delta, "$) to set up the acceptable missing ratio of the uniform convergence of the empirical risk (in sample) to the expected risk. Its complement, i.e., $", 100-(my.delta*100), "$\\% defines the confidence level you will have in practical scenarios;\n", sep=""), conn)
	base::writeLines(base::paste("\\end{itemize}\n", sep=""), conn)
	my.t_h_n = base::paste("(", t_h_n, ")", sep="")
	my.full_lower_h_n = base::paste("2^(1 * (", ncol(X), "*", my.t_h_n, ")^(2/(", ncol(X)+1, ")) * Ln(Ln(", my.t_h_n, ")) / Ln(", my.t_h_n,"))", sep="")
	my.full_upper_h_n = base::paste("2^(1 * (", ncol(X), "*", my.t_h_n, ")^(2/(", ncol(X)+1, ")))", sep="") 
	sample.minimal = as.character(ceiling(as.numeric(Ryacas::tex(Ryacas::yac_symbol(base::paste("Newton(Sqrt(4/n*(Ln(2*(", my.full_lower_h_n, "))-Ln(", my.delta, ")))-", my.epsilon, ",n,100,0.1);", sep=""))))))
	sample.maximal = as.character(ceiling(as.numeric(Ryacas::tex(Ryacas::yac_symbol(base::paste("Newton(Sqrt(4/n*(Ln(2*(", my.full_upper_h_n, "))-Ln(", my.delta, ")))-", my.epsilon, ",n,100,0.1);", sep=""))))))
	base::writeLines(base::paste("Given this setting, the lower and upper Shattering coefficient functions define $", sample.minimal, "$ as the minimal and $", sample.maximal, "$ as the maximal required training sample sizes to ensure learning guarantees according to [\\textcolor{blue}{the empirical risk minimization principle}](https://www.springer.com/gp/book/9783319949888) and respecting the separability of all homogeneous-class space regions according to the theoretical results by [\\textcolor{blue}{Har-Peled and Jones}](https://arxiv.org/abs/1706.02004).\n", sep=""), conn)

	base::close(conn)

	rmarkdown::render(base::paste(directory, "/", file, ".Rmd", sep=""), rmarkdown::pdf_document())
	ret$filename = base::paste(directory, "/", file, ".pdf", sep="")

	return (ret)
}