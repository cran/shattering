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

	### FIXME: We could resample t times for each sample size when computing the Shattering coefficient
	### FIXME: We could paralelize this function
	### FIXME: We could save the tex messages into a text file
	oldpar <- graphics::par(no.readonly = TRUE)
	on.exit(graphics::par(oldpar))

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

	ret = list()
	ret$number.hyperplanes = Hyperplanes

	#cat("Generating the PDF report file.\n")
	######################################################################################################
	# Generating the PDF report file
	######################################################################################################
	## PAGE 1
	conn = base::file(base::paste(directory, "/", file, ".Rmd", sep=""), "w")
	#base::writeLines("---\n", conn)
	#base::writeLines("fontsize: 11pt\n", conn)
	#base::writeLines("geometry: margin=0.5in: 11pt\n", conn)
	#base::writeLines("---\n", conn)
	base::writeLines("# Package Shattering\n", conn)
	base::writeLines("This document reports an analysis on the shattering coefficient for your supervised dataset. This is helpful in terms of understanding the complexity of your data, the number of hyperplanes required to perform the classification task, and the minimal training sample size to ensure proper learning bounds (i.e. to ensure your model will perform similarly on unseen examples).\n", conn)
	base::writeLines("**Available at** [\\textcolor{blue}{https://cran.r-project.org/web/packages/shattering}](https://cran.r-project.org/web/packages/shattering)\n", conn)
	base::writeLines("**Maintained by** Rodrigo Fernandes de Mello <<mellorf@gmail.com>>\n", conn)
	base::writeLines("**Cite this report** [\\textcolor{blue}{https://arxiv.org/abs/1911.05461}](https://arxiv.org/abs/1911.05461)\n\n", conn)
	base::writeLines("\\newpage ## Original dataset\n", conn)
	base::writeLines(base::paste("This dataset contains ", nrow(X), " rows and ", ncol(X), " columns and it is here assessed using the following user-defined parameters:\n", sep=""), conn)
	base::writeLines(base::paste("\\begin{itemize}\n", sep=""), conn)
	base::writeLines(base::paste("\\item length=", length, " sets up the number of points to plot illustrative bounds of the Shattering coefficient functions without any impact on the theoretical assessment provided in this document;\n", sep=""), conn)
	base::writeLines(base::paste("\\item quantile.percentage=", quantile.percentage, " sets up the number of points from class overlapping space regions to be considered in our assessment. In case of doubt, we suggest you to set it up as $1$ so that it will consider all data inputs. However, this will take (eventually) too long to process your dataset and produce this report.\n", sep=""), conn)
	base::writeLines(base::paste("\\end{itemize}\n", sep=""), conn)

	grDevices::jpeg(filename = base::paste(directory, "/dataset.jpg", sep=""), width = 480, height = 480, units = "px", pointsize = 12, quality = 100, bg = "white")
	graphics::plot(X, col=Y, main="Original dataset")
	grDevices::dev.off()
	base::writeLines(base::paste("![Original dataset](", directory, "/dataset.jpg){width=75%}\n", sep=""), conn)

	## PAGE 2
	base::writeLines("\\newpage ## Number of homogeneous-class regions\n", conn)
	base::writeLines(base::paste("[\\textcolor{blue}{Our approach}](https://arxiv.org/pdf/1911.05461.pdf) estimates the number of homogeneous-class examples at space neighborhoods before computing the minimum number of hyperplanes to separate every point from any other using the theoretical result by [\\textcolor{blue}{Har-Peled and Jones}](https://arxiv.org/abs/1706.02004) which finds the separability of some dataset as the sample size increases. We then use such growth function of homogeneous-class space regions to:\n", sep=""), conn)
	base::writeLines(base::paste("\\begin{itemize}\n", sep=""), conn)
	base::writeLines(base::paste("\\item understand how the class overlapping or class mixing happens as the sample size increases. For instance, if you have a linear slope equals to $0.1$, it means that $10$\\% of examples appear in such overlapping region so that an average accuracy of $0.9$ or $90$\\% is expected;\n", sep=""), conn)
	base::writeLines(base::paste("\\item estimate the number of hyperplanes required to provide the separability of this dataset. This term separability is used by Har-Peled and Jones to determine the number of hyperplanes necessary to separate each input space element from any other but, in our case, this is related to the separation of homogeneous-class regions.\n", sep=""), conn)
	base::writeLines(base::paste("\\end{itemize}\n", sep=""), conn)
	base::writeLines(base::paste("The number of hyperplanes $h(n)$ required to shatter the input space of this dataset is given by the following equation:\n", sep=""), conn)
	base::writeLines(base::paste("$$h(n) = ", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[2])), " \\times n + ", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[1])), ",$$\n", sep=""), conn)
	base::writeLines(base::paste("as the sample size $n$ increases.\n", sep=""), conn)


	grDevices::jpeg(filename = base::paste(directory, "/hyperplanes.jpg", sep=""), width = 480, height = 480, units = "px", pointsize = 12, quality = 100, bg = "white")
	graphics::plot(Hyperplanes$estimation[,1:2], main="Number of Homogeneous Regions (y axis)\nversus the Original dataset size (x axis)", xlab="Sample size", ylab="Reduced sample size", t="l")
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
	base::writeLines(base::paste("$$\\resizebox{\\textwidth}{!}{$\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n}) = \\alpha (", h_n, ")^\\frac{2}{", ncol(X)+1, "} \\log \\log{(", h_n, ")} / \\log{(", h_n, ")}.$}$$\n", sep=""), conn)
	base::writeLines(base::paste("While the upper bound is given by:\n", sep=""), conn)
	base::writeLines(base::paste("$$O(d n^\\frac{2}{d+1}) = \\beta n^\\frac{2}{d+1} = \\beta h(n)^\\frac{2}{d+1}$$\n", sep=""), conn)
	base::writeLines(base::paste("$$O(d n^\\frac{2}{d+1}) = \\beta (", h_n, ")^\\frac{2}{", ncol(X)+1, "},$$\n", sep=""), conn)
	base::writeLines(base::paste("for constants $\\alpha, \\beta > 0$.\n", sep=""), conn)
	base::writeLines(base::paste("**Observation:** If the maximal number of hyperplanes found after using $O(h(n)^\\frac{2}{d+1})$ is used by your supervised learning algorithm, you will have a great probability of overfitting this dataset. Therefore, we always suggest you to set less hyperplanes in order to avoid shattering (or dividing) this sample in all possible ways according to its homogeneous-class regions.\n", sep=""), conn)

	## PAGE 4
	base::writeLines("\\newpage ## Shattering coefficient estimation\n", conn)
	base::writeLines("From the lower and the upper bounds of the number of hyperplanes, we obtain the number of space regions (half spaces) produced by those hyperplanes as the sample size $n$ increases, in form:\n", conn)
	base::writeLines(base::paste("$$2^{\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n})} = 2^{\\alpha h(n)^\\frac{2}{", ncol(X)+1, "} \\log \\log{h(n)} / \\log{h(n)}}$$\n", sep=""), conn)
	lower_h_n = base::paste("2^Omega", sep="")

	full_lower_h_n = base::paste("2^(1 * (", ncol(X), "*", t_h_n, ")^(2/(", ncol(X)+1, ")) * Ln(Ln(", t_h_n, ")) / Ln(", t_h_n,"))", sep="")
	full_upper_h_n = base::paste("2^(1 * (", ncol(X), "*", t_h_n, ")^(2/(", ncol(X)+1, ")))", sep="") 

	base::writeLines(base::paste("$$2^{O(d n^\\frac{2}{d+1})} = 2^{\\beta h(n)^\\frac{2}{", ncol(X)+1, "}}$$\n", sep=""), conn)
	upper_h_n = base::paste("2^O", sep="")
	base::writeLines("given each hyperplane divides the input space into two halves.\n", conn)
	base::writeLines("As next step, we compute the Shattering coefficient function using the following inequality, proven in [\\textcolor{blue}{our paper}](https://arxiv.org/pdf/1911.05461.pdf):\n", conn)
	base::writeLines("$$\\mathcal{N}(\\mathcal{F},2n) \\leq 1 + \\sum_{c_1=1}^{2^m} \\sum_{c_2=1}^{2^m-c_1} \\ldots \\sum_{c_{C-1}=1}^{2^m-c_1-c_2\\ldots-c_{C-2}} \\binom{2^m}{c_1} \\times \\binom{2^m-c_1}{c_2}\\times \\ldots \\times \\binom{2^m-c_1-c_2\\ldots-c_{C-2}}{c_{C-1}}$$\n", conn)
	base::writeLines("From that, we define the lower and upper bounds for the Shattering coefficient function as follows:\n", conn)
	base::writeLines("$$1+\\sum_{c_1=1}^{2^{\\Omega}} \\sum_{c_2=1}^{2^\\Omega-c_1} \\ldots \\sum_{c_{C-1}=1}^{2^\\Omega-c_1-c_2\\ldots-c_{C-2}} \\binom{2^{\\Omega}}{c_1} \\times \\binom{2^{\\Omega}-c_1}{c_2}\\times \\ldots \\times \\binom{2^{\\Omega}-c_1-c_2\\ldots-c_{C-2}}{c_{C-1}}$$\n", conn)
	base::writeLines("$$\\leq \\mathcal{N}(\\mathcal{F},2n) \\leq$$\n", conn)
	base::writeLines("$$1+\\sum_{c_1=1}^{2^{O}} \\sum_{c_2=1}^{2^{O}-c_1} \\ldots \\sum_{c_{C-1}=1}^{2^{O}-c_1-c_2\\ldots-c_{C-2}} \\binom{2^{O}}{c_1} \\times \\binom{2^{O}-c_1}{c_2}\\times \\ldots \\times \\binom{2^{O}-c_1-c_2\\ldots-c_{C-2}}{c_{C-1}},$$", conn)
	base::writeLines(base::paste("considering $C$ classes in our problem ($C=", length(unique(Y)), "$ for this dataset) and using $\\Omega$ and $O$ for short.\n", sep=""), conn)

	###########################
	# Defining the lower bound
	###########################
	func = base::paste("FactorialSimplify(1+", sep="")
	ffunc = base::paste("FactorialSimplify(1+", sep="")
	t = base::paste("", sep="")
	ft = base::paste("", sep="")
	lower = base::paste(lower_h_n, sep="")
	flower = base::paste(full_lower_h_n, sep="")
	for (i in 1:(length(unique(Y))-1)) {
		# Sum terms
		func = base::paste(func,"Sum(c",i,",1,", lower, ",", sep="")
		ffunc = base::paste(ffunc,"Sum(c",i,",1,", flower, ",", sep="")

		# Internal term
		t = base::paste(t,"Bin(", lower, ", c",i,")", sep="")
		ft = base::paste(ft,"Bin(", flower, ", c",i,")", sep="")
		if (i < length(unique(Y))-1) {
			t = base::paste(t," * ", sep="")
			ft = base::paste(ft," * ", sep="")
		}
	
		#lower = base::paste(lower_h_n, "-Sum(i,c1,c", i, ",i)", sep="")
		#flower = base::paste(full_lower_h_n, "-Sum(i,c1,c", i, ",i)", sep="")
		lower = base::paste(lower, "-c", i, sep="")
		flower = base::paste(flower, "-c", i, sep="")
	}
	func = base::paste(func, t, pracma::strcat(rep(")", length(unique(Y)))), sep="")
	shattering_lower_func = base::paste(ffunc, ft, pracma::strcat(rep(")", length(unique(Y)))), sep="")
	shattering_lower = Ryacas::tex(Ryacas::yac_symbol(func))

	###########################
	# Defining the upper bound
	###########################
	func = base::paste("FactorialSimplify(1+", sep="")
	ffunc = base::paste("FactorialSimplify(1+", sep="")
	t = base::paste("", sep="")
	ft = base::paste("", sep="")
	upper = base::paste(upper_h_n, sep="")
	fupper = base::paste(full_upper_h_n, sep="")
	for (i in 1:(length(unique(Y))-1)) {
		# Sum terms
		func = base::paste(func,"Sum(c",i,",1,", upper, ",", sep="")
		ffunc = base::paste(ffunc,"Sum(c",i,",1,", fupper, ",", sep="")

		# Internal term
		t = base::paste(t,"Bin(", upper, ", c",i,")", sep="")
		ft = base::paste(ft,"Bin(", fupper, ", c",i,")", sep="")
		if (i < length(unique(Y))-1) {
			t = base::paste(t," * ", sep="")
			ft = base::paste(ft," * ", sep="")
		}
	
		#upper = base::paste(upper_h_n, "-Sum(i,c1,c", i, ",i)", sep="")
		#fupper = base::paste(full_upper_h_n, "-Sum(i,c1,c", i, ",i)", sep="")
		upper = base::paste(upper, "-c", i, sep="")
		fupper = base::paste(fupper, "-c", i, sep="")
	}
	func = base::paste(func, t, pracma::strcat(rep(")", length(unique(Y)))), sep="")
	shattering_upper_func = base::paste(ffunc, ft, pracma::strcat(rep(")", length(unique(Y)))), sep="")
	shattering_upper = Ryacas::tex(Ryacas::yac_symbol(func))

	## PAGE 5
	base::writeLines(base::paste("\\newpage ## Solving the lower and upper bounds for the Shattering coefficient\n", sep=""), conn)
	base::writeLines(base::paste("For this dataset, the lower bound for the Shattering coefficient function is:\n", sep=""), conn)
	base::writeLines(base::paste("$$\\resizebox{\\textwidth}{!}{$", shattering_lower, "$}.$$\n", sep=""), conn)

	base::writeLines(base::paste("While the upper bound for the Shattering coefficient function is:\n", sep=""), conn)
	base::writeLines(base::paste("$$\\resizebox{\\textwidth}{!}{$", shattering_upper, "$}.$$\n", sep=""), conn)
	base::writeLines(base::paste("Having $n > 0$, and $O$ and $\\Omega$ as previously defined.\n", sep=""), conn)

	## PAGE 6
	base::writeLines(base::paste("\\newpage ## Generalization bound\n", sep=""), conn)
	base::writeLines(base::paste("Now you can use the [\\textcolor{blue}{Generalization bound}](https://www.springer.com/gp/book/9780387987804) to understand the minimal training sample sizes to ensure learning guarantees according to the [\\textcolor{blue}{Statistical Learning Theory}](https://www.springer.com/gp/book/9783319949888):\n", sep=""), conn)
	base::writeLines(base::paste("$$R(f) \\leq R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left( \\log{2\\mathcal{N}(\\mathcal{F},2n)} - \\log{\\delta} \\right)},$$\n", sep=""), conn)
	base::writeLines(base::paste("for the (expected) risk $R(f)$, the empirical risk (or sample error) $R_\\text{emp}(f)$, provided some probility $\\delta$ (whose $1-\\delta$ corresponds to the confidence you wish to ensure for the [\\textcolor{blue}{uniform convergence over the space of admissible functions}](https://www.springer.com/gp/book/9783319949888) used by your supervised learning algorithm). Term $f$ corresponds to the classifier found by your algorithm after the learning stage and $\\mathcal{N}(\\mathcal{F},2n)$ is the Shattering coefficient function.\n", sep=""), conn)
	base::writeLines(base::paste("We suggest you to apply the lower and upper bounds for the Shattering coefficient function in this Generalization bound so you can study how the Risk (expected value of the loss function on the joint probability distribution forming your dataset) behaves, as follows:\n", sep=""), conn)
	base::writeLines(base::paste("$$\\resizebox{\\textwidth}{!}{$R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left\\{ \\log{\\left(2 ", shattering_lower, "\\right)} - \\log{\\delta} \\right\\}}$}$$\n", sep=""), conn)
	base::writeLines(base::paste("$$\\leq R(f) \\leq$$\n", sep=""), conn)
	base::writeLines(base::paste("$$\\resizebox{\\textwidth}{!}{$R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left\\{ \\log{\\left(2 ", shattering_upper, "\\right)} - \\log{\\delta} \\right\\}}$},$$\n", sep=""), conn)
	base::writeLines(base::paste("Having:\n", sep=""), conn)
	base::writeLines(base::paste("$$\\Omega = \\alpha h(n)^\\frac{2}{", ncol(X)+1, "} \\log \\log{h(n)} / \\log{h(n)}$$\n", sep=""), conn)
	base::writeLines(base::paste("$$O = \\beta h(n)^\\frac{2}{", ncol(X)+1, "}.$$\n", sep=""), conn)

#	## PAGE 7
#	# Newton(Sqrt(4/n*(Ln(2*n^2-2)-Ln(0.05)))-0.1,n,100,0.1);
#	base::writeLines(base::paste("\\newpage ## Some practical results\n", sep=""), conn)
#	base::writeLines(base::paste("This is an analysis that considers you set up the number of hyperplanes for your learning algorithm using the lower and upper bounds as proposed in [\\textcolor{blue}{our paper}](https://arxiv.org/pdf/1911.05461.pdf):\n"), conn)
#	base::writeLines(base::paste("$$\\Omega(n^\\frac{2}{d+1} \\log \\log{n} / \\log{n}) = \\alpha (", h_n, ")^\\frac{2}{", ncol(X)+1, "} \\log \\log{(", h_n, ")} / \\log{(", h_n, ")}.$$\n", sep=""), conn)
#	base::writeLines(base::paste("$$O(d n^\\frac{2}{d+1}) = \\beta (", h_n, ")^\\frac{2}{", ncol(X)+1, "},$$\n", sep=""), conn)
#	base::writeLines(base::paste("for constants $\\alpha, \\beta > 0$.\n", sep=""), conn)
#       	base::writeLines(base::paste("The training sample sizes necessary to ensure the probability bound $\\delta$:\n", sep=""), conn)
#	base::writeLines(base::paste("$$P(\\sup_{f \\in \\mathcal{F}} |R_\\text{emp}(f)-R(f)| > \\epsilon) \\leq \\delta$$\n", sep=""), conn)
#       	base::writeLines(base::paste("are illustrated in the following table:\n", sep=""), conn)
#	# Creating the table
#	base::writeLines(base::paste("\\begin{table}[h!] \\centering \\begin{tabular}{|c|c|c|}\n", sep=""), conn)
#	base::writeLines(base::paste("\\hline \\textbf{Parameters} & \\textbf{Lower} & \\textbf{Upper} \\\\ \\hline\\hline\n", sep=""), conn)
#
#	slf = Ryacas::yac_symbol(shattering_lower_func)
#	suf = Ryacas::yac_symbol(shattering_upper_func)
#	for (epsilon in c(0.01, 0.05, 0.1)) {
#		for (delta in c(0.01, 0.05, 0.1)) {
#			cat("oi\n")
#			print(base::paste("Newton(Sqrt(4/n*(Ln(2*(", slf, "))-Ln(", delta, ")))-", epsilon, ",n,100,0.1);", sep=""))
#			minimal = as.character(ceiling(as.numeric(Ryacas::tex(Ryacas::yac_symbol(base::paste("Newton(Sqrt(4/n*(Ln(2*(", slf, "))-Ln(", delta, ")))-", epsilon, ",n,100,0.1);", sep=""))))))
#			maximal = as.character(ceiling(as.numeric(Ryacas::tex(Ryacas::yac_symbol(base::paste("Newton(Sqrt(4/n*(Ln(2*(", suf, "))-Ln(", delta, ")))-", epsilon, ",n,100,0.1);", sep=""))))))
#			base::writeLines(base::paste("$\\delta=", delta, ", \\epsilon=", epsilon, "$ & $", minimal,"$ & $", maximal, "$ \\\\ \n", sep=""), conn)
#		}
#	}
#	base::writeLines(base::paste("\\hline\\end{tabular}\\end{table}\n", sep=""), conn)
#	base::writeLines(base::paste("Remember:\n", sep=""), conn)
#	base::writeLines(base::paste("\\begin{itemize}\n", sep=""), conn)
#	base::writeLines(base::paste("\\item term $\\epsilon$ corresponds to the acceptable divergence between the empirical (sample error) and the (expected) risk for the Joint Probability Distribution $P(X,Y)$;\n", sep=""), conn)
#	base::writeLines(base::paste("\\item term $\\delta$ corresponds to the acceptable probability to ensure the uniform convergence to your classifier;\n", sep=""), conn)
#	base::writeLines(base::paste("\\item column ``Lower'' corresponds to the training sample size to ensure the lower bound of the Shattering coefficient function;\n", sep=""), conn)
#	base::writeLines(base::paste("\\item column ``Upper'' corresponds to the training sample size to ensure the upper bound of the Shattering coefficient function.\n", sep=""), conn)
#	base::writeLines(base::paste("\\end{itemize}\n", sep=""), conn)
#
#
#	## PAGE 8
#	# Lower bound
#
#	## FIXME: Eu devo recalcular aqui o tamanho de amostra para o número de hiperplanos escolhido pelo usuário
#
#	# my.delta=0.05, my.epsilon=0.05
#	base::writeLines(base::paste("\\newpage ## Your learning scenario\n", sep=""), conn)
#	base::writeLines(base::paste("As you defined ", number.hyperplanes, " hyperplanes, your shattering coefficient function is:\n"), conn)
#	base::writeLines(base::paste("$$\\resizebox{\\textwidth}{!}{$", user_lower_shattering, "$}$$\n", sep=""), conn)
#	base::writeLines("$$\\leq \\mathcal{N}(\\mathcal{F},2n) \\leq$$\n", conn)
#	base::writeLines(base::paste("$$\\resizebox{\\textwidth}{!}{$", user_upper_shattering, ".$}$$\n", sep=""), conn)
#	base::writeLines(base::paste("This particular number of hyperplanes under the following parametrization:\n"), conn)
#	base::writeLines(base::paste("\\begin{itemize}\n", sep=""), conn)
#	base::writeLines(base::paste("\\item use $", my.epsilon*100, "$\\% (my.epsilon=$", my.epsilon, "$) to set up the acceptable divergence in between the empirical (in sample) and expected risks while classifying unseen examples;\n", sep=""), conn)
#	base::writeLines(base::paste("\\item use $", my.delta*100, "$\\% (my.delta=$", my.delta, "$) to set up the acceptable missing ratio of the uniform convergence of the empirical risk (in sample) to the expected risk. Its complement, i.e., $", 100-(my.delta*100), "$\\% defines the confidence level you will have in practical scenarios;\n", sep=""), conn)
#	base::writeLines(base::paste("\\end{itemize}\n", sep=""), conn)
#	# Defining Omega
#	sample.minimal = as.character(ceiling(as.numeric(Ryacas::tex(Ryacas::yac_symbol(base::paste("Newton(Sqrt(4/n*(Ln(2*(", user_lower_shattering_func, "))-Ln(", my.delta, ")))-", my.epsilon, ",n,100,0.1);", sep=""))))))
#	# Defining O
#	sample.maximal = as.character(ceiling(as.numeric(Ryacas::tex(Ryacas::yac_symbol(base::paste("Newton(Sqrt(4/n*(Ln(2*(", user_upper_shattering_func, "))-Ln(", my.delta, ")))-", my.epsilon, ",n,100,0.1);", sep=""))))))
#	base::writeLines(base::paste("Given this setting, the lower and upper Shattering coefficient functions define $", sample.minimal, "$ as the minimal and $", sample.maximal, "$ as the maximal required training sample sizes to ensure learning guarantees according to [\\textcolor{blue}{the empirical risk minimization principle}](https://www.springer.com/gp/book/9783319949888) and respecting the separability of all homogeneous-class space regions according to the theoretical results by [\\textcolor{blue}{Har-Peled and Jones}](https://arxiv.org/abs/1706.02004).\n", sep=""), conn)

	## PAGE 9
	base::writeLines("\\newpage ## R Code to generate the lower-bound Shattering coefficient function\n", conn)
	base::writeLines("The following R code employs all previous formulation to numerically estimate the lower bound for the Shattering coefficient function, given the exact computation is time intensive and leads to infinite values. We suggest the user to run the following code trying to increase as much as possible the value of variable n.end in attempt to estimate the shattering function for the greatest as possible sample size.\n", conn)
	base::writeLines(base::paste("\\begin{verbatim}\n", sep=""), conn)
	eqn = base::paste("h_n <- function(n) { return (", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[2])), " * n ", sep="")
	if (sign(as.numeric(ret$number.hyperplanes$regression$coefficients[1])) >= 0) {
		eqn = base::paste(eqn, "+", sep="")
	} else {
		eqn = base::paste(eqn, "-", sep="")
	}
	eqn = base::paste(eqn, sprintf("%.5f", abs(as.numeric(ret$number.hyperplanes$regression$coefficients[1]))), ") }\n", sep="")
	base::writeLines(eqn, conn)
	base::writeLines(base::paste("big_Omega_power <- function(alpha, n, power=", 2/(ncol(X)+1), ") {\n return (2^(alpha*h_n(n)^power * log(log(h_n(n)))/log(h_n(n))))\n }\n", sep=""), conn)

	###########################
	# Defining the lower bound
	###########################
	txt = base::paste("shattering_lower_bound <- function(alpha, n.start, n.end, n.by, power=", 2/(ncol(X)+1), ") {\n counter = 1\n shat = matrix(0, nrow=length(seq(n.start, n.end, by=n.by)), ncol=2)\n for (n in seq(n.start, n.end, by=n.by)) {\n  value = 1\n", sep="")
	prod = ""
	subtract = ""
	spaces = pracma::strcat(rep(" ", length(unique(Y))+2))
	end = ""
	for (i in 1:(length(unique(Y))-1)) {
		# Sum terms
		txt = base::paste(txt, pracma::strcat(rep(" ", i+1)), "for (c",i," in 1:round(max(c(big_Omega_power(alpha, n, power)", subtract, ", 1)))) {", sep="")

		# Internal term
		prod = base::paste(prod, "choose(max(c(big_Omega_power(alpha, n)", sep="")
		#if (i > 1) {
		#prod = base::paste(prod, "-sum(c1:c", i, ")", sep="")
		prod = base::paste(prod, subtract, sep="")
		subtract = base::paste(subtract, "-c", i, sep="")
		#}
		prod = base::paste(prod, ", c",i,")), c", i, ")", sep="")
		if (i < length(unique(Y))-1) {
			txt = base::paste(txt, "\n", sep="")
			prod = base::paste(prod, "*\n", spaces, " ", sep="")
		}

		#if (i == 1) {
		#	subtract = base::paste("-c", i, sep="")
		#} else {
		#	subtract = base::paste("-c", i-2, sep="")
		#}
		end = base::paste(end, pracma::strcat(rep(" ", length(unique(Y))-i+1)), "}\n", sep="")
	}
	base::writeLines(txt, conn)
	base::writeLines(base::paste(spaces, "value = value * ", prod, sep=""), conn)
	base::writeLines(base::paste(end, sep=""), conn)
	base::writeLines(base::paste("  shat[counter,] = c(n, value+1)", sep=""), conn)
	base::writeLines(base::paste("  counter = counter + 1", sep=""), conn)
	base::writeLines(base::paste(" }\n return (shat)\n}\n", sep=""), conn)

	base::writeLines(base::paste("model.shattering_lower_bound <- function(alpha=1, n.start=", nrow(X), ",\n\t\tn.end=", nrow(X)+100, ", n.by=1, plot=TRUE) {", sep=""), conn)
	base::writeLines(base::paste(" dataset = as.data.frame(shattering_lower_bound(alpha, n.start, n.end, n.by))", sep=""), conn)
	base::writeLines(base::paste(" if (plot) { plot(dataset) }", sep=""), conn)
	base::writeLines(base::paste(" model = lm(log(V2) ~ V1, data=dataset)", sep=""), conn)
	base::writeLines(base::paste(" eqn = paste(\"exp(\", as.numeric(model$coefficients[2]),\"*n + \",\n\t\t\tas.numeric(model$coefficients[1]), \")\", sep=\"\")", sep=""), conn)
	base::writeLines(base::paste(" ret = list()", sep=""), conn)
	base::writeLines(base::paste(" ret$model = model", sep=""), conn)
	base::writeLines(base::paste(" ret$eqn = eqn", sep=""), conn)
	base::writeLines(base::paste(" return (ret)", sep=""), conn)
	base::writeLines(base::paste("}", sep=""), conn)
	base::writeLines(base::paste("\\end{verbatim}\n", sep=""), conn)

	## PAGE 10
	base::writeLines("\\newpage ## R Code to generate the upper-bound Shattering coefficient function\n", conn)
	base::writeLines("The following R code employs all previous formulation to numerically estimate the upper bound for the Shattering coefficient function, given the exact computation is time intensive and leads to infinite values. We suggest the user to run the following code trying to increase as much as possible the value of variable n.end in attempt to estimate the shattering function for the greatest as possible sample size.\n", conn)
	base::writeLines(base::paste("\\begin{verbatim}\n", sep=""), conn)
	eqn = base::paste("h_n <- function(n) { return (", sprintf("%.5f", as.numeric(ret$number.hyperplanes$regression$coefficients[2])), " * n ", sep="")
	if (sign(as.numeric(ret$number.hyperplanes$regression$coefficients[1])) >= 0) {
		eqn = base::paste(eqn, "+", sep="")
	} else {
		eqn = base::paste(eqn, "-", sep="")
	}
	eqn = base::paste(eqn, sprintf("%.5f", abs(as.numeric(ret$number.hyperplanes$regression$coefficients[1]))), ") }\n", sep="")
	base::writeLines(eqn, conn)
	base::writeLines(base::paste("big_O_power <- function(beta, n, power=", 2/(ncol(X)+1), ") {\n return (2^(beta*h_n(n)^power))\n }\n", sep=""), conn)

	###########################
	# Defining the upper bound
	###########################
	txt = base::paste("shattering_upper_bound <- function(beta, n.start, n.end, n.by, power=", 2/(ncol(X)+1), ") {\n counter = 1\n shat = matrix(0, nrow=length(seq(n.start, n.end, by=n.by)), ncol=2)\n for (n in seq(n.start, n.end, by=n.by)) {\n  value = 1\n", sep="")
	prod = ""
	subtract = ""
	spaces = pracma::strcat(rep(" ", length(unique(Y))+2))
	end = ""
	for (i in 1:(length(unique(Y))-1)) {
		# Sum terms
		txt = base::paste(txt, pracma::strcat(rep(" ", i+1)), "for (c",i," in 1:round(max(c(big_O_power(beta, n, power)", subtract, ", 1)))) {", sep="")

		# Internal term
		prod = base::paste(prod, "choose(max(c(big_O_power(beta, n)", sep="")
		#if (i > 1) {
		#prod = base::paste(prod, "-sum(c1:c", i, ")", sep="")
		prod = base::paste(prod, subtract, sep="")
		subtract = base::paste(subtract, "-c", i, sep="")
		#}
		prod = base::paste(prod, ", c",i,")), c", i, ")", sep="")
		if (i < length(unique(Y))-1) {
			txt = base::paste(txt, "\n", sep="")
			prod = base::paste(prod, "*\n", spaces, " ", sep="")
		}

		#if (i == 1) {
		#	subtract = base::paste("-sum(c1:c", i, ")", sep="")
		#} else {
		#	subtract = base::paste("-sum(c1:c", i-2, ")", sep="")
		#}
		end = base::paste(end, pracma::strcat(rep(" ", length(unique(Y))-i+1)), "}\n", sep="")
	}
	base::writeLines(txt, conn)
	base::writeLines(base::paste(spaces, "value = value * ", prod, sep=""), conn)
	base::writeLines(base::paste(end, sep=""), conn)
	base::writeLines(base::paste("  shat[counter,] = c(n, value+1)", sep=""), conn)
	base::writeLines(base::paste("  counter = counter + 1", sep=""), conn)
	base::writeLines(base::paste(" }\n return (shat)\n}\n", sep=""), conn)

	base::writeLines(base::paste("model.shattering_upper_bound <- function(beta=1, n.start=", nrow(X), ",\n\t\tn.end=", nrow(X)+100, ", n.by=1, plot=TRUE) {", sep=""), conn)
	base::writeLines(base::paste(" dataset = as.data.frame(shattering_upper_bound(beta, n.start, n.end, n.by))", sep=""), conn)
	base::writeLines(base::paste(" if (plot) { plot(dataset) }", sep=""), conn)
	base::writeLines(base::paste(" model = lm(log(V2) ~ V1, data=dataset)", sep=""), conn)
	base::writeLines(base::paste(" eqn = paste(\"exp(\", as.numeric(model$coefficients[2]),\"*n + \",\n\t\t\tas.numeric(model$coefficients[1]), \")\", sep=\"\")", sep=""), conn)
	base::writeLines(base::paste(" ret = list()", sep=""), conn)
	base::writeLines(base::paste(" ret$model = model", sep=""), conn)
	base::writeLines(base::paste(" ret$eqn = eqn", sep=""), conn)
	base::writeLines(base::paste(" return (ret)", sep=""), conn)
	base::writeLines(base::paste("}", sep=""), conn)
	base::writeLines(base::paste("\\end{verbatim}\n", sep=""), conn)

	## PAGE 11
	base::writeLines("\\newpage ## Estimating the shattering coefficient functions\n", conn)
	base::writeLines("Next, you run the following code, after loading the two previous pages with R codes, increasing the value of n.end as much as possible in attempt to obtain a good estimation for both the lower and upper bounds for the shattering coefficient function:\n", conn)
	base::writeLines(base::paste("\\begin{verbatim}\n", sep=""), conn)
	base::writeLines(base::paste("lower_bound = model.shattering_lower_bound(n.end=250)", sep=""), conn)
	base::writeLines(base::paste("upper_bound = model.shattering_upper_bound(n.end=250)", sep=""), conn)
	base::writeLines(base::paste("\\end{verbatim}", sep=""), conn)
	base::writeLines("For the best-case scenario, i.e., the lower bound, you can now consider the following function in the probabilistic representation of the empirical risk minimization principle and in the generalization bound:\n", conn)
	base::writeLines(base::paste("\\begin{verbatim}\n", sep=""), conn)
	base::writeLines(base::paste("print(lower$model)", sep=""), conn)
	base::writeLines(base::paste("\\end{verbatim}", sep=""), conn)
	base::writeLines("And for the worst-case scenario, i.e., the upper bound, you can now consider the following function in the probabilistic representation of the empirical risk minimization principle and in the generalization bound:\n", conn)
	base::writeLines(base::paste("\\begin{verbatim}\n", sep=""), conn)
	base::writeLines(base::paste("print(upper$model)", sep=""), conn)
	base::writeLines(base::paste("\\end{verbatim}", sep=""), conn)

	## PAGE 12
	base::writeLines("\\newpage ## Employing the Empirical Risk Minimization Principle\n", conn)
	base::writeLines("The Empirical Risk Minimization Principle (ERMP) is a direct result of the Statistical Learning Theory, whose formulation is given by:\n", conn)
	base::writeLines(base::paste("$$P(\\sup_{f \\in \\mathcal{F}} |R_\\text{emp}(f)-R(f)| > \\epsilon) \\leq 2 \\mathcal{N}(\\mathcal{F},2n) \\exp{(-n \\epsilon^2 / 4)},$$\n", sep=""), conn)
	base::writeLines("in which $n$ represents the sample size, $\\epsilon$ corresponds to the acceptable divergence in between the empirical risk $R_\\text{emp}(f) \\in [0,1]$ and the (expected) risk $R(f) \\in [0,1]$, $\\mathcal{N}(\\mathcal{F},2n)$ is the Shattering coefficient function.\n", conn)
	base::writeLines(base::paste("After generating the lower and upper-bound Shattering coefficient models, we suggest you to take those two functions and apply as follows, assuming the lower-bound is $\\exp{(0.001n)}$ and the upper bound is $\\exp{(0.01n)}$:\n", sep=""), conn)
	base::writeLines(base::paste("$$P_\\text{lower}(\\sup_{f \\in \\mathcal{F}} |R_\\text{emp}(f)-R(f)| > \\epsilon) \\leq 2 \\exp{(0.001 n)} \\exp{(-n \\epsilon^2 / 4)},$$\n", sep=""), conn)
	base::writeLines(base::paste("$$P_\\text{upper}(\\sup_{f \\in \\mathcal{F}} |R_\\text{emp}(f)-R(f)| > \\epsilon) \\leq 2 \\exp{(0.01 n)} \\exp{(-n \\epsilon^2 / 4)},$$\n", sep=""), conn)
	base::writeLines(base::paste("Therefore, $\\epsilon_\\text{lower} \\approx \\sqrt{4 \\times 0.001}$, while $\\epsilon_\\text{upper} \\approx \\sqrt{4 \\times 0.01}$.\n", sep=""), conn)
	base::writeLines(base::paste("From this analysis, we have the minimal divergence $\\epsilon$ for which learning convergence is ensured. For example, given the empirical and the expected risks are in $[0,1]$, we wish to have a very small value for $\\epsilon$ approaching zero. If you wish to compare this dataset against another one devoted to solve the same classification task, $\\epsilon$ is a great factor for comparison. The smaller it is, the more separable (better) it is.\n", sep=""), conn)
	base::writeLines(base::paste("The minimal training sample size to ensure learning is then resultant of the value $\\epsilon + \\zeta$, for any $\\zeta > 0$, thus leading to:\n", sep=""), conn)
	base::writeLines(base::paste("$$P(\\sup_{f \\in \\mathcal{F}} |R_\\text{emp}(f)-R(f)| > \\epsilon) \\leq 2 \\exp{(-n \\zeta^2 / 4)},$$\n", sep=""), conn)
	base::writeLines(base::paste("which you can simply solve for $n$ and find the minimal sample size to train your model, assuming:\n", sep=""), conn)
	base::writeLines(base::paste("$$\\delta = 2 \\exp{(-n \\zeta^2 / 4)},$$\n", sep=""), conn)
	base::writeLines(base::paste("given $\\delta$ is the probability you wish to ensure. For example, if $\\delta=0.05$, you have $95\\%$ of confidence for your learning guarantees. Suppose you assume $\\delta=0.05$, and you decided to set $\\zeta = 0.001$, then you will have:\n", sep=""), conn)
	base::writeLines(base::paste("$$0.05 = 2 \\exp{(-n\\; 0.001^2 / 4)},$$\n", sep=""), conn)
	base::writeLines(base::paste("$$n \\approx 1.47555 \\times 10^7,$$\n", sep=""), conn)
	base::writeLines(base::paste("but maybe you decided to set $\\zeta = 0.01$, then you will need far less training examples:\n", sep=""), conn)
	base::writeLines(base::paste("$$n \\approx 147,555.$$\n", sep=""), conn)

	## PAGE 13
	base::writeLines("\\newpage ## Employing the Generalization Bound\n", conn)
	base::writeLines(base::paste("After generating the lower and upper-bound Shattering coefficient models, we suggest you to take those two functions and apply in the Generalization Bound:\n", sep=""), conn)
	base::writeLines(base::paste("$$R(f) \\leq R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left( \\log{(2\\mathcal{N}(\\mathcal{F},2n))} - \\log{\\delta} \\right)}.$$\n", sep=""), conn)
	base::writeLines(base::paste("Here we assume the lower-bound is $\\exp{(0.001n)}$ and the upper bound is $\\exp{(0.01n)}$, so that\n", sep=""), conn)
	base::writeLines(base::paste("$$R_\\text{lower}(f) \\leq R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left( \\log{(2 \\exp{(0.001n)})} - \\log{\\delta} \\right)},$$\n", sep=""), conn)
	base::writeLines(base::paste("$$R_\\text{upper}(f) \\leq R_\\text{emp}(f) + \\sqrt{\\frac{4}{n} \\left( \\log{(2 \\exp{(0.01n)})}- \\log{\\delta} \\right)},$$\n", sep=""), conn)
	base::writeLines(base::paste("Then, assuming $\\delta = 0.05$ and solving for $n \\to \\infty$, we find:\n", sep=""), conn)
	base::writeLines(base::paste("$$R_\\text{lower}(f) \\leq R_\\text{emp}(f) + 0.0632456$$\n", sep=""), conn)
	base::writeLines(base::paste("$$R_\\text{upper}(f) \\leq R_\\text{emp}(f) + 0.2$$\n", sep=""), conn)
	base::writeLines(base::paste("from which we conclude there is a perturbation around $6\\%$ when using the lower (best-case scenario) and $20\\%$ for the upper bound (worst-case scenario).\n", sep=""), conn)

	base::close(conn)

	rmarkdown::render(base::paste(directory, "/", file, ".Rmd", sep=""), rmarkdown::pdf_document())
	ret$filename = base::paste(directory, "/", file, ".pdf", sep="")

	return (ret)
}
