
require(datasets)
#res = complexity_analysis(X=as.matrix(iris[,1:4]), Y=as.numeric(iris[,5]), quantile.percentage=0.1)
res = estimate_number_hyperplanes(X=as.matrix(iris[,1:4]), Y=as.numeric(iris[,5]), length=5, quantile.percentage=0.1, epsilon=1e-7)

