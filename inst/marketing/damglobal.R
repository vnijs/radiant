# avoid breaks in R-output print, don't show error messages in Rstudio
options(width = 150, shiny.trace=TRUE)

# loading list of data.frame in the car package
listPackData <- function(packs) {
	libnames <- c('')
	lib <- c('')
	for(ipack in packs) {
		ilib <- data(package = ipack)$results
		libnames <- c(libnames, paste(ilib[,'Package'],ilib[,'Item'], sep = '-'))
		lib <- c(lib,ilib[,'Item'])
	}
	names(lib) <- libnames
	as.list(lib)
}

packDataSets <- listPackData(c('car','datasets'))

# Simulate a big data-file
# n <- 200000
# n.var <- 100
# bigsimdat <- data.frame(matrix(rnorm(n*n.var),nrow = n, ncol <- n.var))
# save(simdat,file = "data/bigsimdat.rda")
