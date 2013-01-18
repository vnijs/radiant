# avoid breaks in R-output print, don't show error messages in Rstudio
# options(width = 200, show.error.messages = FALSE, warn = -1)
# options(width = 200, shiny.trace=TRUE, error = browser)
options(width = 150, shiny.trace=TRUE)
# options(width = 150)

lastLoadedData <- list('packData' = "")

# loading list of data.frame in the car package
listPackData <- function(packs) {
	libnames <- c("Choose a file")
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

# n <- 200000
# n.var <- 100
# simdat <- data.frame(matrix(rnorm(n*n.var),nrow = n, ncol <- n.var))
# save(simdat,file = "data/simdat.rda")
