# avoid breaks in R-output print, don't show error messages in Rstudio
# options(width = 200, show.error.messages = FALSE, warn = -1)
options(width = 200)

last_loaded_data_files <- list('localfile' = "", 'packagefile' = "")

# loading list of data.frame in the car package
listPackageData <- function(pack) {
	lib <- data(package = pack)$results
	libnames <- c("Choose a file", paste(lib[,'Package'],lib[,'Item'], sep = '-'))
	lib <- c('',lib[,'Item'])
	names(lib) <- libnames
	as.list(lib)
}

# pdata_sets <- c('')
pdata_sets <- listPackageData('car')
# pdata_sets <- listPackageData('RadyMenu')

