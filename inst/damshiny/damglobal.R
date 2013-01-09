# avoid breaks in R-output print, don't show error messages in Rstudio
# options(width = 200, show.error.messages = FALSE, warn = -1)
# options(width = 200, shiny.trace=TRUE, error = browser)
# options(width = 200, shiny.trace=TRUE, error = browser)
options(width = 200)

lastLoadedData <- list('userData' = "", 'packData' = "")

# loading list of data.frame in the car package
listPackData <- function(pack) {
	lib <- data(package = pack)$results
	libnames <- c("Choose a file", paste(lib[,'Package'],lib[,'Item'], sep = '-'))
	lib <- c('',lib[,'Item'])
	names(lib) <- libnames
	as.list(lib)
}

# packDataSets <- c('')
# packDataSets <- list("Choose a file" = "", "Data 1" = "morley", "Data 2" = "rock")
packDataSets <- listPackData('car')
# packDataSets <- listPackData('RadyMenu')
