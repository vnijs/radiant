################################################################
# regular functions used in damshiny app 
################################################################

varnames <- function() {
	if(is.null(input$datasets)) return()

	dat <- getdata()
	colnames <- names(dat)
	names(colnames) <- paste(colnames, " {", sapply(dat,class), "}", sep = "")
	colnames
}

changedata <- function(addCol = NULL, addColName = "") {
	# function that changes data as needed
	if(is.null(addCol) || addColName == "") return()
  # We don't want to take a reactive dependency on anything
  isolate(
  	values[[input$datasets]][[addColName]] <- addCol
  )
}

getdata <- function(dataset = input$datasets) {
  values[[dataset]]
}	

loadUserData <- function(uFile) {

	# filename <- uFile$name
	ext <- file_ext(uFile)
	objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(uFile))
	ext <- tolower(ext)

	if(ext == 'rda' || ext == 'rdata') {
		# objname will hold the name of the object inside the R datafile
	  objname <- robjname <- load(uFile)
		values[[robjname]] <- get(robjname)
	}

	if(datasets[1] == 'choosefile') {
		datasets <<- c(objname)
	} else {
		datasets <<- unique(c(objname,datasets))
	}

	if(ext == 'sav') {
		values[[objname]] <- read.sav(uFile)
	} else if(ext == 'dta') {
		values[[objname]] <- read.dta(uFile)
	} else if(ext == 'csv') {
		values[[objname]] <- read.csv(uFile)
	}
}

loadPackData <- function(pFile) {

	robjname <- data(list = pFile)
	dat <- get(robjname)
	if(!is.data.frame(dat)) return("Not a data.frame. Choose another dataset")
	if(pFile != robjname) return("R-object not found. Please choose another dataset")

	values[[robjname]] <- dat

	if(datasets[1] == 'choosefile') {
		datasets <<- c(robjname)
	} else {
		datasets <<- unique(c(robjname,datasets))
	}

}      

summary.dataview <- plot.dataview <- extra.dataview <- function(state) {
	return()
}