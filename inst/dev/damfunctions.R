################################################################
# all regular functions used in damshiny app 
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
  isolate({
    # rdat isn't the dataset, it's a reactive value that holds the dataset.
    # We use value() to read it, and value<-() to write to it.
    # rdat <- get(input$datasets, pos=datasetEnv)
    # dat <- value(rdat)
    # dat[,addColName] <- addCol
    # value(rdat) <- dat

    # dat <- values[[input$datasets]]
    # dat[,addColName] <- addCol
    # values[[input$datasets]] <- dat
    # dat <- values[[input$datasets]]
    # dat[,addColName] <- addCol
    values[[input$datasets]][[addColName]] <- addCol
  })
}

getdata <- function(dataset = input$datasets) {

  # First we get the reactive value from datasetEnv. Then we need to use the
  # value() function to actually retrieve the dataset.
	# dat <- value(get(dataset, pos=datasetEnv))
	# dat
  values[[dataset]]
}	

loadUserData <- function(uFile) {

	# filename <- uFile$name
	ext <- file_ext(uFile)
	objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(uFile))
	ext <- tolower(ext)

	if(ext == 'rda' || ext == 'rdata') {
		# objname will hold the name of the object inside the R datafile
	  # objname <- robjname <- load(uFile$datapath, envir = datasetEnv)
	  objname <- robjname <- load(uFile, envir = datasetEnv)
		assign(robjname, reactiveValues(get(objname, pos = datasetEnv)), pos = datasetEnv)
	}

	if(datasets[1] == 'choosefile') {
		datasets <<- c(objname)
	} else {
		datasets <<- unique(c(objname,datasets))
	}

	if(ext == 'sav') {
		# assign(objname, reactiveValues(read.sav(uFile$datapath)), pos = datasetEnv)
		assign(objname, reactiveValues(read.sav(uFile)), pos = datasetEnv)
	} else if(ext == 'dta') {
		# assign(objname, reactiveValues(read.dta(uFile$datapath)), pos = datasetEnv)
		assign(objname, reactiveValues(read.dta(uFile)), pos = datasetEnv)
	} else if(ext == 'csv') {
		# assign(objname, reactiveValues(read.csv(uFile$datapath)), pos = datasetEnv)
		assign(objname, reactiveValues(read.csv(uFile)), pos = datasetEnv)
	}
}

loadPackData <- function(pFile) {

	robjname <- data(list = pFile, envir = datasetEnv)

	if(datasets[1] == 'choosefile') {
		datasets <<- c(robjname)
	} else {
		datasets <<- unique(c(robjname,datasets))
	}

	assign(robjname, reactiveValues(get(robjname, pos = datasetEnv)), pos = datasetEnv)
}      

summary.dataview <- plot.dataview <- extra.dataview <- function(state) {
	return()
}