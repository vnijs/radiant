################################################################
# all reactive functions used in damshiny app 
################################################################

uploadfunc <- reactive(function() {
  if(input$upload == 0) return("")
  fpath <- try(file.choose())
  if(is(fpath, 'try-error')) {
  	return("")
  } else {
  	return(fpath)
  }
})

output$columns <- reactiveUI(function() {
	cols <- varnames()
	if(is.null(cols)) return()

	selectInput("columns", "Select columns to show:", choices  = as.list(cols), selected = names(cols), multiple = TRUE)
	# get select2 lifecyle for this. Awesome. http://ivaynberg.github.com/select2/
})

output$datasets <- reactiveUI(function() {

	fpath <- uploadfunc()

	# loading user data
	if(fpath != "") {
		loadUserData(fpath)
	} 

	# loading package data
	if(input$yahooData != "" && input$yahooData != lastLoadedYahooData) {
		loadYahooData(input$yahooData)
		lastLoadedYahooData <<- input$yahooData
	}

	# Drop-down selection of data set
	selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
})

output$nrRows <- reactiveUI(function() {
	if(is.null(input$datasets)) return()
	dat <- getdata()

	# observations to show in data view
	nr <- nrow(dat)
	sliderInput("nrRows", "Number of days:", min = 1, max = nr, value = min(30,nr), step = 1)
})

# variable selection
output$var1 <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()

	selectInput(inputId = "var1", label = "Time-series:", choices = vars, selected = NULL, multiple = FALSE)
})

################################################################
# Data reactives - view, plot, transform data, and log your work
################################################################
output$dataviewer <- reactiveTable(function() {
	if(is.null(input$datasets) || is.null(input$columns)) return()

	dat <- getdata()

	# not sure why this is needed when files change ... but it is
	# without it you will get errors the invalid columns have been
	# selected
	if(!all(input$columns %in% colnames(dat))) return()


	# Show only the selected columns and no more than 50 rows
	# at a time
	nr <- input$nrRows
	nrRows <- nrow(dat)
	dat <- data.frame(dat[(nrRows-nr):nrRows, input$columns, drop = FALSE])

	# idea: Add download button so data can be saved
	# example here https://github.com/smjenness/Shiny/blob/master/SIR/server.R
})

output$visualize <- reactivePlot(function() {
	if(is.null(input$datasets)) return()
	if(input$datatabs != 'Visualize') return()

	dat <- getdata()
	if(input$log_data) dat <- log(dat)

	chartSeries(dat, name = input$datasets, type = input$chart_type, 
		subset = paste("last", input$nrRows, "days"), theme     = "white")

})

################################################################
# Analysis reactives - functions have the same names as the 
# values for the toolChoices values # in global.R
# Calling a reactive several times is more efficient than 
# calling a regular function several times
################################################################

source('garch.R', local = TRUE)

################################################################
# Output controls for the Summary, Plots, and Extra tabs
# The tabs are re-used for various tools. Depending on the tool
# selected by the user the appropropriate analaysis function 
# is called.
# Naming conventions: The reactive function to be put in the
# code block above must be of the same name as the tool
# in the tools drop-down. See global.R for the current list
# of tools (and tool-names) 
################################################################

# Generate output for the summary tab
output$summary <- reactivePrint(function() {
	if(is.null(input$datasets) || input$tool == 'dataview') return()

	# get the summary function for currenly selected tool and feed
	# it the output from one of the analysis reactives above
	# get-function structure is used because there may be a large
	# set of tools that will have the same output structure
	f <- get(paste("summary",input$tool,sep = '.'))
	result <- get(input$tool)()
	if(is.character(result)) {
		cat(result,"\n")
	} else {
		f(result)
	}
})

# Generate output for the plots tab
output$plots <- reactivePlot(function() {

	# plotting could be expensive so only done when tab is being viewed
	if(input$tool == 'dataview' || input$analysistabs != 'Plots') return()

	f <- get(paste("plot",input$tool,sep = '.'))
	result <- get(input$tool)()
	if(!is.character(result)) {
		f(result)
	} else {
		plot(x = 1, type = 'n', main="No variable selection made", axes = FALSE, xlab = "", ylab = "")
	}
}, width=600, height=600)

# Generate output for the extra tab
output$extra <- reactivePrint(function() {

	# if extra calculations are expensive
	# do only when tab is being viewed
	if(input$tool == 'dataview' || input$analysistabs != 'Extra') return()

	f <- get(paste("extra",input$tool,sep = '.'))
	result <- get(input$tool)()
	if(is.character(result)) {
		cat(result,"\n")
	} else {
		f(result)
	}
})