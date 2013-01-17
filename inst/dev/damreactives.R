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

	# Create a group of checkboxes and select them all by default
	checkboxGroupInput("columns", "Choose columns", choices  = as.list(cols), selected = names(cols))
})

output$datasets <- reactiveUI(function() {

	state <- as.list(input)
	fpath <- uploadfunc()

	# loading user data
	if(fpath != "") {
		loadUserData(fpath)
	} 

	# loading package data
	if(!is.null(state$packData) && state$packData != "") {
		if(state$packData != lastLoadedData$packData) {
			loadPackData(state$packData)
			lastLoadedData$packData <<- state$packData 
		}
	}

	# Drop-down selection of data set
	selectInput(inputId = "datasets", label = "Datasets:", choices = datasets, selected = datasets[1], multiple = FALSE)
})

output$packData <- reactiveUI(function() {

	# Drop-down selection of data sets from 'car' package
	selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
})

output$nrRows <- reactiveUI(function() {
	if(is.null(input$datasets)) return()
	dat <- getdata()

	# observations to show in data view
	nr <- nrow(dat)
	sliderInput("nrRows", "Rows to show (max 50):", min = 1, max = nr, value = min(15,nr), step = 1)
})

# variable selection
output$var1 <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()

	if(input$tool == 'compareMeans') {
		isFct <- sapply(getdata(), is.factor)
		vars <- vars[isFct]
	}

	selectInput(inputId = "var1", label = labels1[input$tool], choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection
output$var2 <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()
	selectInput(inputId = "var2", label = labels2[input$tool], choices = vars[-which(vars == input$var1)], selected = NULL, multiple = TRUE)
})

# variable selection in the datatabs views
output$varview1 <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()

	selectInput(inputId = "varview1", label = "X-variable", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection
output$varview2 <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()
	selectInput(inputId = "varview2", label = "Y-variable", choices = vars[-which(vars == input$varview1)], selected = NULL, multiple = TRUE)
})

output$varinterdep <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()
	selectInput(inputId = "varinterdep", label = "Variables", choices = vars, selected = NULL, multiple = TRUE)
})

# dropdown used to select the number of clusters to create
output$nrClus <- reactiveUI(function() {
	selectInput(inputId = "nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE)
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
	dat[max(1,nr-50):nr, input$columns, drop = FALSE] 
	# head(dat, input$nrRows)

	# idea: Add download button so data can be saved
	# example here https://github.com/smjenness/Shiny/blob/master/SIR/server.R
})

output$visualize <- reactivePlot(function() {
	if(is.null(input$datasets) || is.null(input$varview1)) return()
	if(input$datatabs != 'Visualize') return()

	dat <- getdata()

	x <- dat[,input$varview1]
	ifelse(is.factor(x), bw <- .1, bw <- diff(range(x)) / 12)

	if(is.null(input$varview2)) {
		p <- ggplot(dat, aes_string(x=input$varview1)) + geom_histogram(colour = 'black', fill = 'blue', binwidth = bw)
	} else {
		y <- dat[,input$varview2]
		ifelse(is.factor(y), jitt <- .1, jitt <- diff(range(y)) / 15)

		p <- ggplot(dat, aes_string(x=input$varview1, y=input$varview2)) + geom_point() + geom_jitter(position = position_jitter(width = bw, height = jitt)) + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
	}
	print(p)

	# idea: Add a download button so graphs can be saved
	# idea: Add download button so data can be saved
	# example here https://github.com/smjenness/Shiny/blob/master/SIR/server.R
})

# output$transform <- reactiveTable(function() {
output$transform <- reactivePrint(function() {
	if(is.null(input$datasets) || is.null(input$columns)) return()
	if(input$datatabs != 'Transform') return()

	# idea: use mutate to transformations on the data see links below
	# If will probably be easiest to have this be a text-box input field
	# that runs these. No need for an elaborate UI since it is basically
	# what they would otherwise do in excel. Make sure to add
	# some helptext with a bunch of examples of how the command would work.
	# http://www.inside-r.org/packages/cran/plyr/docs/mutate
	# https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/uZZT564y0i8
	# https://gist.github.com/4515551 

	cat("Command box and data view (in development)")

})

output$logwork <- reactivePrint(function() {
	if(is.null(input$datasets) || is.null(input$columns)) return()

	if(input$datatabs != 'Log') return()

	# idea: When a user presses a log-button the output on screen is saved to an rda file
	# ala the sesson data (.Radata). It would be like taking a snap-shot of the app-state
	# and then call the relevant parts from an Rmd file that gets-sourced. By default all snap
	# shots are shown in log but user can deseleted snap-shots as desired.
	# take another look at Jeff's teaching log. this could be a great starting point
	# ask Jeff about how to attribute code (and also Yihie) if you use some of their code
	# https://github.com/jeffreyhorner/TeachingLab

	cat("Work log (in development)")

})

################################################################
# Analysis reactives - functions have the same names as the 
# values for the toolChoices values # in global.R
# Calling a reactive several times is more efficient than 
# calling a regular function several times
################################################################

source('regression.R', local = TRUE)
source('cluster.R', local = TRUE)
source('edat.R', local = TRUE)

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