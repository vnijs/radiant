################################################################
# functions used in radyant
################################################################

varnames <- function() {
	if(is.null(input$datasets)) return()

	dat <- getdata()
	colnames <- names(dat)
	names(colnames) <- colnames
	### names(colnames) <- paste(colnames, " {", sapply(dat,class), "}", sep = "")
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

	ext <- file_ext(uFile)
	objname <- robjname <- sub(paste(".",ext,sep = ""),"",basename(uFile))
	ext <- tolower(ext)

	if(ext == 'rda' || ext == 'rdata') {
		# objname will hold the name of the object inside the R datafile
	  objname <- robjname <- load(uFile)
		values[[robjname]] <- get(robjname)
	}

	if(datasets[1] == '') {
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

loadYahooData <- function(symbol) {

	# if (symbol == "Choose a symbol") return()
	if (symbol == "") return()
	if (!exists(symbol)) getSymbols(symbol)

	values[[symbol]] <- get(symbol)
	datasets <<- unique(c(symbol,datasets))
	
}

#################################################
# reactive functions used in radyant
#################################################

uploadfunc <- reactive({
  if(input$upload == 0) return("")
  fpath <- try(file.choose())
  if(is(fpath, 'try-error')) {
  	return("")
  } else {
  	return(fpath)
  }
})

output$columns <- renderUI({
	cols <- varnames()

	selectInput("columns", "Select columns to show:", choices  = as.list(cols), selected = names(cols), multiple = TRUE)
})

output$datasets <- renderUI({

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

output$nrRows <- renderUI({
	if(is.null(input$datasets)) return()
	dat <- getdata()

	# number of observations to show in dataview
	nr <- nrow(dat)
	sliderInput("nrRows", "Number of days:", min = 1, max = nr, value = min(30,nr), step = 1)
})

# variable selection in the datatabs views
output$vizvars1 <- renderUI({
	# cols <- input$columns
	cols <- varnames()
	if(is.null(cols)) return()

	selectInput(inputId = "vizvars1", label = "X-variable", choices = as.list(cols), selected = NULL, multiple = FALSE)
})

# variable selection
output$vizvars2 <- renderUI({
	# cols <- input$columns
	cols <- varnames()
	if(is.null(cols)) return()
	# selectInput(inputId = "vizvars2", label = "Y-variable", choices = as.list(cols[-which(cols == input$vizvars1)]), selected = NULL, multiple = TRUE)
	selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols[-which(cols == input$vizvars1)])), selected = "", multiple = FALSE)
})

output$viz_color <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	# isFct <- sapply(getdata(), is.integer)
 # 	cols <- cols[isFct]
	selectInput('viz_color', 'Color', c('None'="", as.list(cols)))
})

output$viz_facet_row <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
	# selectInput('viz_facet_row', 'Facet row', c(None='.', as.list(cols[-which(cols == input$viz_color)])))
	selectInput('viz_facet_row', 'Facet row', c(None='.', as.list(cols)))
})

output$viz_facet_col <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
	# selectInput('viz_facet_col', 'Facet col', c(None='.', as.list(cols[-which(cols == input$viz_color)])))
	selectInput('viz_facet_col', 'Facet col', c(None='.', as.list(cols)))
})

################################################################
# Data reactives - view, plot, transform data, and log your work
################################################################
output$dataviewer <- renderTable({
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
	data.frame(dat[(nrRows-nr):nrRows, input$columns, drop = FALSE])

	# idea: Add download button so data can be saved
	# example here https://github.com/smjenness/Shiny/blob/master/SIR/server.R
})

output$stockplot<- renderPlot(function() {
	if(is.null(input$datasets)) return()
	if(input$datatabs != 'Stock plot') return()

	# inspired by Winston's stocks app http://glimmer.rstudio.com/winston/stocks/
	dat <- getdata()
	if(input$sp_logdat) dat <- log(dat)

	chartSeries(dat, name = input$datasets, type = input$sp_charttype, 
		subset = paste("last", input$nrRows, "days"), theme     = "white")

})

output$visualize <- renderPlot(function() {
	if(is.null(input$datasets) || is.null(input$vizvars2)) return()
	if(input$datatabs != 'Visualize') return()

		# inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
		dat <- getdata()

		if(input$vizvars2 == "") {
			p <- ggplot(dat, aes_string(x=input$vizvars1)) + geom_histogram(colour = 'black', fill = 'blue') 
			return(print(p))
		} else {
		  p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_point()
		}

    if (input$viz_color != '') {
    	# p <- p + aes_string(color=input$viz_color) + scale_colour_gradient(colors=rainbow(4))
    	p <- p + aes_string(color=input$viz_color) + scale_fill_brewer()
    }

    facets <- paste(input$viz_facet_row, '~', input$viz_facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$viz_jitter)
      p <- p + geom_jitter()
    if (input$viz_smooth)
      p <- p + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
    
    print(p)
}, width = 800, height = 800)

# output$transform <- renderTable({
output$transform <- renderPrint({
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

	cat("Command box and data view (in development)\n")

})

output$logwork <- renderPrint(function() {
	if(is.null(input$datasets) || is.null(input$columns)) return()

	# if(input$datatabs != 'Log') return()
	# idea: When a user presses a log-button the output on screen is saved to an rda file
	# ala the sesson data (.Radata). It would be like taking a snap-shot of the app-input
	# and then call the relevant parts from an Rmd file that gets-sourced. By default all snap
	# shots are shown in log but user can deseleted snap-shots as desired.
	# take another look at Jeff's teaching log. this could be a great starting point
	# ask Jeff about how to attribute code (and also Yihie) if you use some of their code
	# https://github.com/jeffreyhorner/TeachingLab

	cat("Analysis log (in development)\n")

})

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
output$summary <- renderPrint({
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
output$plots <- renderPlot({

	# plotting could be expensive so only done when tab is being viewed
	if(input$tool == 'dataview' || input$analysistabs != 'Plots') return()

	f <- get(paste("plot",input$tool,sep = '.'))
	result <- get(input$tool)()
	if(!is.character(result)) {
		f(result)
	} else {
		plot(x = 1, type = 'n', main="No variable selection made", axes = FALSE, xlab = "", ylab = "")
	}
}, width=800, height=800)
