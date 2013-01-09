# varnames <- reactive(function() {
varnames <- function() {
	if(is.null(input$datasets)) return()

	# dat <- get(input$dataset)
	dat <- getdata()
	colnames <- names(dat)
	names(colnames) <- paste(colnames, " {", sapply(dat,class), "}", sep = "")
	colnames
}

getdata <- function(addCol = NULL, addColName = "") {

	dat <- get(input$datasets)
	if(!is.null(addCol)) {
		dat[,addColName] <- addCol
		head(dat,10)
	}
	dat
}

output$columns <- reactiveUI(function() {
	# input$columns # need this so choose columns gets updated when data is changed
	cols <- varnames()

	# Create a group of checkboxes and select them all by default
	checkboxGroupInput("columns", "Choose columns", choices  = as.list(cols), selected = names(cols))
})

output$dataviewer <- reactiveTable(function() {
	if(is.null(input$datasets) || is.null(input$columns)) return()

	# dat <- get(input$dataset)
	dat <- getdata()

	# Keep the selected columns
	dat <- dat[, input$columns, drop = FALSE]

	head(dat, input$nrRows)
	# head(dat, 1)
})

output$datasets <- reactiveUI(function() {

	state <- as.list(input)
	# loading user data
	if(!is.null(state$upload)) {
		if(state$upload$name != lastLoadedData$userData && state$upload$name != '') {
			loadUserData(state$upload)
			lastLoadedData$userData <<- state$upload$name
	  }
	} 
	# loading package data
	if(!is.null(state$packData) && state$packData != "") {
		if(state$packData != lastLoadedData$packData) {
			loadPackData(state$packData)
			lastLoadedData$packData <<- state$packData 
		}
	}

	# Drop-down selection of data set
	selectInput(inputId = "datasets", label = "Data sets", choices = datasets, selected = datasets[1], multiple = FALSE)
})

output$packData <- reactiveUI(function() {

	# Drop-down selection of data sets from 'car' package
	selectInput(inputId = "packData", label = "Load package data:", choices = packDataSets, selected = '', multiple = FALSE)
})

output$nrRows <- reactiveUI(function() {
	if(is.null(input$datasets)) return()
	# dat <- get(input$dataset)
	dat <- getdata()

	# number of observations to show in data view
	nr <- nrow(dat)
	sliderInput("nrRows", "# of rows to show:", min = 1, max = nr, value = min(10,nr), step = 1)
})

# variable selection
output$var1 <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()

	selectInput(inputId = "var1", label = labels1[input$tool], choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection
output$var2 <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()
	selectInput(inputId = "var2", label = labels2[input$tool], choices = vars[-which(vars == input$var1)], selected = NULL, multiple = TRUE)
})

output$varinterdep <- reactiveUI(function() {
	vars <- varnames()
	if(is.null(vars)) return()
	selectInput(inputId = "varinterdep", label = "Variables", choices = vars, selected = NULL, multiple = TRUE)
})

# Analysis reactives
output$visualize <- reactivePlot(function() {
	if(is.null(input$datasets) || is.null(input$columns)) return()

	# dat <- get(input$dataset)
	dat <- getdata()

	x <- dat[,input$var1]
	ifelse(is.factor(x), bw <- .1, bw <- diff(range(x)) / 12)

	if(is.null(input$var2)) {
		p <- ggplot(dat, aes_string(x=input$var1)) + geom_histogram(colour = 'black', fill = 'blue', binwidth = bw)
	} else {
		y <- dat[,input$var2]
		ifelse(is.factor(y), jitt <- .1, jitt <- diff(range(y)) / 15)

		p <- ggplot(dat, aes_string(x=input$var1, y=input$var2)) + geom_point() + geom_jitter(position = position_jitter(width = bw, height = jitt)) + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
	}
	print(p)
})

################################################################
# Analysis reactives - functions have the same names as the 
# values for the toolChoices values # in global.R
# If calling a reactive several times is more efficient than 
# calling a regular function several times I might want to use
# these
################################################################

# regression <- reactive(function() {
# 	if(is.null(input$var2)) return()
# 	main.regression(as.list(input))
# })

# compareMeans <- reactive(function() {
# 	if(is.null(input$var2)) return()
# 	as.list(input)
# })

# hclustering <- reactive(function() {
# 	if(is.null(input$varinterdep)) return()
# 	main.hclustering(as.list(input))
# })

# kmeansClustering <- reactive(function() {
# 	main.kmeansClustering(as.list(input))
# })

################################################################
# Output controls for the the Summary, Plots, and Extra tabs
################################################################
# Generate output for the summary tab
output$summary <- reactivePrint(function() {
	if(is.null(input$datasets)) return()

	# getting the summary function and feeding it the output from 
	# one of the analysis reactives above
	f <- get(paste("summary",input$tool,sep = '.'))
	f(get(input$tool)())

	# calling regular functions directly
	f <- get(paste("summary",input$tool,sep = '.'))
	f(get(input$tool)())

})

output$plots <- reactivePlot(function() {

	# plotting could be expensive so only done
	# when tab is being viewed
	if(!input$analysistabs == 'Plots') return()

	f <- get(paste("plot",input$tool,sep = '.'))
	f(get(input$tool)())
}, width=600, height=600)

# Generate output for the correlation tab
output$extra <- reactivePrint(function() {

	# if extra calculations are expensive
	# do only when tab is being viewed
	if(!input$analysistabs == 'Extra') return()

	f <- get(paste("extra",input$tool,sep = '.'))
	f(get(input$tool)())
})
