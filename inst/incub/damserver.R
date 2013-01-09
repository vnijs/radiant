# varnames <- reactive(function() {
varnames <- function() {
	if(is.null(input$dataset)) return()

	dat <- get(input$dataset)
	colnames <- names(dat)
	names(colnames) <- paste(colnames, " {", sapply(dat,class), "}", sep = "")

	return(colnames)
}

output$choose_columns <- reactiveUI(function() {
	# input$columns # need this so choose columns gets updated when data is changed
	columns <- varnames()


	# Create a group of checkboxes and select them all by default
	checkboxGroupInput("columns", "Choose columns", choices  = as.list(columns), selected = names(columns))
})

output$data <- reactiveTable(function() {
	if(is.null(input$dataset) || is.null(input$columns)) return()
	# if(is.null(input$dataset)) return()
	is.null(input$columns)
	varnames()
	dat <- get(input$dataset)


	# Keep the selected columns
	# dat <- dat[, input$columns, drop = FALSE]

	# head(dat, input$nrRows)
	head(dat, 1)
})

output$dataloaded <- reactiveUI(function() {

	# state <- as.list(input)
	# if(!is.null(state$upload)) {
	# 	if(state$upload$name != last_loaded_data_files$localfile && state$upload$name != '') {
	# 		loadlocaldata(state$upload)
	# 		last_loaded_data_files$localfile <<- state$upload$name
	#   }
	# } 
	# if(!is.null(state$pdataloaded) && state$pdataloaded != "") {
	# 	if(state$pdataloaded != last_loaded_data_files$packagefile) {
	# 		loadpackagedata(state$pdataloaded)
	# 		last_loaded_data_files$packagefile <<- state$pdataloaded 
	# 	}
	# }

	# Drop-down selection of data set
	selectInput(inputId = "dataset", label = "Data sets", choices = data_sets, selected = data_sets[1], multiple = FALSE)
})

output$packagedata <- reactiveUI(function() {


	# Drop-down selection of data sets from install packages
	selectInput(inputId = "pdataloaded", label = "Load package data sets", choices = pdata_sets, selected = '', multiple = FALSE)
})

output$rowsToShow <- reactiveUI(function() {
	if(is.null(input$dataset)) return()
	dat <- get(input$dataset)

	# number of observations to show in data view
	nrRow <- dim(dat)[1]
	sliderInput("nrRows", "# of rows to show:", min = 1, max = nrRow, value = min(15,nrRow), step = 1)
})

# variable selection
output$var1 <- reactiveUI(function() {
	if(is.null(varnames())) return()

	selectInput(inputId = "var1", label = labels1[input$tool], choices = varnames(), selected = NULL, multiple = FALSE)
})

# variable selection
output$var2 <- reactiveUI(function() {
	if(is.null(varnames())) return()
	selectInput(inputId = "var2", label = labels2[input$tool], choices = varnames()[-which(varnames() == input$var1)], selected = NULL, multiple = TRUE)
})

output$varinterdep <- reactiveUI(function() {
	if(is.null(varnames())) return()
	selectInput(inputId = "varinterdep", label = "Variables", choices = varnames(), selected = NULL, multiple = TRUE)
})



# Analysis reactives
output$visualize <- reactivePlot(function() {
	# if(is.null(input$dataset)) return()
	if(is.null(input$dataset) || is.null(input$columns)) return()

	# main.kmeansClustering(as.list(input))
	# dat <- get(input$dataset)
	# print(head(dat, 3))
	
	# input$dataset
	# input$columns
	# input$var1
	# input$var2
	# input$interdep

	# cluster not working
	dat <- get(input$dataset)

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

regression <- reactive(function() {
	# calling a reactive several times may be more efficient than calling a regular function several time
	if(is.null(input$var2)) return()


	main.regression(as.list(input))
})

compareMeans <- reactive(function() {
	if(is.null(input$var2)) return()
	as.list(input)
})

hclustering <- reactive(function() {
	if(is.null(input$varinterdep)) return()
	main.hclustering(as.list(input))
})

kmeansClustering <- reactive(function() {
	#if(is.null(input$varinterdep)) return()
	main.kmeansClustering(as.list(input))
	print("------ In first main.kmeansClustering ------")
	print(colnames(get(input$dataset)))
	print(colnames(get(input$dataset, inherit = TRUE)))
	# input$columns
})

# Generate output for the summary tab
output$summary <- reactivePrint(function() {
	if(is.null(input$dataset)) return()

	# f <- get(input$tool, inherit = TRUE)
	# f()
	# f <- get(paste("summary",input$tool,sep = '.'), inherit = TRUE)
	# func <- paste("summary",input$tool,sep = '.')
	# print(func)
	# f <- get(paste("summary",input$tool,sep = '.'))
	# f <- get(func)
	# f(input)

	# is.null(input$choose_columns)
	# is.null(input$varinterdep)
	# is.null(input$data)
	# varnames()

	kmc <- main.kmeansClustering(as.list(input))
	print(kmc)

	# print(as.list(input))

})

output$plots <- reactivePlot(function() {

	f <- get(paste("plot",input$tool,sep = '.'))
	f(get(input$tool, inherit = TRUE)())
}, width=600, height=600)

# Generate output for the correlation tab
output$extra <- reactivePrint(function() {

	f <- get(paste("extra",input$tool,sep = '.'))
	f(get(input$tool)())
})
