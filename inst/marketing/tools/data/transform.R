# UI-elements for transform
output$tr_columns <- reactiveUI(function() {
	cols <- varnames()
	selectInput("tr_columns", "Select column(s) to transform:", choices  = as.list(cols), selected = NULL, multiple = TRUE)
})

output$tr_nrRows <- reactiveUI(function() {
	if(is.null(input$datasets)) return()
	dat <- getdata()

	# number of observations to show in dataview
	nr <- nrow(dat)
	sliderInput("tr_nrRows", "Rows to show (max 50):", min = 1, max = nr, value = min(15,nr), step = 1)
})

sq <<- function(x) x^2
inv <<- function(x) 1/x
st <<- scale
cent <<- function(x) x - mean(x, na.rm = TRUE)

# trans_options <- list("Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Sum" = "sum", "Mean" = "mean", "Standardize" = "", "Center" = "" )
# trans_options <- list("None" = "", "Delete columns" = "NULL", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Standardize" = "stdize")
trans_options <- list("None" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Center" = "cent", "Standardize" = "st", "Invert" = "inv")

output$ui_transform <- reactiveUI(function() {
	ui_transform()
})

ui_transform <- function() {
  wellPanel(
  	uiOutput("tr_nrRows"), 
    uiOutput("tr_columns"),
    selectInput("tr_transfunction", "Transform columns", trans_options),
    actionButton("addtrans", "Add transformed variables")
  )
}

transform <- reactive(function() {
	if(is.null(input$datasets) || is.null(input$tr_columns)) return()
	if(input$datatabs != 'Transform') return()

	# idea: use mutate to transformations on the data see links below
	# If will probably be easiest to have this be a text-box input field
	# that runs these. No need for an elaborate UI since it is basically
	# what they would otherwise do in excel. Make sure to add
	# some helptext with a bunch of examples of how the command would work.
	# http://www.inside-r.org/packages/cran/plyr/docs/mutate
	# https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/uZZT564y0i8
	# https://gist.github.com/4515551 

	dat <- getdata()
	if(!all(input$tr_columns %in% colnames(dat))) return()
	dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
	if(input$tr_transfunction != '') {
		cn <- c(colnames(dat),paste(input$tr_transfunction,colnames(dat), sep="."))
		# dat <- data.frame(cbind(dat,colwise(input$tr_transfunction)(dat)))
		dat <- cbind(dat,colwise(input$tr_transfunction)(dat))
		colnames(dat) <- cn
	}

	dat
})

output$transform_data <- reactiveTable(function() {
	if(is.null(input$datasets)) return()

	nr <- input$tr_nrRows
	dat <- transform()
	dat[max(1,nr-50):nr,, drop = FALSE]
})

# output$transform_summary <- reactivePrint(function() {
output$transform_summary <- reactiveTable(function() {
	if(is.null(input$datasets)) return()

	dat <- transform()
	if(is.null(dat)) return()
	# if(input$tr_transfunction != '') {
	# 	cn <- paste(input$tr_transfunction,input$tr_columns, sep=".")
	# } else {
	# 	cn <- colnames(dat)
	# }
	# isFct <- sapply(getdata(), is.factor)
	# print(summary(dat[,cn]))

	describe(dat)

})

observe(function() {
	if(is.null(input$addtrans) || input$addtrans == 0) return()
	isolate({
		dat <- transform()
		changedata(dat, colnames(dat))
	})

})


