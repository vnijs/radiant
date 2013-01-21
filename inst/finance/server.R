# sourcing global variables only needed in server.R. global.R is sourced automatically following edits to shiny.R
source('damglobal.R')

shinyServer(function(input, output) {

	values <- reactiveValues()

	robjname <- load('data/GSPC.rda')
	values[[robjname]] <- get(robjname)
	datasets <- c(robjname)

	# sourcing (reactive) functions
	source('damfunctions.R', local = TRUE)
	source('damreactives.R', local = TRUE)

})