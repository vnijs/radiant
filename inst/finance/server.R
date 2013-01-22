# sourcing global variables only needed in server.R. global.R is sourced automatically following edits to shiny.R
source('damglobal.R')

shinyServer(function(input, output) {

	values <- reactiveValues()

	robjname <- load('data/GSPC.rda')
	values[[robjname]] <- get(robjname)
	datasets <- c(robjname)

	# nasdaq_file <- read.csv('data/nasdaq-company-list.csv')
	# symbol_list <- c("",nasdaq_file[,1])
	# names(symbol_list) <- c("",nasdaq_file[,2])

	# sourcing (reactive) functions
	source('damfunctions.R', local = TRUE)
	source('damreactives.R', local = TRUE)

})