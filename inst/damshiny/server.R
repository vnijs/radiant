# sourcing global variables only needed in server.R. global.R is sourced automatically following edits to shiny.R
source('damglobal.R')

shinyServer(function(input, output) {

	# making a local copy of the data
	mtcars <- mtcars
	mtcars$vs <- as.factor(mtcars$vs)
	mtcars$am <- as.factor(mtcars$am)
	morley <- morley
	rock <- rock

	datasets <- c("mtcars", "morley", "rock")

	# sourcing (reactive) functions
	source('damfunctions.R', local = TRUE)
	source('damreactives.R', local = TRUE)

})