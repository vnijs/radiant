# sourcing global variables only needed in server.R. global.R is sourced automatically following edits to shiny.R
source('damglobal.R')

shinyServer(function(input, output) {

	# making a local copy of the data
	mtcars <- mtcars
	mtcars$vs <- as.factor(mtcars$vs)
	mtcars$am <- as.factor(mtcars$am)
	morley <- morley
	rock <- rock

	# Our datasets can change over time (i.e. the changedata function). Therefore,
	# these need to be reactive values; otherwise, the other reactive functions
	# and outputs that depend on these datasets won't know that they are changed.

	# Note that we never get or assign the "original" copies of mtcars, morley, 
	# or rock. This way, all user sessions are independent from each other 

	values <- reactiveValues()
	values$mtcars <- mtcars
	values$morley <- morley
	values$rock <- rock
	values$diamonds <- diamonds[1:1000,]

	datasets <- c("diamonds","mtcars", "morley", "rock")
	lastLoaded <- "" 		

	# sourcing (reactive) functions
	source('damfunctions.R', local = TRUE)
	source('damreactives.R', local = TRUE)
	source('serverControls.R', local = TRUE)

})