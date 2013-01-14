# sourcing global variables only needed in server.R. global.R is sourced automatically following edits to shiny.R
source('damglobal.R')

shinyServer(function(input, output) {

	# making a local copy of the data
	mtcars <- mtcars
	mtcars$vs <- as.factor(mtcars$vs)
	mtcars$am <- as.factor(mtcars$am)
	morley <- morley
	rock <- rock

	# variable that tracks if the action button was pressed
	bval <- FALSE

	# Our datasets can change over time (i.e. the changedata function). Therefore,
	# these need to be reactive values; otherwise, the other reactive functions
	# and outputs that depend on these datasets won't know that they are changed.
	#
	# We'll create an environment to hold these reactive values, and prepopulate
	# it with the initial contents of these datasets.
	#
	# Note that once we populate datasetEnv, we never get or assign the "original"
	# copies of mtcars, morley, or rock. This way, all user sessions are independent
	# from each other (since datasetEnv is being defined in a scope that's local to
	# a particular invocation of the shinyServer function).
	datasetEnv <- new.env()
	datasetEnv$mtcars <- reactiveValue(mtcars)
	datasetEnv$morley <- reactiveValue(morley)
	datasetEnv$rock <- reactiveValue(rock)

	datasets <- c("mtcars", "morley", "rock")

	# sourcing (reactive) functions
	source('damfunctions.R', local = TRUE)
	source('damreactives.R', local = TRUE)

})