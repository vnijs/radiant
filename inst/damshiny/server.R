# sourcing global variables only needed in server.R. global.R is sourced automatically following edits to shiny.R
source('damglobal.R')

shinyServer(function(input, output) {

  datasets <- c("mymtcars","mtcars", "morley", "rock")
  mymtcars <- mtcars[,1:4]

	# Get the data set with the appropriate name
	source('damfunctions.R', local = TRUE)
	source('damserver.R', local = TRUE)

})