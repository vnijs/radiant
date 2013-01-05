# sourcing global variables only needed in server.R. global.R is sourced automatically.
source('damglobal.R')

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

  data_sets <- c("mymtcars","mtcars", "morley", "rock")
  mymtcars <- mtcars[,1:4]

	# Get the data set with the appropriate name
	source('damfunctions.R', local = TRUE)
	source('damserver.R', local = TRUE)

})
