# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {

	# Generate output for the summary tab
	output$summary <- reactivePrint(function() {

		print(as.list(input))

	})
})