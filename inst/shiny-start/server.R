shinyServer(function(input, output) {

	output$summary <- reactivePrint(function() {

		print(as.list(input))

	})
})