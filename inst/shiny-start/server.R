shinyServer(function(input, output) {

	output$summary <- renderPrint({

		print(as.list(input))

	})
})