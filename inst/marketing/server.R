shinyServer(function(input, output) {

	# source base functions
	source('radyant.R', local = TRUE)

	# source analysis tools
	flist <- sourceDirectory('tools', recursive = TRUE)

	# analysis ui-element caller
	output$ui_analysis <- renderUI({
  	if(input$tool == "dataview") return()
	  get(paste('ui_',input$tool, sep=""))()
	})

	# data ui-element caller - not used yet
	# output$ui_dataview <- renderUI(function() {
 	#  	if(input$tool != "dataview") return()
	#   get(paste('ui_',input$datatabs, sep=""))()
	# })
})
