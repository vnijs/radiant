shinyServer(function(input, output) {

	# source base functions
	source('radyant.R', local = TRUE)

	# source data & analysis tools
	flist_analysis <- sourceDirectory('tools/analysis', recursive = TRUE)
	flist_data <- sourceDirectory('tools/data', recursive = TRUE)

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
