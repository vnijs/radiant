shinyServer(function(input, output) {

	# source base functions
	source('radyant.R', local = TRUE)

	# source enabled analysis tools
	flist <- sourceDirectory('tools', recursive = TRUE)
	# print(flist)

	# the 'grand' analysis ui-element caller
	output$ui_analysis <- reactiveUI(function() {
  	if(input$tool == "dataview") return()
	  get(paste('ui_',input$tool, sep=""))()
	})

	# the 'grand' data ui-element caller - only for transform for now
	# not used yet - should all tools be separated out?
	# output$ui_transform <- reactiveUI(function() {
 	#  	if(input$tool != "dataview") return()
	#   get(paste('ui_',input$datatabs, sep=""))()
	# })
})
