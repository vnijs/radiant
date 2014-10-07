shinyServer(function(input, output, session) {

	# source shared functions
	source('radiant.R', local = TRUE)

	# source data & analysis tools
	R.utils::sourceDirectory('tools/data', recursive = TRUE, modifiedOnly = FALSE)
	R.utils::sourceDirectory('tools/app', recursive = TRUE, modifiedOnly = FALSE)

# 	if (grepl("\\W", input$partial))
# 	  return(NULL)
#
	# Source the partials/<pageTab>.R file and return the result.
	# It's important to pass local=TRUE so evaluation happens here,
	# not in the global environment.
# 	source(file.path("tools/data"), local=TRUE)
# 	list.files(file.path("tools/data"), full.names = TRUE)
# ?list.files

})
