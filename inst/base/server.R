shinyServer(function(input, output, session) {

	# source shared functions
	source('radiant.R', local = TRUE)

	# source data & analysis tools
	R.utils::sourceDirectory('tools/data', recursive = TRUE, modifiedOnly = FALSE)
	R.utils::sourceDirectory('tools/app', recursive = TRUE, modifiedOnly = FALSE)
})
