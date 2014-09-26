shinyServer(function(input, output, session) {

	# source shared functions
	source('../base/radiant.R', local = TRUE)

	# source data & app tools from base
	R.utils::sourceDirectory('../base/tools/app', recursive = TRUE, modifiedOnly = FALSE)
	R.utils::sourceDirectory('../base/tools/data', recursive = TRUE, modifiedOnly = FALSE)

	# source additional analysis tools for marketing app
	R.utils::sourceDirectory('tools/analysis', recursive = TRUE, modifiedOnly = FALSE)
})
