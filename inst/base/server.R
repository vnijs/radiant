shinyServer(function(input, output, session) {

	# source shared functions
	source('radiant.R', local = TRUE)

	# source data & analysis tools
	R.utils::sourceDirectory('tools/data', recursive = TRUE, modifiedOnly = FALSE)
 	R.utils::sourceDirectory('tools/app', recursive = TRUE, modifiedOnly = FALSE)

	# Source the partials/<pageTab>.R file and return the result.
	# It's important to pass local=TRUE so evaluation happens here,
	# not in the global environment.
#   for(file in list.files(c("tools/data","tools/app"), full.names = TRUE)) {
#     source(file, local = TRUE)
#     source(file.path(file), local = TRUE)
#   }

})
