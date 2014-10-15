shinyServer(function(input, output, session) {

	# source shared functions
	source('radiant.R', local = TRUE)

	# source data & analysis tools
  for(file in list.files(c("tools/app","tools/data"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)
})
