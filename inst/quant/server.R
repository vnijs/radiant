shinyServer(function(input, output, session) {

	# source shared functions
	source('../base/init.R', local = TRUE)
	source('../base/radiant.R', local = TRUE)

	# source data & app tools from base
  for(file in list.files(c("../base/tools/app","../base/tools/data"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)

	# source analysis tools for quant app
  for(file in list.files(c("tools/analysis"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)

	# save state on crash only if running locally and not from Rstudio
  # if(Sys.getenv("RSTUDIO") != "1" & running_local) saveStateOnRefresh(session)
  saveStateOnRefresh(session)

})
