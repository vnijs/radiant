shinyServer(function(input, output, session) {

	# source shared functions
	source('../base/radiant.R', local = TRUE)

	# source data & app tools from base
  for(file in list.files(c("../base/tools/app","../base/tools/data"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)

	# source analysis tools for quant app
  for(file in list.files(c("tools/analysis"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)

#   if(Sys.getenv("RSTUDIO") != "1") saveStateOnCrash(session)
  if(Sys.getenv("RSTUDIO") != "1" & running_local) saveStateOnCrash(session)

})
