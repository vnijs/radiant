
shinyServer(function(input, output, session) {

	# source shared functions
	source('../base/init.R', local = TRUE)
	source('../base/radiant.R', local = TRUE)

	# for shiny-server
  if(!"package:radiant" %in% search()) {
	  for(file in list.files("../../R",
	      pattern="\\.(r|R)$",
	      full.names = TRUE)) {

	  	source(file, local = TRUE)
	  }
	}

	# source data & app tools from base
	for(file in list.files(c("../base/tools/app", "../base/tools/data"),
	    										 pattern="\\.(r|R)$", full.names = TRUE))
	  source(file, local = TRUE)

	# source analysis tools from quant app
	for(file in list.files(c("../quant/tools/analysis"),
	    										 pattern="\\.(r|R)$", full.names = TRUE))
	  source(file, local = TRUE)

	# source additional analysis tools for marketing app
  for(file in list.files(c("tools/analysis"),
      										 pattern="\\.(r|R)$", full.names = TRUE))
	  source(file, local = TRUE)

	# shinyFileChoose(input, 'file', roots=c('R Installation'=R.home()), session=session, restrictions=system.file(package='base'))
	# shinyFileChoose(input, 'file', roots=c('R Installation'=R.home()), session=session)

  # save state on refresh or browser close
  saveStateOnRefresh(session)

})
