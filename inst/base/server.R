shinyServer(function(input, output, session) {

  # source shared functions
	source('init.R', local = TRUE)
	source('radiant.R', local = TRUE)

	# for shiny-server
  if(!running_local) {
	  for(file in list.files("../../R",
	      pattern="\\.(r|R)$",
	      full.names = TRUE)) {

	  	source(file, local = TRUE)
	  }
	} else {
		# this works when compare_means is not exported
		# source("~/gh/radiant_dev/R/compare_means.R", local = TRUE)
		# this works to get the environment shiny is using
		env_shiny <<- pryr::where("values")
		# what about attach?
	}

	# source data & analysis tools
  for(file in list.files(c("tools/app","tools/data"),
      pattern="\\.(r|R)$",
      full.names = TRUE)) {

  	source(file, local = TRUE)
  }


  # save state on refresh or browser close
  saveStateOnRefresh(session)
})
