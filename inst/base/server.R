shinyServer(function(input, output, session) {

  # source shared functions
	source("init.R", local = TRUE)
	source("radiant.R", local = TRUE)

	# for shiny-server
 	if (!"package:radiant" %in% search()) {
 		if (r_path == "..") {
      for (file in list.files("../../R",
		      pattern="\\.(r|R)$",
		      full.names = TRUE)) {

		  	source(file, local = TRUE)
		  }
		} else {
			radiant::copy_all(radiant)
      set_class <- radiant::set_class
		}
	} else {
	  copy_from(radiant, state_init, state_single, state_multiple)
	}

	# source data & analysis tools
  for (file in list.files(c("tools/app","tools/data"),
      pattern="\\.(r|R)$",
      full.names = TRUE)) {

  	source(file, local = TRUE)
  }

  # save state on refresh or browser close
  saveStateOnRefresh(session)
})
