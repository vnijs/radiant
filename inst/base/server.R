shinyServer(function(input, output, session) {

  # source shared functions
	source('init.R', local = TRUE)
	source('radiant.R', local = TRUE)

	# if(exists("state_list")) {
	#   if(length(state_list) == 0) {
	#     print("Empty list")
	#   } else {
	#     print(str(state_list[sort(names(state_list))]))
	#   }
	# } else {
	#   print("no state_list")
	# }

	# for shiny-server
  if(!running_local) {
	  for(file in list.files("../../R",
	      pattern="\\.(r|R)$",
	      full.names = TRUE)) {

	  	source(file, local = TRUE)
	  }
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
