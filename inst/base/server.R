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

	# source data & analysis tools
  for(file in list.files(c("tools/app","tools/data"),
      pattern="\\.(r|R)$",
      full.names = TRUE)) {

  	source(file, local = TRUE)
  }

  # save state on crash only if running locally and not from Rstudio
  # if(Sys.getenv("RSTUDIO") != "1" & running_local) saveStateOnRefresh(session)
  # if(running_local) saveStateOnRefresh(session)
  saveStateOnRefresh(session)
})
