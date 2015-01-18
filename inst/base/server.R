shinyServer(function(input, output, session) {

  # ip_state_list <- paste0("state_list",session$request$REMOTE_ADDR)
  # ip_values <- paste0("values",session$request$REMOTE_ADDR)

  # if(exists(ip_state_list)) {
  #   state_list <- get(ip_state_list)
  #   if(is.null(state_list)) state_list <- list()
  #   assign(ip_state_list, NULL, envir = .GlobalEnv)
  # }

  # if(exists(ip_values)) {
  #   values <- get(ip_values)
  #   if(is.null(values)) values <<- reactiveValues()
  #   assign(ip_values, NULL, envir = .GlobalEnv)
  # }

  # if(exists("state_list")) {
  #   print(state_list)
  # } else {
  #   print("no state_list")
  # }

	# source shared functions
	source('init.R', local = TRUE)
	source('radiant.R', local = TRUE)

	# source data & analysis tools
  for(file in list.files(c("tools/app","tools/data"),
      pattern="\\.(r|R)$",
      full.names = TRUE)) {

  	source(file, local = TRUE)
  }

# 	if(Sys.getenv("RSTUDIO") != "1" & running_local) saveStateOnCrash(session)
	# if(running_local) saveStateOnCrash(session)
# 	if(!running_local) saveStateOnServerCrash(session)
})
