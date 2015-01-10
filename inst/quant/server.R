shinyServer(function(input, output, session) {

	# source shared functions
	source('../base/radiant.R', local = TRUE)

	# source data & app tools from base
  for(file in list.files(c("../base/tools/app","../base/tools/data"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)

	# source analysis tools for quant app
  for(file in list.files(c("tools/analysis"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)

  # integrate components from app_dev
	source('../../app_dev/quant/base/single_mean.R', local = TRUE)
	source('../../app_dev/quant/base/single_mean_ui.R', local = TRUE)

  if(Sys.getenv("RSTUDIO") != "1" & running_local) saveStateOnCrash(session)

})
