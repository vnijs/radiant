shinyServer(function(input, output, session) {

	# source shared functions
	source('../base/radiant.R', local = TRUE)

	# source data & app tools from base
  for(file in list.files(c("../base/tools/app","../base/tools/data"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)

	# source analysis tools for quant app
  for(file in list.files(c("tools/analysis"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)

	session$onSessionEnded(function() {
	  observe({
	    pth <- "~/radiant_temp/state"
	    if(!file.exists(pth)) dir.create(pth, recursive = TRUE)
	    filename = paste0(pth,"RadiantState-",Sys.Date(),".rsf")
	    RadiantInputs <- isolate(reactiveValuesToList(input))
	    RadiantValues <- isolate(reactiveValuesToList(values))
	    save(RadiantInputs, RadiantValues , file = filename)
	  })
	})
})
