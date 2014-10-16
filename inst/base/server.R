shinyServer(function(input, output, session) {

	# source shared functions
	source('radiant.R', local = TRUE)

	# source data & analysis tools
  for(file in list.files(c("tools/app","tools/data"), pattern="\\.(r|R)$", full.names = TRUE))
    source(file, local = TRUE)


	session$onSessionEnded(function() {
    observe({
      pth <- "~/radiant_temp/state/"
      if(!file.exists(pth)) dir.create(pth)
      filename = paste0(pth,"RadiantState-",Sys.Date(),".rsf")
      RadiantInputs <- isolate(reactiveValuesToList(input))
      RadiantValues <- isolate(reactiveValuesToList(values))
      save(RadiantInputs, RadiantValues , file = filename)
#       Sys.sleep(5)
#       if(session$isClosed()) q("ask")
    })
  })
})
