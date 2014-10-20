#######################################
# Quit app
#######################################
output$quit <- renderUI({
  pth <- "~/radiant_temp/rmd/figure/"
  if(file.exists(pth)) unlink(pth, recursive = TRUE)
  pth <- "~/radiant_temp/state/"
  if(file.exists(pth)) unlink(pth, recursive = TRUE)
  stopApp("Stop Radiant")   # stop Radiant
  # quit R, unless you are running Rstudio
  if(Sys.getenv("RSTUDIO") != "1") q("no")
})

output$quitsave <- renderUI({
  updateTabsetPanel(session, "nav_radiant", selected = "State")

  ###
  # try ShinyFiles!
  ###

#   downloadHandler(
#     filename = function() { paste0("RadiantState-",Sys.Date(),".rsf") },
#     content = function(file) {
#
#       isolate({
#         RadiantInputs <- isolate(reactiveValuesToList(input))
#         RadiantValues <- isolate(reactiveValuesToList(values))
#         save(RadiantInputs, RadiantValues , file = file)
#       })
#     }
#   )
})


#######################################
# Save state
#######################################
# output$quitDownloadState <- downloadHandler(
#   filename = function() { paste0("RadiantState-",Sys.Date(),".rsf") },
#   content = function(file) {
#
#     isolate({
#       RadiantInputs <- isolate(reactiveValuesToList(input))
#       RadiantValues <- isolate(reactiveValuesToList(values))
#       save(RadiantInputs, RadiantValues , file = file)
#     })
#   }
# )
