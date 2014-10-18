#######################################
# Quit app
#######################################
output$quit <- renderUI({
  pth <- "~/radiant_temp/rmd/figure/"
  if(file.exists(pth)) unlink(pth, recursive = TRUE)
  pth <- "~/radiant_temp/state/"
  if(file.exists(pth)) unlink(pth, recursive = TRUE)
  stopApp("Stop Radiant")   # stop Radiant
  q("no")     # quit R
})
