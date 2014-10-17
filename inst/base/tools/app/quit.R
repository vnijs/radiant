#######################################
# Quit app
#######################################
output$quit <- renderUI({
  pth <- "~/radiant_temp/rmd/figure/"
  if(file.exists(pth)) unlink(pth, recursive = TRUE)
  pth <- "~/radiant_temp/state/"
  if(file.exists(pth)) unlink(pth, recursive = TRUE)
#   rsf_file <- (paste0(pth,"RadiantState-",Sys.Date(),".rsf"))
#   if(file.exists(rsf_file)) file.remove(rsf_file)

   stopApp("Stop Radiant")   # stop Radiant
   q("no")     # quit R
})
