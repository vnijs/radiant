#######################################
# Quit app
#######################################
output$quit <- renderUI({
#   unlink(c("www/rmd/cache/", "www/rmd/figure"), recursive = TRUE)
  unlink(c("~/radiant_temp/rmd/figure"), recursive = TRUE)

  pth <- "~/radiant_temp/state/"
  rsf_file <- (paste0(pth,"RadiantState-",Sys.Date(),".rsf"))
  if(file.exists(rsf_file)) file.remove(rsf_file)
#   unlink(rsf_file)

  stopApp()   # stop Radiant
#   q("no")     # quit R
})
