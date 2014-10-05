#######################################
# Quit app
#######################################
output$quit <- renderUI({
#   unlink(c("www/rmd/cache/", "www/rmd/figure"), recursive = TRUE)
  unlink(c("~/radiant_temp/rmd/figure"), recursive = TRUE)
  stopApp()   # stop Radiant
  q("no")     # quit R
})
