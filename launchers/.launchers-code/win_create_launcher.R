# Creating windows batch file to launch Radiant
create_bat <- function(rhome, course) {

  radiant_bat <- paste0(rhome," -e \"Sys.setlocale(category = 'LC_ALL','English_United States.1252'); setwd('~/Dropbox/radiant/'); source('dependencies.R'); shiny::runApp('inst/",course[1],"', port = 4",course[2],", launch.browser=TRUE)\"")
  sink(paste0("launchers/",course[1],"/radiant_win_",course[1],".bat"))
  cat(radiant_bat)
  sink()
}

if (.Platform$OS.type == 'windows') {

  setwd('~/Dropbox/radiant/')
  rhome <- Sys.which("R")

  create_bat(rhome, c("quant","403"))
  create_bat(rhome, c("marketing","475"))
} 

q("no")
