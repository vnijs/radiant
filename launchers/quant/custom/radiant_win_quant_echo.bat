set R=C:\Program Files\R\R-3.1.1\bin\R.exe
"%R%" -e "Sys.setlocale(category = 'LC_ALL','English_United States.1252'); setwd('C:/Users/Administrator/Dropbox/radiant'); source('radiant-miniCRAN/dependencies.R');  shiny::runApp('inst/quant', port = 4403, launch.browser=TRUE)"

pause
