set radrpath=C:\Program Files\R\R-3.1.1\bin\R.exe
set radpath=%HOMEDRIVE%%HOMEPATH%/Dropbox/radiant/inst/base/
set radpath=%radpath:\=/%
"%radrpath%" -e "Sys.setlocale(category = 'LC_ALL','English_United States.1252'); setwd('%radpath%'); source('../../radiant-miniCRAN/dependencies.R'); shiny::runApp('../base', port = 4000, launch.browser=TRUE)"
