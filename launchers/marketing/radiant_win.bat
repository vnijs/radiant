set radrpath=C:\Program Files\R\R-3.1.1\bin\R.exe
set radpath=%HOMEDRIVE%%HOMEPATH%/Dropbox/radiant/inst/marketing/
set radpath=%radpath:\=/%
"%radrpath%" -e "Sys.setlocale(category = 'LC_ALL','English_United States.1252'); setwd('%radpath%'); source('../../radiant-miniCRAN/dependencies.R'); shiny::runApp('../marketing', port = 4475, launch.browser=TRUE)"