set radrpath=C:\Program Files\R\R-3.1.1\bin\R.exe
set radpath=%HOMEDRIVE%%HOMEPATH%/Dropbox/radiant/inst/quant/
set radpath=%radpath:\=/%
"%radrpath%" -e "Sys.setlocale(category = 'LC_ALL','English_United States.1252'); setwd('%radpath%'); source('../../radiant-miniCRAN/dependencies.R'); shiny::runApp('../quant', psetort = 4403, launch.browser=TRUE)"