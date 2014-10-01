set R=C:\Program Files\R\R-3.1.1\bin\R.exe
IF exist "C:\Program Files\R\R-3.1.1\bin\x64" (set R=C:\Program Files\R\R-3.1.1\bin\x64\R.exe)

set radpath=%HOMEDRIVE%%HOMEPATH%/Dropbox/radiant
set radpath=%radpath:\=/%
"%R%" -e "setwd(normalizePath('%radpath%')); Sys.setlocale(category = 'LC_ALL','English_United States.1252'); setwd('%radpath%'); source('radiant-miniCRAN/dependencies.R'); shiny::runApp('inst/quant', port = 4403, launch.browser=TRUE)"

pause
