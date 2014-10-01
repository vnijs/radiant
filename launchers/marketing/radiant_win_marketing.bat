:: Working
:: "%R%" -e "Sys.setlocale(category = 'LC_ALL','English_United States.1252'); setwd('%radpath%'); source('radiant-miniCRAN/dependencies.R'); shiny::runApp('inst/marketing', port = 4475, launch.browser=TRUE)"

"%R%" -e "fpath <- normalizePath('%APPDATA%\Dropbox\info.json', winslash = '/'); fpath"
:: f <- file(fpat,'r');
:: json_string <- suppressWarnings(readLines(f, -1L));
:: path_part <- sub('.*path\\\": \\\"','',json_string);
:: path <- sub('\\\",.*','',path_part);
:: setwd(path);
:: source('radiant-miniCRAN/dependencies.R');
:: shiny::runApp('inst/marketing', port = 4475, launch.browser=TRUE)"

set R=C:\Program Files\R\R-3.1.1\bin\x64\R.exe
set fpath=%APPDATA%/Dropbox/info.json
set fpath=%fpath:\=/%

"%R%" -e "Sys.setlocale(category = 'LC_ALL','English_United States.1252');  f <- file('%fpath%','r'); json_string <- suppressWarnings(readLines(f, -1L)); json_string"


path_part <- sub('.*path\\\": \\\"','',json_string); path <- sub('\\\",.*','',path_part); setwd(path); source('radiant-miniCRAN/dependencies.R');  shiny::runApp('inst/marketing', port = 4475, launch.browser=TRUE)"
