:: Using 32-bit R unless 64-bit R is available
set R=C:\Program Files\R\R-3.1.1\bin\R.exe
IF exist "C:\Program Files\R\R-3.1.1\bin\x64" (set R=C:\Program Files\R\R-3.1.1\bin\x64\R.exe)

:: Finding the path to the dropbox info.json file
set fpath=%APPDATA%/Dropbox/info.json
set fpath=%fpath:\=/%

:: Commands used
:: "%R%" -e "Sys.setlocale(category = 'LC_ALL','English_United States.1252');
:: f <- file('%fpath%','r');
:: json_string <- suppressWarnings(readLines(f, -1L));
:: path_part <- sub('.*path\\\": \\\"','',json_string);
:: path <- paste0(sub('\\\",.*','',path_part), '\\\\radiant');
:: setwd(normalizePath(path, winslash='/'));
:: source('radiant-miniCRAN/dependencies.R');
:: shiny::runApp('inst/quant', port = 4403, launch.browser=TRUE)"

:: Uglyfied
"%R%" -e "Sys.setlocale(category = 'LC_ALL','English_United States.1252'); f <- file('%fpath%','r'); json_string <- suppressWarnings(readLines(f, -1L)); path_part <- sub('.*path\\\": \\\"','',json_string); path <- paste0(sub('\\\",.*','',path_part), '\\\\radiant'); setwd(normalizePath(path, winslash='/')); source('radiant-miniCRAN/dependencies.R');  shiny::runApp('inst/quant', port = 4403, launch.browser=TRUE)"

pause
