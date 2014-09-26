set radrpath=C:\Program Files\R\R-3.1.1\bin\R.exe
"%radrpath%" -e "source('https://www.dropbox.com/s/bbch3xuyy7z4c6w/finding_radiant_app.R?dl=0'); shiny::runApp('inst/base', port = 4000, launch.browser=TRUE)"
