# deploy to shinyapps.io
devtools::install_github("vnijs/radiant")
devtools::install_github(c("smartinsightsfromdata/rpivotTable","trestletech/shinyAce"))
devtools::install_github(c("rstudio/DT"))
devtools::install_github(c('jeroenooms/jsonlite', 'rstudio/shiny', 'ramnathv/htmlwidgets'))

library(shinyapps)
fpath <- "~/gh/radiant_dev/inst/base"
setwd(fpath)
deployApp()
setwd(file.path(fpath,"../quant"))
deployApp()
setwd(file.path(fpath,"../marketing"))
deployApp()

# in case of problems
# shinyapps::showLogs(entries=1000)
