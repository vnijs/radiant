# deplory shinyapps.io
devtools::install_github("vnijs/radiant")
devtools::install_github("rstudio/shiny")
devtools::install_github("jeroenooms/jsonlite")

library(shinyapps)
fpath <- "~/gh/radiant_dev/inst/base"
setwd(fpath)
deployApp()

shinyapps::showLogs(entries=1000)

setwd(file.path(fpath,"../quant"))
deployApp()
setwd(file.path(fpath,"../marketing"))
deployApp()
