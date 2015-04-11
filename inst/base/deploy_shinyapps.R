# deplory shinyapps.io
devtools::install_github("vnijs/radiant")
devtools::install_github("jeroenooms/jsonlite")
devtools::install_github("rstudio/shiny")

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
