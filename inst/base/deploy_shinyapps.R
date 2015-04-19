# deploy to shinyapps.io
devtools::install_github("vnijs/radiant")
devtools::install_github(c("smartinsightsfromdata/rpivotTable","trestletech/shinyAce"))
devtools::install_github(c("jeroenooms/jsonlite", "rstudio/shiny"))
devtools::install_github(c("ramnathv/htmlwidgets","rstudio/DT"))

# install.packages("rmarkdown")
# install.packages("ggvis")
# install.packages("testthat")
# devtools::install_github("jimhester/covr")

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
