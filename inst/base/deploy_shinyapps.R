# deploy to shinyapps.io
devtools::install_github("vnijs/radiant")
devtools::install_github(c("smartinsightsfromdata/rpivotTable","trestletech/shinyAce"))
devtools::install_github(c("jeroenooms/jsonlite", "rstudio/shiny"))
devtools::install_github(c("ramnathv/htmlwidgets","rstudio/DT"))

options(repos = "http://cran.rstudio.com")
# install.packages("rmarkdown")
# install.packages("ggvis")
# install.packages("testthat")
# devtools::install_github('rstudio/shinyapps')
# devtools::install_github("themel/sendmailR")
# devtools::install_github("jimhester/covr")

library(shinyapps)
fpath <- "~/gh/radiant_dev/inst/base"
setwd(fpath)
deployApp(account = "vnijs")
setwd(file.path(fpath,"../quant"))
deployApp(account = "vnijs")
setwd(file.path(fpath,"../marketing"))
deployApp(account = "vnijs")

# in case of problems
# shinyapps::showLogs(entries=1000)
