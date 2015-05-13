# deploy to shinyapps.io

fn <- "~/gh/radiant_dev/inst/base/www/style.css"
readLines(fn) %>%
  gsub("top: 95px;", "top: 145px;", .) %>%
  cat(file = fn, sep = "\n")

system("git add --all .")
system("git commit -m 'Update css for shinyapps.io: `date +\"%m-%d-%Y\"` [ci skip]'")
system("git push")

devtools::install_github("vnijs/radiant")
devtools::install_github(c("smartinsightsfromdata/rpivotTable","trestletech/shinyAce"))
devtools::install_github(c("jeroenooms/jsonlite", "rstudio/shiny"))
devtools::install_github(c("ramnathv/htmlwidgets","vnijs/DT"))

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
Sys.sleep(300)
setwd(file.path(fpath,"../quant"))
deployApp(account = "vnijs")
Sys.sleep(300)
setwd(file.path(fpath,"../marketing"))
deployApp(account = "vnijs")
Sys.sleep(300)

fn <- "~/gh/radiant_dev/inst/base/www/style.css"
readLines(fn) %>%
  gsub("top: 145px;", "top: 95px;", .) %>%
  cat(file = fn, sep="\n")

system("git add --all .")
system("git commit -m 'Undo css update for shinyapps.io: `date +\"%m-%d-%Y\"` [ci skip]'")
system("git push")

# in case of problems
# shinyapps::showLogs(entries=1000)
