# deploy to shinyapps.io

library(magrittr)
fn <- "~/gh/radiant_dev/inst/base/www/style.css"
readLines(fn) %>%
  gsub("top: 95px;", "top: 145px;", .) %>%
  cat(file = fn, sep = "\n")

system("git add --all .")
system("git commit -m 'Update css for shinyapps.io: `date +\"%m-%d-%Y\"` [ci skip]'")
system("git push")

# devtools::install_github(c("smartinsightsfromdata/rpivotTable","trestletech/shinyAce"))
# devtools::install_github(c("jeroenooms/jsonlite", "rstudio/shiny", "ramnathv/htmlwidgets"))
devtools::install_github(c("vnijs/radiant","vnijs/DT"))

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

for (file in list.files("../../../shinyapps/R", pattern="\\.(r|R)$", full.names = TRUE))
  source(file, local = TRUE)
source("../../build/deployapp.R", local = TRUE)

deployApp(account = "vnijs", launch.browser = FALSE)

setwd(file.path(fpath,"../quant"))
deployApp(account = "vnijs", launch.browser = FALSE)

setwd(file.path(fpath,"../marketing"))
deployApp(account = "vnijs", launch.browser = FALSE)

fn <- "~/gh/radiant_dev/inst/base/www/style.css"
readLines(fn) %>%
  gsub("top: 145px;", "top: 95px;", .) %>%
  cat(file = fn, sep="\n")

system("git add --all .")
system("git commit -m 'Undo css update for shinyapps.io: `date +\"%m-%d-%Y\"` [ci skip]'")
system("git push")

# in case of problems
# shinyapps::showLogs(entries=1000)
