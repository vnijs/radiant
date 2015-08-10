## ui for base radiant
shinyUI(
  do.call(navbarPage, c("Radiant", nav_ui, shared_ui))
)


# library(readr)
# packageVersion('readr')
# dat <- read_csv("Date\n2014-1-1\n2014-1-2")
# DT::datatable(dat, filter = list(position= "top"))
#
# devtools::install_github("hadley/readr")

#
# dat[[1]] <- as.Date(as.POSIXlt(dat[[1]]))
# DT::datatable(dat, filter = list(position= "top"))
#
#
# dat <- readr::read_csv("~/gh/mba_bootcamp/site/www/data/sales.csv") %>% as.data.frame
# dat[[1]] <- as.Date(as.POSIXlt(dat[[1]]))
# DT::datatable(dat, filter = list(position= "top"))
#
# dat[[1]] <- as.character(dat[[1]])
# dat[[1]] <- as.Date(dat[[1]])
#
# dat <- read.csv("~/gh/mba_bootcamp/site/www/data/sales.csv")
# dat[[1]] <- as.Date(dat[[1]])
# DT::datatable(dat, filter = list(position = "top"))
