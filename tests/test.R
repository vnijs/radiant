# list of function classs - run and scan for errors
# todo: convert to testthat
setwd("~/Desktop/GitHub/radiant_dev/tests")
library(radiant)

# example data used in radiant
load("radiant.rda")

# installing packages to global_dir
repos <- "http://cran.rstudio.com"
options(repos = c(CRAN = repos))

# is local_dir is not in the library path (e.g., when usig brew in mac)
local_dir <- Sys.getenv("R_LIBS_USER")
.libPaths()
# if(!file.exists(local_dir)) {
# 	dir.create(local_dir, recursive = TRUE)
# 	.libPaths(local_dir)
# }

local_dir <- .libPaths()[1]
global_dir <- .libPaths()[2]

with_libpaths(new = global_dir, install_github("hadley/lineprof"))
install.packages('microbenchmark', lib = global_dir)
library(microbenchmark)
library(lineprof)
library(pryr)


x <- runif(100)
microbenchmark(
  sqrt(x),
  x %>% sqrt
)

x <- runif(100)
microbenchmark(
  is.null(x),
  x %>% is.null
)

x <- runif(100)
microbenchmark(
  nrow(x),
  x %>% nrow
)

# this does work when r_data is a reactive list
changedata_names <- function(oldnames, newnames)
  r_data[[input$dataset]] %<>% rename_(.dots = setNames(oldnames, newnames))

r_data <- list()
r_data$dat <- mtcars
input <- list()
input$dataset <- "dat"
r_data[[input$dataset]]
oldnames <- colnames(mtcars)
newnames <- paste0(oldnames,"_r")
r_data[[input$dataset]] %<>% rename_(.dots = setNames(oldnames, newnames))
r_data$dat

microbenchmark(
	r_data[[input$dataset]] %<>% rename_(.dots = setNames(newnames, newnames)),
	rename_(r_data[[input$dataset]], .dots = setNames(newnames, newnames))
)

# check if a variable is null or not in the data
not_available <- function(x)
  ifelse(any(is.null(x)) || (sum(x %in% varnames()) < length(x)), TRUE, FALSE)

not_available2 <- function(x)
  if(any(is.null(x)) || (sum(x %in% varnames()) < length(x))) TRUE else FALSE

varnames <- function() c("x", "y")
x <- letters
microbenchmark(
  not_available(x),
  not_available2(x)
)

# check for duplicate entries
has_duplicates <- function(x)
  ifelse(length(x %>% unique) < length(x), TRUE, FALSE)

has_duplicates2 <- function(x)
  if(length(unique(x)) < length(x)) TRUE else FALSE

x <- c(letters,letters)
microbenchmark(
  has_duplicates(x),
  has_duplicates2(x)
)

s_date <- read.table(header = FALSE, text = "1/1/10
1/2/10
1/3/10
1/4/10
1/5/10
1/6/10
1/7/10
1/8/10
1/9/10
1/10/10
1/11/10
1/12/10
1/13/10
1/14/10
1/15/10")

s_date
library(lubridate)
mdy(s_date) %>% as.Date

# convert a date variable to character for printing
d2c <- function(x) if(x %>% is_date) { x %>% as.character } else { x }
d2c2 <- function(x) if(is_date(x)) as.character(x) else x

is_date <- function(x) is.Date(x) | is.POSIXct(x) | is.POSIXt(x)

x <- s_date
microbenchmark(
  d2c(x),
  d2c2(x)
)


getdata <- function(dataset, vars = "", na.rm = TRUE, filt = "", slice = "") {

  if(exists("r_env")) {
      r_env$r_data[[dataset]]
    } else if(exists("r_data") && !is.null(r_data[[dataset]])) {
      # if(running_local) cat("Dataset", dataset, "loaded from r_data list\n")
      r_data[[dataset]]
    } else if(exists(dataset)) {
      # cat("Dataset", dataset, "loaded from global environment\n")
      get(dataset)
    } else {
      paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
        stop %>% return
    }
}

reg1 <- function(dataset) {
	getdata(dataset) %>%
	lm(mpg ~ cyl + vs, data = .)
}

reg2 <- function(dat) {
	lm(mpg ~ cyl + vs, data = dat)
}

library(ggplot2)
dat <- mtcars
dataset <- "dat"
# reg1(dataset)
# reg2(dat)

microbenchmark(times = 1000,
  reg1(dataset),
  reg2(dat)
)


getdata <- function(dataset) {
  get(dataset)
}

reg1 <- function(dataset) {
	getdata(dataset) %>%
	lm(mpg ~ cyl + vs, data = .)
}

reg2 <- function(dat) {
	lm(mpg ~ cyl + vs, data = dat)
}

dat <- mtcars
dataset <- "dat"
reg1(dataset)
reg2(dat)

reg1 <- function(dataset) {
	getdata(dataset) %>%
	lm(price ~ carat + color, data = .)
}

reg2 <- function(dat) {
	lm(price ~ carat + color, data = dat)
}

library(ggplot2)
dat <- diamonds
dataset <- "dat"
reg1(dataset)
reg2(dat)

microbenchmark(times = 100,
  reg1(dataset),
  reg2(dat)
)

r_env <- new.env()
r_env$diamonds <- diamonds

getdata <- function(dataset) {
  r_env$dataset
}

reg1 <- function(dataset) {
	getdata(dataset) %>%
	lm(mpg ~ cyl + vs, data = .)
}

reg2 <- function(dat) {
	lm(mpg ~ cyl + vs, data = dat)
}

dat <- diamonds
dataset <- "dat"
reg1(dataset)
reg2(dat)

microbenchmark(times = 100,
  reg1(dataset),
  reg2(dat)
)

reg1 <- function(dataset) {
	local_dat <- getdata(dataset)
	lm(price ~ carat + color, data = local_dat)
}

reg2 <- function(dat) {
	local_dat <- dat
	lm(price ~ carat + color, data = dat)
}

dat <- diamonds
dataset <- "dat"
reg1(dataset)
reg2(dat)

microbenchmark(times = 100,
  reg1(dataset),
  reg2(dat)
)

getdata_full <- function(dataset, vars = "", na.rm = TRUE, filt = "", slice = "") {

	filt %<>% gsub("\\s","", .)

	{ if(exists("r_env")) {
	    r_env$r_data[[dataset]]
	  } else if(exists("r_data") && !is.null(r_data[[dataset]])) {
	    if(running_local) cat("Dataset", dataset, "loaded from r_data list\n")
	    r_data[[dataset]]
	  } else if(exists(dataset)) {
	    cat("Dataset", dataset, "loaded from global environment\n")
	    get(dataset)
	  } else {
	    paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
	      stop %>% return
	  }
	} %>% { if(filt == "") . else filter_(., filt) } %>%
	      { if(slice == "") . else slice_(., slice) } %>%
	      { if(vars[1] == "") . else select_(., .dots = vars) } %>%
	      { if(na.rm) na.omit(.) else . }
}

getdata <- function(dataset, vars = "", na.rm = TRUE, filt = "", slice = "") {

	filt %<>% gsub("\\s","", .)

	{ if(exists("r_env")) {
			r_env$r_data[[dataset]]
		} else if(exists("r_data") && !is.null(r_data[[dataset]])) {
			if(running_local) cat("Dataset", dataset, "loaded from r_data list\n")
			r_data[[dataset]]
		} else if(exists(dataset)) {
			cat("Dataset", dataset, "loaded from global environment\n")
			get(dataset)
		} else {
			paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
			stop %>% return
		}
	} %>% { if(filt == "") . else filter_(., filt) } %>%
	      { if(slice == "") . else slice_(., slice) } %>%
	      { if(vars[1] == "") . else select_(., .dots = vars) }
	      # %>% { if(na.rm) { if(anyNA(.)) na.omit(.) else . } }
}

library(ggplot2)
library(microbenchmark)
library(magrittr)
dat <- diamonds
dataset <- "dat"
r_env <- new.env()
r_env$dat <- diamonds
r_env$r_data <- list()
r_env$r_data$dat <- diamonds

reg1 <- function(dataset)
	lm(price ~ carat + color, data = get(dataset))

reg2 <- function(dataset)
	lm(price ~ carat + color, data = r_env[[dataset]])

reg3 <- function(dat)
	lm(price ~ carat + color, data = dat)

reg4 <- function(dataset)
	lm(price ~ carat + color, data = getdata(dataset))

reg1(dataset)
reg2(dataset)
reg3(dat)
reg4(dataset)

microbenchmark(times = 100,
  reg1(dataset),
  reg2(dataset),
  reg3(dat),
  reg4(dataset)
)


library(lineprof)
prof <- lineprof(getdata("dat"))
shine(prof)



library(ggplot2)
library(dplyr)
library(microbenchmark)
library(magrittr)
dat <- diamonds
dataset <- "dat"
r_env <- new.env()
r_env$dat <- diamonds
r_env$r_data <- list()
r_env$r_data$dat <- diamonds

reg1 <- function(dataset, vars) {
	dat <- select_(get(dataset), .dots = vars)
	lm(price ~ carat + color, data = dat)
}

reg2 <- function(dataset, vars) {
	dat <- select_(r_env[[dataset]], .dots = vars)
	lm(price ~ carat + color, data = dat)
}

reg3 <- function(dat, vars) {
	dat <- select_(dat, .dots = vars)
	lm(price ~ carat + color, data = dat)
}

reg4 <- function(dataset, vars) {
	dat <- getdata(dataset, vars)
	lm(price ~ carat + color, data = dat)
}

vars <- c("price","carat","color")
reg1(dataset, vars)
reg2(dataset, vars)
reg3(dat, vars)
reg4(dataset, vars)

microbenchmark(times = 100,
  reg1(dataset, vars),
  reg2(dataset, vars),
  reg3(dat, vars),
  reg4(dataset, vars)
)







