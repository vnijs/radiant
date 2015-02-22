# list of function classs - run and scan for errors
# todo: convert to testthat

setwd("~/Desktop/GitHub/radiant_dev/tests")
library(radiant)
# unload(inst("radiant"))

# example data used in radiant
load("radiant.rda")
rm(r_env)

# analysis on built-in dataset mtcars
result <- single_mean("mtcars", "mpg", sm_comp_value = 20, sm_alternative = "greater")
summary(result)
# plot(result)

result <- compare_means("mtcars", "cyl", "mpg")
summary(result)
plot(result)

result <- compare_means("diamonds", "cut", "price", cm_adjust = "bonf", cm_plots = c("box","bar","density"))
plot(result)

# if the salary data was loaded in radiant you should get output here
result <- compare_means("salary", "rank", "salary")
summary(result)
plot(result)
result <- compare_means("diamonds","cut","price", cm_plots = "bar")

# should produce an error because no dataset with that name exists
result <- try(compare_means("no_dat", "cyl", "mpg"), silent = TRUE)
if(is(result, 'try-error')) print("Error produced as expected")

# regression
result <- regression("diamonds", "price", c("carat","clarity"), reg_plot = c("coef"))
summary(result)
plot(result)

result <- regression("diamonds", "price", c("carat","clarity"), reg_plot = c("dashboard"))
plot(result)
?regression

result <- correlation("diamonds", c("price","carat","clarity"))
summary(result)
plot(result)

result <- compare_means("diamonds","cut","price")
plot(result)

result <- conjoint(dataset = "mp3", ca_var1 = "Rating", ca_var2 = c("Memory", "Radio", "Size", "Price", "Shape"), ca_scale_plot = TRUE)
summary(result)
plot(result)

result <- compare_props("titanic", "pclass", "survived")
summary(result)

result <- conjoint("mp3", "Rating", c("Memory", "Radio", "Size", "Price", "Shape"))
summary(result)

result <- compare_props("titanic", "pclass", "survived")
summary(result)
plot(result)

result <- compare_props("titanic", "pclass", "survived", cp_plots = c("props","counts"))
summary(result)
plot(result)

foo <- function(x) x^2 %>% set_class(c("foo", class(.)))
foo(4)

sig_stars(c(.0009, .049, .009, .4, .09))


# getdata_exp <- function(dataset, vars, na.rm = TRUE, filt = "") {

#   # dat <- mtcars
#   # filt <- "mpg > 20\n"
#   # filt %<>% gsub("\\s","", .)
#   # dat %>% { if(filt == "") . else filter_(.,filt) }
#   # filt <- ""
#   # dat %>% { if(filt == "") . else filter_(.,filt) }
#   # dat[1,1] <- NA
#   # na.rm <- FALSE
#   # dat %>% { if(na.rm) na.omit(.) else .}
#   # na.rm <- TRUE
#   # dat %>% { if(na.rm) na.omit(.) else .}

#   filt %<>% gsub("\\s","", .)

# #   if(exists("session") && exists("r_env")) {
# #   if(exists("r_env") && !is.null(r_env$session)) {
#   if(exists("r_env")) {
#     # cat("Dataset", dataset, "loaded from the radiant environment (r_env)\n")
# #     select_(getdata(), .dots = vars) %>%
# #     select_(r_data[[dataset]], .dots = vars) %>%
#     select_(diamonds[1:1000,], .dots = vars) %>%
#       { if(na.rm) na.omit(.) else . }
# #   } else if(exists("r_env")) {
# #     cat("The requested dataset cannot be loaded from r_env. To make data from Radiant accesible in R(studio), stop the application using Quit > Quit in the navigation bar.\n") %>%
# #       stop %>% return
#   } else if(exists("r_data") && !is.null(r_data[[dataset]])) {
#     if(running_local) cat("Dataset", dataset, "loaded from r_data list\n")
#     select_(r_data[[dataset]], .dots = vars) %>%
#       { if(na.rm) na.omit(.) else . } %>%
#       { if(filt == "") . else filter_(.,filt) }
#   } else if(exists(dataset)) {
#     cat("Dataset", dataset, "loaded from global environment\n")
#     select_(get(dataset), .dots = vars) %>%
#       { if(na.rm) na.omit(.) else . } %>%
#       { if(filt == "") . else filter_(.,filt) }
#   } else {
#     paste0("Dataset ", dataset, " is not available. Please load the dataset and use the name in the function call") %>%
#       stop %>% return
#   }
# }

# test
# rm(list = ls())
# library(dplyr)
# library(magrittr)
# running_local <- TRUE
# mtcars_ <- mtcars
# getdata_exp("mtcars_", c("cyl","mpg"))
# r_data <- list()
# r_data$mtcars_ <- mtcars_
# r_data$mtcars_[5:20,2] <- NA
# getdata_exp("mtcars_", c("cyl","mpg"))
# getdata_exp("mtcars_", c("cyl","mpg"), filt = "mpg > 20")
# rm(mtcars_)
# getdata_exp("mtcars_", c("cyl","mpg"))
# getdata_exp("mtcars_", c("cyl","mpg"), filt = "mpg > 20")
# r_env <- new.env()
# session <- ""
# r_env$r_data <- r_data
# getdata_exp("mtcars_", c("cyl","mpg"))
# rm(r_data)
# getdata_exp("mtcars_", c("cyl","mpg"))
# rm(r_env)
# rm(session)
# getdata_exp("mtcars_", c("cyl","mpg"))
# getdata_exp("mycars", c("cyl","mpg"))
# getdata_exp("mtcars", c("cyl","mpg"), filt = "mpg > 20")
# end test


mac_launcher("base")
mac_launcher("quant")
mac_launcher()
win_launcher()
