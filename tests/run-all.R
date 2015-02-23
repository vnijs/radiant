# list of function classs - run and scan for errors
# todo: convert to testthat

setwd("~/Desktop/GitHub/radiant_dev/tests")
library(radiant)
# unload(inst("radiant"))

dat1 <- diamonds[1:100,]
dat2 <- diamonds[101:110,]
rm(r_data)
merge_data("dat1","dat2","price")
r_data <- list()
r_data$datasetlist <- "a"
merge_data("dat1","dat2","price")

# example data used in radiant
load("radiant.rda")
rm(r_env)

# has spaces in the variable names
result <- cross_tabs("newspaper","Income","Newspaper", ct_observed = TRUE, ct_expected = TRUE)
summary(result)
plot(result)

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

dat <- mtcars %>% mutate(cars = rownames(.)) %>% slice(1:10)
getdata_exp("dat")
getdata_exp("dat", filt = "mpg > 20")
getdata_exp("dat", slice = "n()-20:n()")
dat[1:10,1] <- NA
getdata_exp("dat")
getdata_exp("dat", na.rm = FALSE)

getdata_exp("dat", c("cyl","mpg"))
r_data$dat <- dat
r_data$dat[5:20,2] <- NA
getdata_exp("dat", c("cyl","mpg"))
getdata_exp("dat", c("cyl","mpg"), filt = "vs == 1")

getdata_exp("dat", c("cyl","mpg"), na.rm = FALSE, slice = "sample(1:10)")
getdata_exp("dat", slice = "sample(1:10)")
getdata_exp("dat", na.rm = FALSE, slice = "sample(n())[1:(n()/2)]")

r_env <- new.env()
r_env$r_data <- r_data
r_env$r_data$dat$mpg <- 1
getdata_exp("dat", c("cyl","mpg"))
rm(r_env)
getdata_exp("dat", c("cyl","mpg"))


mac_launcher("base")
mac_launcher("quant")
mac_launcher()
win_launcher()


# is local_dir is not in the library path (e.g., when usig brew in mac)
local_dir <- Sys.getenv("R_LIBS_USER")
if(!file.exists(local_dir)) {
	dir.create(local_dir, recursive = TRUE)
	.libPaths(local_dir)
}

local_dir <- .libPaths()[1]
global_dir <- .libPaths()[2]




# library(ggplot2)
# library(broom)
# library(dplyr)
# library(magrittr)
# library(tidyr)
# options(max.print = 300)
# source("~/gh/radiant_dev/R/radiant.R")
# load("~/Desktop/GitHub/radiant_dev/inst/marketing/data/data_examples/titanic.rda")

# with(titanic, expand.grid(survived = levels(survived), sex = levels(sex), age = 1:10))

# glm_plots <- "prob"
# glm_prob_vars <- "pclass"

# dat <- ggplot2::fortify(result$model)
# vars <- as.character(attr(result$model$terms,'variables'))[-1]
# glm_dep_var <- vars[1]
# glm_indep_var <- vars[-1]
# dat <- dat[,glm_indep_var, drop = FALSE]
# nval <- levels(dat[,glm_prob_vars]) %>% data.frame %>% set_colnames(glm_prob_vars)

# isFct <- sapply(dat, is.factor)
# isNum <- sapply(dat, is.numeric)

# newdat <- ""
# if(sum(isNum) > 0)  newdat <- data.frame(newdat,t(colMeans(dat[,isNum, drop = FALSE])))
# # from http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
# if(sum(isFct) > 0)  newdat <- data.frame(newdat,t(apply(dat[,isFct, drop = FALSE],2,function(x) names(which.max(table(x))))))
# newdat


# newdat[names(nval)] <- list(NULL)
# nnd <- data.frame(newdat[-1],nval)
# pred <- predict(result$model, nnd, type = 'response')
# nnd <- data.frame(nnd, pred, check.names = FALSE)


# titanic.est <- titanic %>%
#   filter(!age=='NA') %>%
#   mutate(age.f=cut(age,breaks=c(0,20,30,40,50,60,100)))

# dataset <- "titanic.est"
# data_filter <- ""
# glm_indep_var <- c("pclass","sex","age.f")
# glm_link <- "logit"
# glm_check <- "confint"
# glm_check <- "odds"
# glm_conf_level = .95
# glm_plots = "dashboard"
# glm_coef_int = TRUE
# result <- glm_reg(dataset, glm_dep_var, glm_indep_var, glm_test_var = glm_test_var)
# result <- glm_reg(dataset, glm_dep_var, glm_indep_var)



# test
# library(gridExtra)
# library(ggplot2)
# library(dplyr)
# diamonds <- diamonds[1:100,]
# result <- single_mean("diamonds","price", sm_plots = c("hist","simulate"))
# summary(result)
# result <- single_mean("diamonds","price", sm_plots = c("hist","simulate"),
#                       sm_alternative = "less")
# plot(result)
# result <- single_mean("diamonds","price", sm_plots = c("hist","simulate"),
#                       sm_alternative = "greater")
# plot(result)
# end test

# bootstrap using dplyr and broom NOT WORKING
# library(broom)
# select(mtcars, mpg) %>% bootstrap(10)
# mtcars %>% bootstrap(10) %>% summarize(mean(mpg))
# boot <- bootstrap(mtcars,10)

# mtcars %>%
# 	group_by(vs) %>%
# 	summarize(mean = mean(mpg))

# mtcars %>%
# 	bootstrap(10) %>%
# 	summarize(mean = mean(mpg))

# bootstrap(mtcars, 5) %>% summarize(min(mpg))

# select(mtcars,mpg) %>% sample(., length(.), replace = TRUE) %>% summarize(mean = mean(mpg))

# don't use the suggestion below. seems much slower than a simple loop
# library(broom)
# library(ggplot2)
# library(dplyr)

# nruns <- 1000

# system.time(
# 	simdat1 <- select(diamonds,price) %>% bootstrap(nruns) %>% do(data.frame(mean = mean(.$price)))
# )

# system.time({
# 	simdat2 <- matrix(0, nrow = nruns)
# 	for(i in 1:nrow(simdat)) {
# 		simdat2[i] <- diamonds$price %>%
# 									 sample(., length(.), replace = TRUE) %>%
# 									 mean
# 	}
# })

# simdat1
# simdat2[1:10]


# rm(list = ls())
# library(ggplot2)
# library(dplyr)
# library(broom)
# dataset <- "diamonds"
# single_prop("diamonds","clarity","VS")

# diamonds[1:10,"clarity"] %>% as.character %>% as.factor
# lapply(diamonds,class)
# x <- diamonds[,"clarity"] %>% as.character %>% as.factor
# class(x)
# diamonds$x

# %>% relevel(sp_levels)

# dat <- diamonds
# sp_var <- "clarity"
# sp_levels <- "VVS2"

# dat[,sp_var] %>% as.factor %>% relevel(sp_levels)
# dat[,sp_var] %>% as.character %>% as.factor %>% relevel(sp_levels)

# sp_levels <- "Fair"
# sp_comp_value = 0.05
# sp_alternative = "less"
# sp_sig_level = .95
# sp_plots = "hist"

# rm(list = ls())
# library(ggplot2)
# library(dplyr)
# library(broom)
# dataset <- "diamonds"
# single_prop("diamonds","clarity","VS")

# diamonds[1:10,"clarity"] %>% as.character %>% as.factor
# lapply(diamonds,class)
# x <- diamonds[,"clarity"] %>% as.character %>% as.factor
# class(x)
# diamonds$x

# %>% relevel(sp_levels)

# dat <- diamonds
# sp_var <- "clarity"
# sp_levels <- "VVS2"

# dat[,sp_var] %>% as.factor %>% relevel(sp_levels)
# dat[,sp_var] %>% as.character %>% as.factor %>% relevel(sp_levels)

# sp_levels <- "Fair"
# sp_comp_value = 0.05
# sp_alternative = "less"
# sp_sig_level = .95
# sp_plots = "hist"

# library(gridExtra)
# library(ggplot2)
# dat <- diamonds[1:1000,]
# viz_vars1 <- c("price","carat","cut")
# viz_type <- "hist"
# visualize("diamonds", viz_vars1, viz_type = "hist")
