# test exported function changedata
dat <- mtcars[1:10,]
x <- 1:nrow(dat)
changedata("dat", vars = x, var_names = "x")
dat
dat <- mtcars[1:10,]
x <- data.frame(z = 1:nrow(dat), q = letters[1:nrow(dat)])
names(x)
changedata("dat", vars = x, var_names = c("z","q"))
dat
dat <- mtcars[1:10,]
changedata("dat", vars = x)
dat

r_data$shopping
changedata("shopping", vars = 1:20, var_names = "x")
getdata("shopping")







# list of function classs - run and scan for errors
# todo: convert to testthat


# library(dplyr)
# library(magrittr)
# source("~/gh/radiant_dev/R/radiant.R")
# data <- "mtcars"
# hc_vars <- c("cyl", "mpg")
# hc_plots <- "dendo"
# result <- hierCluster(data, hc_vars, hc_plots = hc_plots)
# plots_hierCluster(result)

# Could use ggplot2 for dendrogram
# library(ggplot2)
# library(ggdendro)
# hc <- hclust(dist(USArrests))
# hcdata <- dendro_data(hc, type="rectangle")
# ggplot() +
#     geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
#     geom_text(data=label(hcdata), aes(x=x, y=y, label=label, hjust=0), size=3) +
#     coord_flip() + scale_y_reverse(expand=c(0.2, 0))

# ### demonstrate plotting directly from object class hclust
# ggdendrogram(hc, rotate=FALSE)
# ggdendrogram(hc, rotate=TRUE)
# ### demonstrate converting hclust to dendro using dendro_data first
# hcdata <- dendro_data(hc)
# ggdendrogram(hcdata, rotate=TRUE) + labs(title="Dendrogram in ggplot2")

cat(file = stderr(), file = "", file = stdout())

install.packages('rjson')
devtools::install_github("sdwfrost/epiwidgets")
library(epiwidgets)
install.packages('ape')
library(ape)

treewidget(
"(((Crotalus_oreganus_oreganus_cytochrome_b:0.00800,Crotalus_horridus_cytochrome_b:0.05866):0.04732,(Thamnophis_elegans_terrestris_cytochrome_b:0.00366,Thamnophis_atratus_cytochrome_b:0.00172):0.06255):0.00555,(Pituophis_catenifer_vertebralis_cytochrome_b:0.00552,Lampropeltis_getula_cytochrome_b:0.02035):0.05762,((Diadophis_punctatus_cytochrome_b:0.06486,Contia_tenuis_cytochrome_b:0.05342):0.01037,Hypsiglena_torquata_cytochrome_b:0.05346):0.00779);"
)

treewidget(
  as.phylo(hclust(dist(USArrests), "ave"))
)


# cat(file = stderr(), file = "", file = stdout())

library(shiny)
rv <- reactiveValues()
is.reactivevalues(rv)
rv$diamonds <- diamonds

library(pryr)
object_size(rv, diamonds)
object_size(rv)
object_size(diamonds)

isolate(rv$diamonds[1,1] <- NA)

isolate(rv$diamonds[1:10,])

sapply(mget(ls()), class) == 'data.frame'

cat(file = stderr(), file = "", file = stdout())
cat(file = stderr(), file = "", file = stdout())

cat(file = stderr(), file = "", file = stdout())

cat(file = stderr(), file = "", file = stdout())

df_list <- sapply(mget(ls(), .GlobalEnv), is.data.frame)
df_list[df_list]











###############################################
###############################################
###############################################
###############################################

source("~/gh/radiant_dev/R/single_mean.R")

input <- list()
sm_plots <- "hist"

result <- single_mean("mtcars", "mpg", sm_comp_value = 20, sm_alternative = "greater")
summary(result)
plot(result, sm_plots = "hist")



    # outputs <- inp_out <- character(0)
    # inp_out <- list(sm_plots = input$sm_plots) %>% c("",.)
    inp_out <- c("","")
    outputs <- c("summary","plot")
    if(length(input$sm_plots) == 0) {
      figs <- FALSE
      outputs <- c("summary")
    }

  inp_out <- c("","")
  inp_main <- as.list(formals(single_mean))
  fun_name <- "single_mean"
  pre_cmd <- "result <- "
  outputs <- c("summary","plot")

  cmd <- deparse(inp_main, control = c("keepNA"), width.cutoff = 500L) %>%
           sub("list", fun_name, .) %>%
           paste0(pre_cmd, .)

  lout <- length(outputs)
  if(lout > 0) {
    for(i in 1:lout) {
      if(inp_out[i] != "") {
        cmd <- deparse(inp_out[i], control = c("keepNA"), width.cutoff = 500L) %>%
                 sub("list\\(", paste0(outputs[i], "\\(result, "), .) %>%
                 paste0(cmd, "\n", .)
      } else {
        cmd <- paste0(cmd, "\n")
      }
    }
  }


 # cmd <- paste0(cmd, "\n", sub("list\\(",paste0(outputs[i], "\\(result, "),
        #               deparse(inp_out[i], control = c("keepNA"), width.cutoff = 500L)))


    update_report2(inp_main = clean_args(sm_inputs(), sm_args),

                  inp_out = inp_out,
                  fun_name = "single_mean",
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * sm_plot_width()/650,2),
                  fig.height = round(7 * sm_plot_height()/650,2))
  })
})

###############################################
###############################################
###############################################
###############################################






















setwd("~/Desktop/GitHub/radiant_dev/tests")
load("radiant.rda")
library(ggplot2)
library(broom)
library(dplyr)
library(magrittr)
library(tidyr)
library(lubridate)
library(gridExtra)
options(max.print = 300)
source("~/gh/radiant_dev/R/radiant.R")
rm(r_env)


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
getdata("dat")
getdata("dat", filt = "mpg > 20")
getdata("dat", slice = "n()-20:n()")
dat[1:10,1] <- NA
getdata("dat")
getdata("dat", na.rm = FALSE)

getdata("dat", c("cyl","mpg"))
r_data$dat <- dat
r_data$dat[5:20,2] <- NA
getdata("dat", c("cyl","mpg"))
getdata("dat", c("cyl","mpg"), filt = "vs == 1")

getdata("dat", c("cyl","mpg"), na.rm = FALSE, slice = "sample(1:10)")
getdata("dat", slice = "sample(1:10)")
getdata("dat", na.rm = FALSE, slice = "sample(n())[1:(n()/2)]")

r_env <- new.env()
r_env$r_data <- r_data
r_env$r_data$dat$mpg <- 1
getdata("dat", c("cyl","mpg"))
rm(r_env)
getdata("dat", c("cyl","mpg"))


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




library(ggplot2)
library(broom)
library(dplyr)
library(magrittr)
library(tidyr)
options(max.print = 300)
source("~/gh/radiant_dev/R/radiant.R")
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
