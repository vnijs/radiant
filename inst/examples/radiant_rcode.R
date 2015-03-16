library(radiant)

# analysis on built-in dataset mtcars
result <- single_mean("mtcars", "mpg", sm_comp_value = 20, sm_alternative = "greater")
summary(result)
plot(result)

result <- compare_means("mtcars", "cyl", "mpg")
summary(result)
plot(result)

result <- compare_means("diamonds", "cut", "price")
summary(result)
plot(result)

# in radiant load Examples > Load examples from the Data > Manage tab
radiant()

# this result will differ from the previous one because the data from radiant is used which includes only a subset of all diamond prices for illustration purposes
result <- compare_means("diamonds", "cut", "price")
summary(result)
plot(result)

# if the salary data was loaded in radiant you should get output here
result <- compare_means("salary", "rank", "salary")
summary(result)
plot(result)

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

# set working directory to home
setwd("~/Desktop")
# or
# setwd("~/Documents/Desktop")

# get the data from the link below and save it to your desktop
browseURL("https://github.com/mostly-harmless/radiant/blob/master/inst/marketing/data/shopping.rda?raw=true")

# load example dataset, if the file is not found make sure your working directory
# is set to Desktop
load("shopping.rda")

# load the radiant library
library(radiant)

# start with hierarchical clustering, view help
?hier_clus

# run hierarchical cluster analysis on the shopping data, variables v1 through v6
result <- hier_clus("shopping", c("v1:v6"))

# summary - not much here - plots are more important
summary(result)

# check the help file on how to plot results from hierarchical cluster analysis
?plot.hier_clus

# default plots
plot(result)

# show the dendrogram
plot(result, hc_plots = "dendro")

# show the dendrogram with cutoff at 0
plot(result, hc_plots = "dendro", hc_cutoff = 0)

# see if you can reproduce this output in the radiant gui
# the shopping data should be loaded from the global environment
# note that radiant runs in a browser so when you want to load data it
# will look on the client-side (i.e., on your local computer)
# to get data from the server into the radiant gui load it into
# the global environment using 'load' as illustrated above and then
# start radiant using 'radiant()'
# you may want to allow pop-ups from the rsm server to avoid the warning
# message you are about to see
radiant()

# if you use Quit > Quit to stop radiant the app will dump the current state
# into the global environment
# if you start radiant again it will read the state information and
# you can continue where you left-off. To reset to a clean state go to Quit
# and hit the reset button







