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
# then quit the app using Quit > Quit
radiant()

# this result will differ from the previous one because the data from radiant
# is used which includes only a subset of all diamond prices for illustration
# purposes
# IF YOU GET AN ERROR HERE - run the previous command, and follow the
# instructions in the comment
result <- compare_means("diamonds", "cut", "price")
summary(result)
plot(result, cm_plots = "box")

# if the salary data was loaded in radiant you should get output here
result <- compare_means("salary", "rank", "salary")
summary(result)
plot(result, cm_plots = c("bar","density"))

# should produce an error because no dataset with that name exists
result <- try(compare_means("no_dat", "cyl", "mpg"), silent = TRUE)
if(is(result, 'try-error')) print("Error produced as expected")

# regression
result <- regression("diamonds", "price", c("carat","clarity"))
summary(result)
plot(result, reg_plot = "coef")
result <- regression("diamonds", "price", c("carat","clarity"))
plot(result, reg_plot = c("dashboard"))

# get help for the regression function
?regression

# set working directory to home
path2desktop <- "~/Desktop/"
if(!file.exists(path2desktop))
	path2desktop <- "~/Documents/Desktop"
setwd(path2desktop)

# get the data from the link below and save it to your desktop
browseURL("https://github.com/vnijs/radiant/blob/master/inst/marketing/data/shopping.rda?raw=true")

# load example dataset, if the file is not found make sure your working directory
# is set to Desktop
load("shopping.rda")

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

# there seems to be three clusters (segments)
result <- kmeans_clus("shopping", km_vars = "v1:v6", km_nr_clus = 3)
summary(result)
plot(result)

# to add a variable with segment membership information to the shopping dataset
save_membership(result)

# did that work? there should be a column 'kclus3' in the dataset
getdata("shopping") %>% head

# to save the average shopping attitude scores for each segment
write.csv(result$clus_means, file = "kmeans.csv")

# the following command should open the created csv file in Excel
browseURL("kmeans.csv")

# see if you can reproduce this output in radiant's web-interface
# the shopping data should be loaded from the global environment
radiant()

# if you use Quit > Quit to stop radiant the app will dump the current state
# into the global environment if you start radiant again it will read the state
# information and you can continue where you left-off. To reset to a clean state
# go to Quit and hit the reset button

# to unload radiant ...
# detach("package:radiant", unload=TRUE)
