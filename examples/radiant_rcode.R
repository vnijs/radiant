library(radiant)

## analysis on built-in dataset mtcars
result <- single_mean(mtcars, "mpg", comp_value = 20, alternative = "greater")
summary(result)
plot(result)

result <- compare_means(mtcars, "cyl", "mpg")
summary(result)
plot(result)

result <- compare_means(diamonds, "cut", "price")
summary(result)
plot(result)

## regression
result <- regress(diamonds, "price", c("carat","clarity"))
summary(result)
plot(result, plots = "coef")
plot(result, plots = "dashboard")

## get help for the regression function
?regress

## set working directory to Desktop - checking two possible locations
path2desktop <- "~/Desktop/"
if (!file.exists(path2desktop))
	path2desktop <- "~/Documents/Desktop"
setwd(path2desktop)

## load the data from  url
shopping <- loadrda_url("https://github.com/radiant-rstats/radiant.multivariate/blob/master/data/shopping.rda?raw=true")

## start with hierarchical clustering, view help
?hier_clus

## run hierarchical cluster analysis on the shopping data, variables v1 through v6
result <- hier_clus(shopping, "v1:v6")

## summary - not much here - plots are more important
summary(result)

## check the help file on how to plot results from hierarchical cluster analysis
?plot.hier_clus

## default plots
plot(result)

## show the dendrogram
plot(result, plots = "dendro")

## show the dendrogram with cutoff at 0
plot(result, plots = "dendro", cutoff = 0)

## there seems to be three clusters (segments)
result <- kmeans_clus(shopping, vars = "v1:v6", nr_clus = 3)
summary(result)
plot(result)

## to add a variable with segment membership information to the shopping dataset
shopping <- store(result)

## did that work? there should be a column 'kclus3' in the dataset
head(shopping)

## to save the average shopping attitude scores for each segment
write.csv(result$clus_means, file = "kmeans.csv")

## the following command should open the created csv file in Excel
browseURL("kmeans.csv")

## see if you can reproduce this output in radiant's web-interface
## the shopping data should be loaded from the global environment
radiant()

## if you stop radiant by clicking the power icon and then refresh the app
## will put the the current state into the global environment
## if you start radiant again it will read the state and you can continue
## where you left-off. To reset to a clean state click the power icon and
## then click refresh
