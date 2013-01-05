library(shiny)
library(car)
library(tools)
library(foreign)
library(ggplot2)

# variables are in global.R so they will be available in both ui.R and server.R
# from server.R
# Labels for variable selectors
labels1 <- c("X-variable", "Dependent variable","Group variable","")
labels2 <- c("Y-variable", "Independent variables","Variables","")
labtools <- c("dataView", "regression", "compareMeans","hclustering")
names(labels1) <- names(labels2) <- labtools

# from ui.R
toolChoices <- list("Data view" = "dataView", "Regression" = "regression", "Compare means" = "compareMeans", "Hierarchical clustering" = "hclustering", "Kmeans clustering" = "kmeansClustering")

# depChoices <- c("visualize", "regression", "compareMeans")
interdepChoices <- c("hclustering","kmeansClustering")
notInAnd <- paste("input.tool != '",interdepChoices,"'", sep = "", collapse = " && ")
inOr <- paste("input.tool == '",interdepChoices,"'", sep = "", collapse = " || ")