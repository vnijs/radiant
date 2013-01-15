library(shiny)
library(shinyIncubator)
library(car)
library(tools)
library(foreign)
library(ggplot2)
library(gridExtra)

# variables are in global.R so they will be available in both ui.R and server.R
# from server.R
# Labels for variable selectors
labels1 <- c("Dependent variable","Group variable (only factors shown)")
labels2 <- c("Independent variables","Variables (select one)")
labtools <- c("regression", "compareMeans")
names(labels1) <- names(labels2) <- labtools

# labels for output$addvariable
addvarlabel <- c("Save residuals (see Data view)", "Save cluster membership (see Data view)")
labtools <- c("regression", "kmeansClustering")
names(addvarlabel) <- labtools

# from ui.R
toolChoices <- list("Data view" = "dataview", "Compare means" = "compareMeans", "Regression" = "regression", "Hierarchical clustering" = "hclustering", "Kmeans clustering" = "kmeansClustering")

depChoices <- c("visualize", "regression", "compareMeans")
interdepChoices <- c("hclustering","kmeansClustering")
notInAnd <- paste("input.tool != '",interdepChoices,"'", sep = "", collapse = " && ")
inOr <- paste("input.tool == '",interdepChoices,"'", sep = "", collapse = " || ")

# there are two tools sofar that can change the data used
# there will be more in the future
toolsThatCanChangeData <- c("regression", "kmeansClustering")
inOrChange <- paste("input.tool == '",toolsThatCanChangeData,"'", sep = "", collapse = " || ")
# inOrChange <- paste(inOrChange,"&& input.analysistabs == 'Summary'")
