labels1 <- c("Dependent variable","Variable (select one)", "Group variable (only factors shown)")
labels2 <- c("Independent variables","", "Variables (select one)")
labtools <- c("regression", "singleMean", "compareMeans")
names(labels1) <- names(labels2) <- labtools

# labels for output$addvariable
addvarlabel <- c("Save residuals (see Data view)", "Save cluster membership (see Data view)")
labtools <- c("regression", "kmeansClustering")
names(addvarlabel) <- labtools

# from ui.R
# toolChoices <- list("Data view" = "dataview", 
#                     "EDAT - Single mean" = "singleMean", 
#                     "EDAT - Compare means" = "compareMeans", 
#                     "Regression - Linear" = "regression", 
#                     "Regression - Logistic (ud)" = "logistic", 
#                     "Clustering - Hierarchical" = "hclustering", 
#                     "Clustering - Kmeans" = "kmeansClustering",
#                     "Product maps - MDS (ud)" = "mds",
#                     "Product maps - Perceptual map (ud)" = "perceptualMap",
#                     "Conjoint - Design (ud)" = "conjointDesign",
#                     "Conjoint - Analysis (ud)" = "conjointAnalysis"
#                     )

# depChoices <- c("visualize", "regression", "singleMean", "compareMeans")
interdepChoices <- c("hclustering","kmeansClustering")
notInInterdep <- paste("input.tool != '",interdepChoices,"'", sep = "", collapse = " && ")
inInterdep <- paste("input.tool == '",interdepChoices,"'", sep = "", collapse = " || ")

singleVar <- c("singleMean")
notInSingle <- paste("input.tool != '",singleVar,"'", sep = "", collapse = " && ")
inSingle  <- paste("input.tool == '",singleVar,"'", sep = "", collapse = " || ")


# there are two tools sofar that can change the data used
# there will be more in the future
toolsThatCanChangeData <- c("regression", "kmeansClustering")
inOrChange <- paste("input.tool == '",toolsThatCanChangeData,"'", sep = "", collapse = " || ")

# for alternative hypothesis
alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

ui_singleMean <- function() {
  wellPanel(
    uiOutput("sm_var"),
    selectInput(inputId = "sm_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
    sliderInput('sm_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01),
    numericInput("sm_compValue", "Comparison value:", 0)
  )
}

ui_compareMeans <- function() {
  wellPanel(
    uiOutput("cm_var1"),
    uiOutput("cm_var2"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
      selectInput(inputId = "cm_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
      sliderInput('cm_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01)
    )
  )
}

ui_regression <- function() {
  wellPanel(
    uiOutput("reg_var1"),
    uiOutput("reg_var2"),
    checkboxInput(inputId = "reg_vif", label = "Calculate VIF-values", value = FALSE),
    actionButton("saveres", "Save residuals:")
  )
}

ui_hclustering <- function() {
  wellPanel(
    uiOutput("hc_vars"), tags$style(type='text/css', "#hc_vars { height: 250px; padding-bottom: 35px;}"),
    selectInput(inputId = "hc_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("hc_saveclus", "Save cluster membership:")
  )
}

ui_kmeansClustering <- function() {
  wellPanel(
    uiOutput("km_vars"), tags$style(type='text/css', "#km_vars { height: 250px; padding-bottom: 35px;}"),
    # set.seed(1234)
    numericInput("km_seed", "Set random seed:", 1234, min = 0),
    selectInput(inputId = "km_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("km_saveclus", "Save cluster membership:")
  )
}





# output$varinterdep <- reactiveUI(function() {
#   vars <- varnames()
#   if(is.null(vars)) return()
#   selectInput(inputId = "varinterdep", label = "Variables", choices = vars, selected = NULL, multiple = TRUE)
# })

# # dropdown used to select the number of clusters to create
# output$nrClus <- reactiveUI(function() {
#   selectInput(inputId = "nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE)
# })