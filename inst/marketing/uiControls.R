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
    uiOutput("reg_var3"),
    checkboxInput(inputId = "reg_vif", label = "Calculate VIF-values", value = FALSE),
    checkboxInput(inputId = "reg_stepwise", label = "Select variables step-wise", value = FALSE),
    actionButton("saveres", "Save residuals")
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
    numericInput("km_seed", "Set random seed:", 1234, min = 0),
    selectInput(inputId = "km_nrClus", label = "Number of clusters", choices = 2:20, selected = NULL, multiple = FALSE),
    actionButton("km_saveclus", "Save cluster membership")
  )
}
