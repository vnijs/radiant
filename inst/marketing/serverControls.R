# variable selection - singleMean
output$sm_var <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "sm_var", label = "Variable (select one):", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection - compareMeans
output$cm_var1 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  isFct <- sapply(getdata(), is.factor)
  vars <- vars[isFct]
  selectInput(inputId = "cm_var1", label = "Group variable (only factors shown):", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection - compareMeans
output$cm_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "cm_var2", label = "Variables (select one):", choices = vars[-which(vars == input$cm_var1)], selected = NULL, multiple = TRUE)
})

# variable selection - regression
output$reg_var1 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "reg_var1", label = "Dependent variable:", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection - compareMeans
output$reg_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "reg_var2", label = "Independent variables:", choices = vars[-which(vars == input$reg_var1)], selected = NULL, multiple = TRUE)
})

# variable selection - compareMeans
output$reg_var3 <- reactiveUI(function() {
  vars <- input$reg_var2
  if(is.null(vars)) return()
  selectInput(inputId = "reg_var3", label = "Variables to test:", choices = vars, selected = NULL, multiple = TRUE)
})

# variable selection - hclustering
output$hc_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})

output$km_vars <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "km_vars", label = "Variables:", choices = vars, selected = NULL, multiple = TRUE)
})