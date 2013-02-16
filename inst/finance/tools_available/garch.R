# UI controls
cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd", "snig", "QMLE")

plots <- list("Standardized Residuals Plot" = 9, "ACF Plot of Standardized Residuals" = 10,
  "ACF Plot of Squared Standardized Residuals" = 11, "Cross Correlation Plot between $r^2$ and r" = 12,
  "Quantile-Quantile Plot of Standardized Residuals" = 13)

ui_garch <- function() {
  wellPanel(
    selectInput("ga_var", "Time-series:", choices = varnames(), selected = NULL, multiple = FALSE),
    selectInput("ga_cond.dist", "Conditional distribution:", choices = cond.dist),
    numericInput("ga_order1", "Order 1:", 1, min = 1),
    numericInput("ga_order2", "Order 2:", 0, min = 0),
    checkboxInput("ga_logdat", "Log data", TRUE),
    checkboxInput("ga_inclmean", "Include mean", FALSE),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("ga_plots", "Diagnostic plots:", choices = plots, selected = 1, multiple = FALSE)
    )
  )
}

# analysis
summary.garch <- function(result) {
	summary(result)
}

plot.garch <- function(result) {
   plot(result, which = as.integer(input$ga_plots))
}

garch <- reactive({
	if(is.null(input$ga_var)) return("Please select a variable for analysis")

  dat = getdata()[,input$ga_var]
  if(input$ga_logdat) dat <- log(dat)

  formula <- as.formula(paste("~ garch(",input$ga_order1,",",input$ga_order2,")"))
  title <- paste("GARCH(",input$ga_order1,",",input$ga_order2,") model for variable \"", input$ga_var,"\"",sep ="")
  garchFit(formula, title = title, data = dat, include.mean=input$ga_inclmean, cond.dist = input$ga_cond.dist, trace=FALSE)
  
})
