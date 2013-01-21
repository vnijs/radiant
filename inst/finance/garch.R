main.garch <- function() {

	dat = getdata()[,input$var1]
	if(input$logdat) dat <- log(dat)

	formula <- as.formula(paste("~ garch(",input$order1,",",input$order2,")"))
	title <- paste("GARCH(",input$order1,",",input$order2,") model for variable \"", input$var1,"\"",sep ="")
	result <- garchFit(formula, title = title, data = dat, include.mean=input$inclmean, cond.dist = input$cond.dist, trace=FALSE)
	result
 
}

summary.garch <- function(result) {
	summary(result)
}

plot.garch <- function(result) {
   plot(result, which = as.integer(input$plots))
}

extra.garch <- function(result) {
	cat("Nothing here yet\n")
}

garch <- reactive(function() {
	if(is.null(input$var1)) return("Please select a variable for analysis")
	main.garch()
})

# UI controls
cond.dist = c("norm", "snorm", "ged", "sged", "std", "sstd", "snig", "QMLE")

# plots <- list("Time SeriesPlot" = 1, "Conditional Standard Deviation Plot" = 2,
#   "Series Plot with 2 Conditional SD Superimposed" = 3, "Autocorrelation function Plot of Observations" = 4,
#   "Autocorrelation function Plot of Squared Observations" = 5, "Cross Correlation Plot" = 6,
#   "Residuals Plot" = 7, "Conditional Standard Deviations Plot" = 8,
#   "Standardized Residuals Plot" = 9, "ACF Plot of Standardized Residuals" = 10,
#   "ACF Plot of Squared Standardized Residuals" = 11, "Cross Correlation Plot between $r^2$ and r" = 12,
#   "Quantile-Quantile Plot of Standardized Residuals" = 13)

plots <- list("Standardized Residuals Plot" = 9, "ACF Plot of Standardized Residuals" = 10,
  "ACF Plot of Squared Standardized Residuals" = 11, "Cross Correlation Plot between $r^2$ and r" = 12,
  "Quantile-Quantile Plot of Standardized Residuals" = 13)


controlsFor_garch <- function() {
  wellPanel(
    uiOutput("var1"),
    selectInput("cond.dist", "Conditional distribution:", choices = cond.dist),
    numericInput("order1", "Order 1:", 1, min = 1),
    numericInput("order2", "Order 2:", 0, min = 0),
    checkboxInput("logdat", "Log data", TRUE),
    checkboxInput("inclmean", "Include mean", FALSE),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("plots", "Diagnostic plots:", choices = plots, selected = 1, multiple = FALSE)
    )
  )
}