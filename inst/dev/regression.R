main.regression <- function(state) {
	formula <- paste(state$var1, "~", paste(state$var2, collapse = " + "))
	lm(formula, data = getdata())
}

summary.regression <- function(result) {
	summary(result)
}

plot.regression <- function(result) {
	par(mfrow = c(2,2))
	plot(result, ask = FALSE)
}

extra.regression <- function(result) {
	if(length(result$coefficients) > 2) {
  	cat("Variance Inflation Factors\n")
  	VIF <- sort(vif(result), decreasing = TRUE)
		data.frame(VIF)
	} else {
  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
	}
}

regression <- reactive(function() {
	if(is.null(input$var2)) return("Please select one or more independent variables")
	main.regression(as.list(input))
})

observe(function() {
	if (input$saveres == 0) return()
	isolate(
		changedata(regression()$residuals, "residuals")
	)
})