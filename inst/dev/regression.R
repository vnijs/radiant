main.regression <- function(state) {
	formula <- paste(state$var1, "~", paste(state$var2, collapse = " + "))
	result <- lm(formula, data = getdata())
	reg_residuals()
 	result
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
	isolate({
		result <- regression()
		var.name <- "residuals"
		changedata(result$residuals, var.name)
	})
})