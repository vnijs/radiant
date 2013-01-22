summary.regression <- function(result) {
	summary(result)
}

plot.regression <- function(result) {
	par(mfrow = c(2,2))
	plot(result, ask = FALSE)
}

extra.regression <- function(result) {
	if(input$reg_vif) {
		if(length(result$coefficients) > 2) {
	  	cat("Variance Inflation Factors\n")
	  	VIF <- sort(vif(result), decreasing = TRUE)
			data.frame(VIF)
		} else {
	  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
		}
	} else {
	  	cat("No extra's selected\n")
	}
}

regression <- reactive(function() {
	if(is.null(input$reg_var2)) return("Please select one or more independent variables")
	formula <- paste(input$reg_var1, "~", paste(input$reg_var2, collapse = " + "))
	lm(formula, data = getdata())
})

observe(function() {
	if(is.null(input$saveres) || input$saveres == 0) return()
	isolate(
		changedata(regression()$residuals, "residuals")
	)
})