# UI-elements for GLM
# variable selection - GLM
output$glm_var1 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "glm_var1", label = "Dependent variable:", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection - GLM
output$glm_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "glm_var2", label = "Independent variables:", choices = vars[-which(vars == input$glm_var1)], selected = NULL, multiple = TRUE)
})

ui_regression <- function() {
  wellPanel(
    uiOutput("glm_var1"),
    uiOutput("glm_var2"),
    uiOutput("glm_var3"),
    checkboxInput(inputId = "glm_vif", label = "Calculate VIF-values", value = FALSE),
    checkboxInput(inputId = "glm_stepwise", label = "Select variables step-wise", value = FALSE),
    actionButton("saveres", "Save residuals")
  )
}

# analysis functions
summary.regression <- function(result) {
	print(summary(result))
	if(input$glm_vif) {
		if(!is.null(input$glm_var3)) {
			print(vif.regression(result))
			cat("\n")
			test.regression(result)
		} else {
			vif.regression(result)
		}
	} else if(!is.null(input$glm_var3)) {
		test.regression(result)
	}
}

plot.regression <- function(result) {
	par(mfrow = c(4,1))
	# dev.new(width=6, height=12)
	# par(mfrow = c(2,2))
	plot(result, ask = FALSE)
}

vif.regression <- function(result) {
	if(input$glm_vif) {
		if(length(result$coefficients) > 2) {
	  	cat("Variance Inflation Factors\n")
	  	VIF <- sort(vif(result), decreasing = TRUE)
			t(data.frame(VIF))
		} else {
	  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
		}
	}
}

test.regression <- function(result) {
	if(!input$glm_stepwise) {
		not_selected <- setdiff(input$glm_var2,input$glm_var3)
		if(length(not_selected) == 0) {
			sub_formula <- paste(input$glm_var1, "~ 1")
		} else {
			sub_formula <- paste(input$glm_var1, "~", paste(not_selected, collapse = " + "))
		}
		glm_sub <-lm(sub_formula, data = getdata())
		anova(glm_sub, result, test='F')
	} else {
	  cat("Model comparisons are not conducted when Stepwise estimation has been selected.\n")
	}
}

regression <- reactive(function() {
	if(is.null(input$glm_var2)) return("Please select one or more independent variables")
	formula <- paste(input$glm_var1, "~", paste(input$glm_var2, collapse = " + "))
	if(input$glm_stepwise) {
		step(lm(as.formula(paste(input$glm_var1, "~ 1")), data = getdata()), scope = list(upper = formula), direction = 'forward')
	} else {
		lm(formula, data = getdata())
	}
})

observe(function() {
	if(is.null(input$saveres) || input$saveres == 0) return()
	isolate(
		changedata(regression()$residuals, "residuals")
	)
})