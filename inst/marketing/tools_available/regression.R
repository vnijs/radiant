# UI-elements for regression

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

# analysis functions
summary.regression <- function(result) {
	print(summary(result))
	if(input$reg_vif) {
		if(!is.null(input$reg_var3)) {
			print(vif.regression(result))
			cat("\n")
			test.regression(result)
		} else {
			vif.regression(result)
		}
	} else if(!is.null(input$reg_var3)) {
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
	if(input$reg_vif) {
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
	if(!input$reg_stepwise) {
		not_selected <- setdiff(input$reg_var2,input$reg_var3)
		if(length(not_selected) == 0) {
			sub_formula <- paste(input$reg_var1, "~ 1")
		} else {
			sub_formula <- paste(input$reg_var1, "~", paste(not_selected, collapse = " + "))
		}
		reg_sub <-lm(sub_formula, data = getdata())
		anova(reg_sub, result, test='F')
	} else {
	  cat("Model comparisons are not conducted when Stepwise estimation has been selected.\n")
	}
}

regression <- reactive(function() {
	if(is.null(input$reg_var2)) return("Please select one or more independent variables")
	formula <- paste(input$reg_var1, "~", paste(input$reg_var2, collapse = " + "))
	if(input$reg_stepwise) {
		step(lm(as.formula(paste(input$reg_var1, "~ 1")), data = getdata()), scope = list(upper = formula), direction = 'forward')
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