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

plots <- list("Residuals vs Fitted" = 1, "Normal Q-Q" = 2, "Scale-Location" = 3,
	"Cook's distance" = 4, "Residuals vs Leverage" = 5, "Cook's distance vs Leverage" = 6
)

plot.regression <- function(result) {
	mod <- fortify(result)
	# plot(result, ask = FALSE)

  if(input$reg_plots == 1) {
		p <- qplot(.fitted, .resid, data = mod) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE) +
			labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))

	} else if(input$reg_plots == 2) {
		p <- qplot(sample =.stdresid, data = mod, stat = "qq") +
			geom_abline() + 
			labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))

	} else if(input$reg_plots == 3) {
		p <- qplot(.fitted, sqrt(abs(.stdresid)), data = mod) + geom_smooth(se = FALSE) +
			labs(list(title = "Scale-Location", x = "Fitted values", y = "Sqrt. standardized residuals"))

	} else if(input$reg_plots == 4) {

		p <- qplot(seq_along(.cooksd), .cooksd, data = mod, geom = "bar", stat="identity") +
			labs(list(title = "Cook's distance", x = "Observation number", y = "Cook's distance"))

	} else if(input$reg_plots == 5) {
		p <- qplot(.hat, .stdresid, data = mod, size = .cooksd) + geom_smooth(se = FALSE, size = 0.5) +
			labs(list(title = "Residuals vs Leverage", x = "Leverage", y = "Standardize residuals", size = "Cook's distance"))

	} else if(input$reg_plots == 6) {
		p <- ggplot(mod, aes(.hat, .cooksd)) + geom_vline(xintercept = 0, colour = NA) +
			geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") + geom_smooth(se = FALSE) +
			geom_point() + labs(list(title = "Cook's distance vs Leverage", x = "Leverage", y = "Cook's distance"))
		 	# p <- qplot(.hat, .cooksd, size = .cooksd / .hat, data = mod) + scale_area()
	}

	print(p)
}


ui_regression <- function() {
  wellPanel(
    uiOutput("reg_var1"),
    uiOutput("reg_var2"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    uiOutput("reg_var3"),
	    checkboxInput(inputId = "reg_vif", label = "Calculate VIF-values", value = FALSE),
  	  checkboxInput(inputId = "reg_stepwise", label = "Select variables step-wise", value = FALSE)
  	),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("reg_plots", "Diagnostic plots:", choices = plots, selected = 1, multiple = FALSE)
    ),
    actionButton("saveres", "Save residuals")
  )
}

output$plotswhich <- reactivePlot(function() {
	if(input$tool == 'dataview' || input$analysistabs != 'Plots') return()
	# 'traditional' residual plots for lm
	plot(regression(), which = as.integer(input$reg_plots))
}, width=800, height=800)

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
		mod <- step(lm(as.formula(paste(input$reg_var1, "~ 1")), data = getdata()), scope = list(upper = formula), direction = 'forward')
	} else {
		mod <- lm(formula, data = getdata())
	}
})

observe(function() {
	if(is.null(input$saveres) || input$saveres == 0) return()
	isolate(
		changedata(regression()$residuals, "residuals")
	)
})