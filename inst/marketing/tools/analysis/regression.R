output$reg_var1 <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "reg_var1", label = "Dependent variable:", choices = vars, selected = NULL, multiple = FALSE)
})

output$reg_var2 <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "reg_var2", label = "Independent variables:", choices = vars[-which(vars == input$reg_var1)], selected = NULL, multiple = TRUE)
})

output$reg_var3 <- renderUI({
  vars <- input$reg_var2
  if(is.null(vars)) return()
	if(!is.null(input$reg_intsel) && input$reg_interactions != 'none') vars <- c(vars,input$reg_intsel)

  selectInput(inputId = "reg_var3", label = "Variables to test:", choices = vars, selected = NULL, multiple = TRUE)
})

r_plots <- list("Coefficient plot" = "coef", "Actual vs Fitted" = 0, "Residuals vs Fitted" = 1, "Normal Q-Q" = 2, "Scale-Location" = 3,
	"Cook's distance" = 4, "Residuals vs Leverage" = 5, "Cook's distance vs Leverage" = 6
)

plot.regression <- function(result) {
	mod <- fortify(result)
	if(input$reg_plots == "coef") {
		return(coefplot(result, xlab="", ylab="", main="Coefficient plot", col.pts="blue", CI=2))

	} else if(input$reg_plots == 0) {
		df <- data.frame(cbind(mod$.fitted,mod[1]))
		colnames(df) <- c("x","y")
		p <- ggplot(df, aes(x=x, y=y)) + geom_point() + stat_smooth(method="lm", se=TRUE) +
			labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual values"))

	} else if(input$reg_plots == 1) {
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

# create all the interaction terms
reg_int_vec <- function(reg_vars, nway) {
	n <- length(reg_vars)
	iway <- c()
	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			iway <- c(iway, paste(reg_vars[i],reg_vars[j],sep=":"))
		}
	}
	if(n >= 3 && nway == '3way') {
		for(i in 1:(n-2)) {
			for(j in (i+1):(n-1)) {
				for(k in (j+1):n) {
					iway <- c(iway, paste(reg_vars[i],reg_vars[j],reg_vars[k],sep=":"))
				}
			}
		}
	}
	iway
}

output$reg_intsel <- renderUI({
  vars <- input$reg_var2
  if(is.null(vars) || length(vars) < 2) return()
	selectInput("reg_intsel", label = "", choices = reg_int_vec(vars,input$reg_interactions), selected = NULL, multiple = TRUE)
})

ui_regression <- function() {
  wellPanel(
  	tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")),
    uiOutput("reg_var1"),
    uiOutput("reg_var2"),
  	checkboxInput(inputId = "reg_standardize", label = "Standardized coefficients", value = FALSE),
    radioButtons(inputId = "reg_interactions", label = "Interactions:", c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way"), selected = "None"),
    conditionalPanel(condition = "input.reg_interactions != 'none'",
  		uiOutput("reg_intsel")
  	),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    uiOutput("reg_var3"),
	    checkboxInput(inputId = "reg_vif", label = "Calculate VIF-values", value = FALSE),
  	  checkboxInput(inputId = "reg_stepwise", label = "Select variables step-wise", value = FALSE)
  	),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("reg_plots", "Regression plots:", choices = r_plots, selected = 'coef', multiple = FALSE)
    ),
    actionButton("saveres", "Save residuals")
  )
}

vif.regression <- function(result) {
	if(input$reg_vif) {
		if(length(input$reg_var2) > 1) {
	  	cat("Variance Inflation Factors\n")
	  	VIF <- as.matrix(vif(result))[,1]
	  	VIF <- sort(VIF, decreasing = TRUE)
			t(data.frame(VIF))
		} else {
	  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
		}
	}
}

test.regression <- function(result) {
	dat <- getdata()
	if(input$reg_standardize) dat <- data.frame(lapply(dat,rescale))

	sub_formula <- ". ~ 1"
	vars <- input$reg_var2
  if(!is.null(input$reg_intsel) && input$reg_interactions != 'none') vars <- c(vars,input$reg_intsel)
	not_selected <- setdiff(vars,input$reg_var3)
	if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))

	reg_sub <- update(result, sub_formula)
	anova(reg_sub, result, test='F')
}

summary.regression <- function(result) {
	print(summary(result), digits = 3)
	if(input$reg_vif) {
		print(vif.regression(result))
		cat("\n")
	} 
	if(!is.null(input$reg_var3)) {
		if(!input$reg_stepwise) {
			test.regression(result)
		} else {
	  	cat("Model comparisons are not conducted when Stepwise has been selected.\n")
	  }
	}
}

regression <- reactive({
	vars <- input$reg_var2
	if(is.null(vars)) return("Please select one or more independent variables")
	if(!is.null(input$reg_intsel) && input$reg_interactions != 'none') vars <- c(vars,input$reg_intsel)

	formula <- paste(input$reg_var1, "~", paste(vars, collapse = " + "))
	dat <- getdata()
	if(input$reg_standardize) dat <- data.frame(lapply(dat,rescale))
	if(input$reg_stepwise) {
		mod <- step(lm(as.formula(paste(input$reg_var1, "~ 1")), data = dat), scope = list(upper = formula), direction = 'forward')
	} else {
		mod <- lm(formula, data = dat)
	}
	mod
})

observe({
	if(is.null(input$saveres) || input$saveres == 0) return()
	isolate({
		changedata(regression()$residuals, "residuals")
	})
})

# out <- function(outputId = "summary") {
#   if(class(output[[outputId]]) == 'xtable') {
# 	  pre(id = outputId, class = "shiny-text-output")
# 	 } else {
# 	  div(id = outputId, class = "shiny-html-output")
#   }
# }

# output$plotswhich <- renderPlot({
# 	if(input$tool == 'dataview' || input$analysistabs != 'Plots') return()
# 	# 'traditional' residual plots for lm
# 	plot(regression(), which = as.integer(input$reg_plots))
# }, width=800, height=800)
