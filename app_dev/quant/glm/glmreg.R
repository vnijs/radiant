################################################################
# Generalize Linear Models
################################################################

output$uiGlm_var1 <- renderUI({
	isFct <- "factor" == getdata_class()
 	vars <- varnames()[isFct]
 	if(length(vars) == 0) return()
  selectInput(inputId = "glm_var1", label = "Dependent variable:", choices = vars,
   	selected = names(vars[vars == values$glm_var1]), multiple = FALSE)
})

output$uiGlm_var2 <- renderUI({
	vars <- varnames()
 	vars <- vars[-which(vars == input$glm_var1)]
  if(length(vars) == 0) return()
  selectInput(inputId = "glm_var2", label = "Independent variables:", choices = vars,
   	selected = names(vars[vars %in% values$glm_var2]), multiple = TRUE)
})

output$uiGlm_var3 <- renderUI({

  vars <- input$glm_var2
  if(is.null(vars)) return()
	if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)
  selectInput(inputId = "glm_var3", label = "Variables to test:", choices = vars,
   	selected = values$glm_var2, multiple = TRUE)
})

output$uiGlm_intsel <- renderUI({
  vars <- input$glm_var2
  if(is.null(vars) || length(vars) < 2) return()
 	if(vars %in% varnames()) vars <- glm_int_vec(vars,input$glm_interactions)

	selectInput("glm_intsel", label = "", choices = vars,
   	selected = values$glm_intsel, multiple = TRUE)
})

glm_linkfunc <- c("Logit" = "logit", "Probit" = "probit")
glm_interactions <- c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way")

ui_glmreg <- function() {
  list(wellPanel(
    radioButtons(inputId = "glm_linkfunc", label = "", glm_linkfunc,
    	selected = values$glm_linkfunc),
    # radioButtons(inputId = "glm_glmtype", label = "", c("GLM" = "glm", "Bayes GLM" = "bayesglm"), selected = "GLM"),
    uiOutput("uiGlm_var1"),
    uiOutput("uiGlm_var2"),
 	  checkboxInput(inputId = "glm_standardize", label = "Standardized coefficients",
 	  	value = values$glm_standardize),
    radioButtons(inputId = "glm_interactions", label = "Interactions:", glm_interactions,
    	selected = values$glm_interactions),
    conditionalPanel(condition = "input.glm_interactions != 'none'",
  		uiOutput("uiGlm_intsel")
  	),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    uiOutput("uiGlm_var3")
  	),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("glm_plots", "Plots:", choices = g_plots,
      	selected = values$glm_plots, multiple = FALSE)
    ),
    actionButton("saveglmres", "Save residuals")
  	),
		helpModal('Generalized Linear Models (GLM)','glmreg',includeHTML("tools/help/glmreg.html"))
	)
}

# main functions called from radyant.R
summary.glmreg <- function(result) {
	print(summary(result), digits = 2)
	print(test.glmreg(result))

	if(!is.null(input$glm_var3)) {
		cat("\n")
		test.glmreg(result, FALSE)
	}
}

g_plots <- list("None" = "", "Coefficient plot" = "coef", "Actual vs Fitted" = 0, "Residuals vs Fitted" = 1, "Normal Q-Q" = 2, "Scale-Location" = 3,
	 "Cook's distance" = 4, "Residuals vs Leverage" = 5, "Cook's distance vs Leverage" = 6)

# r_plots <- list("None" = "", "Histograms" = "histlist", "Correlations" = "correlations", "Scatter" = "scatterlist", "Dashboard" = "dashboard",
		# "Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots", "Coefficient plot" = "coef")


plot.glmreg <- function(result) {

	mod <- fortify(result)
	mod$.fitted <- predict(result, type = 'response')
	mod$.actual <- as.numeric(mod[,1])
	mod$.actual <- mod$.actual - max(mod$.actual) + 1 	# adjustment in case max > 1

	if(input$glm_plots == "coef") {
		return(coefplot(result, xlab="", ylab="", main="Coefficient plot", col.pts="blue", CI=2))

	} else if(input$glm_plots == 0) {
		p <- ggplot(mod, aes(x=.fitted, y=.actual)) + geom_point() + stat_smooth(method="glm", family="binomial", se=TRUE) +
			geom_jitter(position = position_jitter(height = .05)) + labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual"))

	} else if(input$glm_plots == 1) {
		p <- qplot(.fitted, .resid, data = mod) + geom_hline(yintercept = 0) + geom_smooth(se = FALSE) +
			labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))

	} else if(input$glm_plots == 2) {
		p <- qplot(sample =.stdresid, data = mod, stat = "qq") +
			geom_abline() +
			labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))

	} else if(input$glm_plots == 3) {
		p <- qplot(.fitted, sqrt(abs(.stdresid)), data = mod) + geom_smooth(se = FALSE) +
			labs(list(title = "Scale-Location", x = "Fitted values", y = "Sqrt. standardized residuals"))

	} else if(input$glm_plots == 4) {

		p <- qplot(seq_along(.cooksd), .cooksd, data = mod, geom = "bar", stat="identity") +
			labs(list(title = "Cook's distance", x = "Observation number", y = "Cook's distance"))

	} else if(input$glm_plots == 5) {
		p <- qplot(.hat, .stdresid, data = mod, size = .cooksd) + geom_smooth(se = FALSE, size = 0.5) +
			labs(list(title = "Residuals vs Leverage", x = "Leverage", y = "Standardize residuals", size = "Cook's distance"))

	} else if(input$glm_plots == 6) {
		p <- ggplot(mod, aes(.hat, .cooksd)) + geom_vline(xintercept = 0, colour = NA) +
			geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") + geom_smooth(se = FALSE) +
			geom_point() + labs(list(title = "Cook's distance vs Leverage", x = "Leverage", y = "Cook's distance"))
		 	# p <- qplot(.hat, .cooksd, size = .cooksd / .hat, data = mod) + scale_area()
	}

	print(p)
}

# analysis reactive
glmreg <- reactive({

	ret_text <- "This analysis requires a dependent variable of type factor\nand one or more independent variables.\nPlease select another dataset."
	if(is.null(input$glm_var1)) return(ret_text)
	vars <- input$glm_var2
	if(is.null(vars)) return("Please select one or more independent variables.")
	if(is.null(inChecker(c(input$glm_var1, vars)))) return(ret_text)

	# adding interaction terms as needed
	if(input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)

	dat <- getdata()
	dv <- which(colnames(dat) == input$glm_var1)
	if(input$glm_standardize) dat[,-dv] <- data.frame(lapply(dat[,-dv, drop = FALSE],rescale))

	formula <- paste(input$glm_var1, "~", paste(vars, collapse = " + "))
	# if(input$glm_glmtype == "bayesglm") {
	# 	mod <- bayesglm(formula, family = binomial(link = input$glm_linkfunc), data = dat)
	# } else {
	# 	mod <- glm(formula, family = binomial(link = input$glm_linkfunc), data = dat)
	# }

	mod <- glm(formula, family = binomial(link = input$glm_linkfunc), data = dat)
	mod
})

# save residuals
observe({
	if(is.null(input$saveglmres) || input$saveglmres == 0) return()
	isolate({
		if(is.character(glmreg())) return()
		changedata(data.frame(residuals(glmreg())), "residuals")
 	})
})

# additional functions
glm_int_vec <- function(glm_vars, nway) {
	# create all the interaction terms. might be better to use expand.grid (Blake's suggestion to include x1^2 and x2^2 as well)
	# do it if you can get select2 working for tools

	n <- length(glm_vars)
	iway <- c()
	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			iway <- c(iway, paste(glm_vars[i],glm_vars[j],sep=":"))
		}
	}
	if(n >= 3 && nway == '3way') {
		for(i in 1:(n-2)) {
			for(j in (i+1):(n-1)) {
				for(k in (j+1):n) {
					iway <- c(iway, paste(glm_vars[i],glm_vars[j],glm_vars[k],sep=":"))
				}
			}
		}
	}
	iway
}

test.glmreg <- function(result, modeleval = TRUE) {

	dat <- getdata()
	dv <- which(colnames(dat) == input$glm_var1)
	if(input$glm_standardize) dat[,-dv] <- data.frame(lapply(dat[,-dv, drop = FALSE],rescale))

	sub_formula <- ". ~ 1"
	if(modeleval == FALSE) {
		vars <- input$glm_var2
		if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)
		not_selected <- setdiff(vars,input$glm_var3)
		if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))
	}

	glm_sub <- update(result, sub_formula)
	anova(glm_sub, result, test='Chi')
}
