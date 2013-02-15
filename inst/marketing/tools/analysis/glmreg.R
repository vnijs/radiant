# variable selection - GLM
output$glm_var1 <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  isFct <- sapply(getdata(), is.factor)
 	vars <- vars[isFct]
 	if(length(vars) == 0) return("Dataset has no factors that can be selected.")
  selectInput(inputId = "glm_var1", label = "Dependent variable:", choices = vars, selected = NULL, multiple = FALSE)
})

output$glm_var2 <- renderUI({
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "glm_var2", label = "Independent variables:", choices = vars[-which(vars == input$glm_var1)], selected = NULL, multiple = TRUE)
})

output$glm_var3 <- renderUI({
  vars <- input$glm_var2
  if(is.null(vars)) return()
	if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)
  selectInput(inputId = "glm_var3", label = "Variables to test:", choices = vars, selected = NULL, multiple = TRUE)
})

gplots <- list("Coefficient plot" = "coef", "Actual vs Fitted" = 0, "Residuals vs Fitted" = 1, "Normal Q-Q" = 2, "Scale-Location" = 3,
	 "Cook's distance" = 4, "Residuals vs Leverage" = 5, "Cook's distance vs Leverage" = 6)


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

# create all the interaction terms. might be better to use expand.grid (Blake's suggestion to include x1^2 and x2^2 as well)
# do it if you can get select2 working for tools
glm_int_vec <- function(glm_vars, nway) {
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

output$glm_intsel <- renderUI({
  vars <- input$glm_var2
  if(is.null(vars) || length(vars) < 2) return()
	selectInput("glm_intsel", label = "", choices = glm_int_vec(vars,input$glm_interactions), selected = NULL, multiple = TRUE)
})

ui_glmreg <- function() {
  wellPanel(
  	# includeHTML("www/js/tools.js"),
    radioButtons(inputId = "glm_linkfunc", label = "", c("Logit" = "logit", "Probit" = "probit"), selected = "Logit"),
    radioButtons(inputId = "glm_glmtype", label = "", c("GLM" = "glm", "Bayes GLM" = "bayesglm"), selected = "GLM"),
    uiOutput("glm_var1"),
    # HTML("<link href=\"js/select2/select2.css\" rel=\"stylesheet\"/> <script src=\"js/select2/select2.js\"></script> <script> $(document).ready(function() { $(\"#glm_var2\").select2({ minimumInputLength: 2, placeholder: \"Select interactions\", width: 'resolve' }); }); </script>"),
    uiOutput("glm_var2"),
 	  checkboxInput(inputId = "glm_standardize", label = "Standardized coefficients", value = FALSE),
    radioButtons(inputId = "glm_interactions", label = "Interactions:", c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way"), selected = "None"),
  	# tags$head(tags$script(src = "www/js/tools.js")),
    conditionalPanel(condition = "input.glm_interactions != 'none'",
  		uiOutput("glm_intsel") 
  	),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    uiOutput("glm_var3")
  	),
    conditionalPanel(condition = "input.analysistabs == 'Plots'",
      selectInput("glm_plots", "Plots:", choices = gplots, selected = "coef", multiple = FALSE)
    )
    # actionButton("saveglmres", "Save residuals")
  )
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

# analysis functions
summary.glmreg <- function(result) {
	print(summary(result), digits = 2)
	print(test.glmreg(result))

	if(!is.null(input$glm_var3)) {
		cat("\n")
		test.glmreg(result, FALSE)
	}
}

glmreg <- reactive({

	vars <- input$glm_var2
	if(is.null(vars)) return("Please select one or more independent variables")
	if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)
	formula <- paste(input$glm_var1, "~", paste(vars, collapse = " + "))

	dat <- getdata()
	dv <- which(colnames(dat) == input$glm_var1)
	if(input$glm_standardize) dat[,-dv] <- data.frame(lapply(dat[,-dv, drop = FALSE],rescale))

	if(input$glm_glmtype == "bayesglm") {
		mod <- bayesglm(formula, family = binomial(link = input$glm_linkfunc), data = dat)
	} else {
		mod <- glm(formula, family = binomial(link = input$glm_linkfunc), data = dat)
	}
	mod
})

observe({
	if(is.null(input$saveglmres) || input$saveglmres == 0) return()
	isolate(
		changedata(residuals(glmreg()), "residuals")
 	)
})
