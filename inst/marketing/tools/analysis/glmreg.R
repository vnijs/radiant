# variable selection - GLM
output$glm_var1 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  isFct <- sapply(getdata(), is.factor)
 	vars <- vars[isFct]
 	if(length(vars) == 0) return("Dataset has no factors that can be selected.")
  selectInput(inputId = "glm_var1", label = "Dependent variable:", choices = vars, selected = NULL, multiple = FALSE)
})

output$glm_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "glm_var2", label = "Independent variables:", choices = vars[-which(vars == input$glm_var1)], selected = NULL, multiple = TRUE)
})

output$glm_var3 <- reactiveUI(function() {
  vars <- input$glm_var2
  if(is.null(vars)) return()
	if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)
  selectInput(inputId = "glm_var3", label = "Variables to test:", choices = vars, selected = NULL, multiple = TRUE)
})

gplots <- list("Coefficient plot" = "coef", "Residuals vs Fitted" = 1, "Normal Q-Q" = 2, "Scale-Location" = 3,
	"Cook's distance" = 4, "Residuals vs Leverage" = 5, "Cook's distance vs Leverage" = 6)

plot.glmreg <- function(result) {

	if(input$glm_plots != "coef") {
		plot(result, which = as.integer(input$glm_plots))
	} else {
		coefplot(result, xlab="", ylab="", main="Coefficient plot", col.pts="blue", CI=2)
	}
}

# create all the interaction terms
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

output$glm_intsel <- reactiveUI(function() {
  vars <- input$glm_var2
  if(is.null(vars) || length(vars) < 2) return()
	selectInput("glm_intsel", label = "", choices = glm_int_vec(vars,input$glm_interactions), selected = NULL, multiple = TRUE)
})

ui_glmreg <- function() {
  wellPanel(
  	tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")),
    radioButtons(inputId = "glm_linkfunc", label = "", c("Logit" = "logit", "Probit" = "probit"), selected = "Logit"),
    radioButtons(inputId = "glm_glmtype", label = "", c("GLM" = "glm", "Bayes GLM" = "bayesglm"), selected = "GLM"),
    uiOutput("glm_var1"),
    uiOutput("glm_var2"),
 	  checkboxInput(inputId = "glm_standardize", label = "Standardized coefficients", value = FALSE),
    radioButtons(inputId = "glm_interactions", label = "Interactions:", c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way"), selected = "None"),
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

# analysis functions
summary.glmreg <- function(result) {
	print(summary(result), digits = 2)

	vars <- input$glm_var2
	if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)

	modsig <- wald.test(b = coef(result), Sigma = vcov(result), Terms = 1:length(vars))
	cat("Model ")
	print(modsig)

	if(!is.null(input$glm_var3)) {
		cat("\n\nCoefficient ")
		wald.test(b = coef(result), Sigma = vcov(result), Terms = match(input$glm_var3,vars))
	}
}

glmreg <- reactive(function() {

	vars <- input$glm_var2
	if(is.null(vars)) return("Please select one or more independent variables")
	if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)
	formula <- paste(input$glm_var1, "~", paste(vars, collapse = " + "))

	dat <- getdata()
	if(input$glm_standardize) dat <- data.frame(lapply(dat,rescale))
	if(input$glm_glmtype == "bayesglm") {
		mod <- bayesglm(formula, family = binomial(link = input$glm_linkfunc), data = dat)
	} else {
		mod <- glm(formula, family = binomial(link = input$glm_linkfunc), data = dat)
	}
})

observe(function() {
	if(is.null(input$saveglmres) || input$saveglmres == 0) return()
	isolate(
		changedata(glmreg()$residuals, "residuals")
	)
})