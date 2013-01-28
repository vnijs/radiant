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
	# cdplot(input$glm_var1 ~ input$glm_var2[1], data = getdata())
}


ui_glmreg <- function() {
  wellPanel(
  	tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")),
    radioButtons(inputId = "glm_linkfunc", label = "", c("Logit" = "logit", "Probit" = "probit"), selected = "Logit"),
    radioButtons(inputId = "glm_glmtype", label = "", c("GLM" = "glm", "Bayes GLM" = "bayesglm"), selected = "GLM"),
    uiOutput("glm_var1"),
    uiOutput("glm_var2"),
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

	# cat(paste("Log-likelihood:",round(logLik(result),3)))
	modsig <- wald.test(b = coef(result), Sigma = vcov(result), Terms = 1:length(input$glm_var2))
	# cat(paste("\nChange in deviance:",round(c_deviance,3),"  Change in df:",c_df,"\n"))
	cat("Model ")
	print(modsig)

	# print(wald.test(b = coef(result), Sigma = vcov(result), Terms = 1:length(input$glm_var2))$result)

	if(!is.null(input$glm_var3)) {
		cat("\n\nCoefficient ")
		wald.test(b = coef(result), Sigma = vcov(result), Terms = match(input$glm_var3,input$glm_var2))
	}
}

glmreg <- reactive(function() {
	if(is.null(input$glm_var2)) return("Please select one or more independent variables")
	formula <- paste(input$glm_var1, "~", paste(input$glm_var2, collapse = " + "))
	if(input$glm_glmtype == "bayesglm") {
		mod <- bayesglm(formula, family = binomial(link = input$glm_linkfunc), data = getdata())
	} else {
		mod <- glm(formula, family = binomial(link = input$glm_linkfunc), data = getdata())
	}
})

observe(function() {
	if(is.null(input$saveglmres) || input$saveglmres == 0) return()
	isolate(
		changedata(glmreg()$residuals, "residuals")
	)
})