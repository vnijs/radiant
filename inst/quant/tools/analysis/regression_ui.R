
reg_interactions <- c("None" = "none", "2-way" = "2way", "3-way" = "3way")
reg_predict <- c("None" = "none", "Data" = "data","Command" = "cmd")
reg_check <- c("RMSE" = "rmse", "Sum of squares" = "sumsquares",
               "VIF" = "vif", "Confidence intervals" = "confint",
               "Standardized coefficients" = "standardize",
               "Stepwise selection" = "stepwise")

reg_lines <- list("Line" = "line", "Loess" = "loess")
reg_plots <- list("None" = "", "Histograms" = "histlist", "Correlations" = "correlations", "Scatter" = "scatterlist", "Dashboard" = "dashboard",
		"Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots")
		#"Coefficient plot" = "coef")

################################################################
# Regression
################################################################
output$uiReg_var1 <- renderUI({
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  selectInput(inputId = "reg_var1", label = "Dependent variable:", choices = vars,
  	selected = state_singlevar("reg_var1",vars), multiple = FALSE)
})

output$uiReg_var2 <- renderUI({
	notChar <- "character" != getdata_class()
  vars <- varnames()[notChar]
  if(length(vars) > 0 ) vars <- vars[-which(vars == input$reg_var1)]
  selectInput(inputId = "reg_var2", label = "Independent variables:", choices = vars,
    # the reference to reg_var2 below should help ensure that variables
    # remain selected even if the dv changes
  	selected = state_init_multvar("reg_var2", isolate(input$reg_var2),vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$uiReg_var3 <- renderUI({
	if(input$reg_var2 %>% not_available) return()
  vars <- varnames()
 	vars <- vars[which(vars %in% input$reg_var2)]

  # adding interaction terms as needed
	if(!is.null(input$reg_intsel) && input$reg_interactions != 'none')
		vars <- c(vars,input$reg_intsel)

	# use selectize for reg_var3 or not?
	# selectInput(inputId = "reg_var3", label = "Variables to test:", choices = vars,
  selectizeInput(inputId = "reg_var3", label = "Variables to test:", choices = vars,
  	selected = state_multvar("reg_var3", vars), multiple = TRUE,
    options = list(placeholder = 'None', plugins = list('remove_button'))
  )
})

output$uiReg_intsel <- renderUI({
  vars <- input$reg_var2
	if(vars %>% not_available || length(vars) < 2) return()
 	choices <- reg_int_vec(vars, input$reg_interactions)
	selectInput("reg_intsel", label = NULL, choices = choices,
  	selected = state_multvar("reg_intsel", vars), multiple = TRUE, selectize = FALSE)
})


output$ui_regression <- renderUI({
  tagList(
  	wellPanel(
		  conditionalPanel(condition = "input.tabs_regression == 'Plots'",
		    selectInput("reg_plots", "Regression plots:", choices = reg_plots,
			  	selected = state_init_list("reg_plots","", reg_plots)),

        checkboxGroupInput("reg_lines", NULL, reg_lines,
          selected = state_init_list("reg_lines","", reg_lines), inline = TRUE)
		    # checkboxInput('reg_line', 'Line', value = state_init("reg_line", FALSE)),
		    # checkboxInput('reg_loess', 'Loess', value = state_init("reg_loess", FALSE))
		  ),
	    uiOutput("uiReg_var1"),
	    uiOutput("uiReg_var2"),
		  radioButtons(inputId = "reg_interactions", label = "Interactions:", reg_interactions,
	    	selected = state_init_list("reg_interactions","none", reg_interactions),
	    	inline = TRUE),
		  conditionalPanel(condition = "input.reg_interactions != 'none'",
				uiOutput("uiReg_intsel")
			),
		  conditionalPanel(condition = "input.tabs_regression == 'Summary'",
   	    radioButtons(inputId = "reg_predict", label = "Prediction:", reg_predict,
	      	selected = state_init_list("reg_predict", "none", reg_predict),
	      	inline = TRUE),

        conditionalPanel(condition = "input.reg_predict == 'cmd'",
          returnTextInput("reg_predict_cmd", "Predict (e.g., carat = seq(.5,1,.05))",
	    		  value = state_init('reg_predict_cmd',''))
        ),
        conditionalPanel(condition = "input.reg_predict == 'data'",
          selectizeInput(inputId = "reg_predict_data", label = "Predict for profiles:",
                      choices = c("None" = "none",r_data$datasetlist),
                      selected = state_init("reg_predict_data"), multiple = FALSE)
        ),
		    uiOutput("uiReg_var3"),
        checkboxGroupInput("reg_check", NULL, reg_check,
          selected = state_init_list("reg_check","", reg_check), inline = TRUE),
		    # conditionalPanel(condition = "input.reg_confint == true | input.reg_predict != 'none'",
		    # conditionalPanel(condition = "input.reg_predict != 'none'",
		    # conditionalPanel(condition = "input.reg_check.contains('confint')",
		    conditionalPanel(condition = "input.reg_predict != 'none' |
		                                  input.reg_check.indexOf('confint') >= 0",
           sliderInput('reg_conf_level',"Adjust confidence level:", min = 0.70, max = 0.99,
                       value = state_init('reg_conf_level',.95), step = 0.01)
		    )
			),
		  actionButton("saveres", "Save residuals")
	  ),
		helpAndReport('Regression','regression', inclRmd("../quant/tools/help/regression.Rmd"))
	)
})

reg_plotWidth <- function() {
	result <- .regression()
	ifelse(class(result$mod) == 'lm', return(result$plotWidth), return(650))
}

reg_plotHeight <- function() {
	result <- .regression()
	ifelse(class(result$mod) == 'lm', return(result$plotHeight), return(650))
}

output$regression <- renderUI({
	# create inputs and outputs - function in radiant.R
  statTabPanel("Regression","Linear (OLS)",".regression","regression", "reg_plotWidth", "reg_plotHeight")
})

# save residuals
observe({
	if(input$saveres %>% not_pressed) return()
	isolate({
		result <- .regression()
		if(result %>% is.character) return()
		saveRegResiduals(result)
	})
})

reg_args <- as.list(formals(regression))

# list of function inputs selected by user
reg_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(reg_args))
    reg_args[[i]] <- input[[i]]
  if(!input$show_filter) reg_args$data_filter = ""
  reg_args
})

.regression <- reactive({
	if(input$reg_var1 %>% not_available)
		return("This analysis requires a dependent variable of type integer\nor numeric and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("diamonds"))

	if(input$reg_var2 %>% not_available)
		return("Please select one or more independent variables.\n\n" %>% suggest_data("diamonds"))

	do.call(regression, reg_inputs())
})

observe({
  if(input$regressionReport %>% not_pressed) return()
  isolate({
		updateReport(reg_inputs() %>% clean_args, "regression",
		             round(7 * reg_plotWidth()/650,2), round(7 * reg_plotHeight()/650,2))
  })
})
