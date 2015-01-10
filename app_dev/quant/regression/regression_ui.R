

# list of inputs
args_reg <- as.list(formals(regression))

inputs_reg <- reactive({

	#
	#
	# particularly for the observer for reporting, make sure to
	# remove elements from the list of inputs that are empty/missing
	# because this will keep things simpler in the call by report
	#
	#

  for(i in names(args_reg))
    args_reg[[i]] <- input[[i]]
  args_reg
})

# args_reg$what_ever --- use to set initial values for ui elements
# or should it be reg_args?




################################################################
# Regression
################################################################
output$uiReg_var1 <- renderUI({
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "reg_var1", label = "Dependent variable:", choices = vars,
  	selected = state_singlevar("reg_var1",vars), multiple = FALSE)
})

output$uiReg_var2 <- renderUI({
  if(is.null(input$reg_var1)) return()
	notChar <- "character" != getdata_class()
  vars <- varnames()[notChar]
 	vars <- vars[-which(vars == input$reg_var1)]
  if(length(vars) == 0) return()
  selectInput(inputId = "reg_var2", label = "Independent variables:", choices = vars,
    # the reference to reg_var2 below should help ensure that variables
    # remain selected even if the dv changes
  	selected = state_init_multvar("reg_var2", isolate(input$reg_var2),vars),
  	multiple = TRUE, selectize = FALSE)
 		# selected = state_multvar("reg_var2", vars), multiple = TRUE, selectize = FALSE)
})

output$uiReg_var3 <- renderUI({
  # if(is.null(input$reg_var2)) return()
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
  if(is.null(vars) || length(vars) < 2) return()
 	choices <- reg_int_vec(vars,input$reg_interactions)
	selectInput("reg_intsel", label = "", choices = choices,
  	selected = state_multvar("reg_intsel", vars), multiple = TRUE, selectize = FALSE)
})

reg_interactions <- c("None" = "none", "All 2-way" = "2way", "All 3-way" = "3way")
reg_pred_buttons <- c("Data" = "dataframe","Command" = "cmd")
output$ui_regression <- renderUI({
  list(
  	wellPanel(
	    uiOutput("uiReg_var1"),
	    uiOutput("uiReg_var2"),
		  radioButtons(inputId = "reg_interactions", label = "Interactions:", reg_interactions,
	    	selected = state_init_list("reg_interactions","none", reg_interactions)),
		  conditionalPanel(condition = "input.reg_interactions != 'none'",
				uiOutput("uiReg_intsel")
			),
		  conditionalPanel(condition = "input.tabs_regression == 'Summary'",
   	    radioButtons(inputId = "reg_predict_buttons", label = "Prediction:", reg_pred_buttons,
	      	selected = state_init_list("reg_predict_buttons","dataframe",reg_pred_buttons)),

        conditionalPanel(condition = "input.reg_predict_buttons == 'cmd'",
          returnTextInput("reg_predict", "Predict (e.g., carat = seq(.5,1,.05))",
	    		  value = state_init('reg_predict',''))
        ),
        conditionalPanel(condition = "input.reg_predict_buttons == 'dataframe'",
          selectInput(inputId = "reg_predict_data", label = "Predict for profiles:", choices = c("None" = "none",values$datasetlist),
            selected = state_init("reg_predict_data"), multiple = FALSE)
        ),

		    uiOutput("uiReg_var3"),
		    # checkboxInput(inputId = "reg_outlier", label = "Outlier test", value = FALSE),
        checkboxInput(inputId = "reg_rmse", label = "RMSE",
	    		value = state_init('reg_rmse',FALSE)),
        checkboxInput(inputId = "reg_sumsquares", label = "Sum of squares",
	    		value = state_init('reg_sumsquares',FALSE)), br(),
  	    checkboxInput(inputId = "reg_vif", label = "VIF",
	    		value = state_init('reg_vif',FALSE)),
        checkboxInput(inputId = "reg_confint", label = "Confidence intervals",
      		value = state_init('reg_rmse',FALSE)),
		    conditionalPanel(condition = "input.reg_confint == true | input.reg_predict_data != 'none' | input.reg_predict != ''",
           sliderInput('reg_conf_level',"", min = 0.70, max = 0.99,
                       value = state_init('reg_conf_level',.95), step = 0.01)
		    ),
        checkboxInput(inputId = "reg_standardize", label = "Standardized coefficients",
     		  value = state_init('reg_standardize',FALSE)),
        checkboxInput(inputId = "reg_stepwise", label = "Select variables step-wise",
      		value = state_init('reg_stepwise',FALSE))
			),
		  conditionalPanel(condition = "input.tabs_regression == 'Plots'",
		    selectInput("reg_plots", "Regression plots:", choices = r_plots,
			  	selected = state_init_list("reg_plots","", r_plots)),
		    checkboxInput('reg_line', 'Line', value = state_init("reg_line", FALSE)),
		    checkboxInput('reg_loess', 'Loess', value = state_init("reg_loess", FALSE))
		  ),
		  actionButton("saveres", "Save residuals")
	  ),
		helpAndReport('Regression','regression', inclRmd("../quant/tools/help/regression.Rmd"))
	)
})

reg_plotWidth <- function() {
	result <- .regression()
	ifelse(class(result) == 'lm', return(result$plotWidth), return(650))
}

reg_plotHeight <- function() {
	result <- .regression()
	ifelse(class(result) == 'lm', return(result$plotHeight), return(650))
}

output$regression <- renderUI({
	# create inputs and outputs - function in radiant.R
  statTabPanel("Regression","Linear (OLS)",".regression","regression", "reg_plotWidth", "reg_plotHeight")
})

.regression <- reactive({
	if(is.null(input$reg_standardize)) return("")

	ret_text <- "This analysis requires a dependent variable of type integer\nor numeric and one or more independent variables.\nPlease select another dataset."
	if(is.null(input$reg_var1)) return(ret_text)
	if(is.null(input$reg_var2)) return("Please select one or more independent variables.")

  if(is.null(inChecker(c(input$reg_var1, input$reg_var2)))) return()

	result <- regression(input$dataset, input$reg_var1, input$reg_var2, input$reg_var3, input$reg_intsel,
		input$reg_interactions, input$reg_predict_buttons, input$reg_predict, input$reg_predict_data, input$reg_standardize,
    input$reg_sumsquares, input$reg_confint, input$reg_conf_level, input$reg_rmse, input$reg_vif, input$reg_stepwise,
    input$reg_plots, input$reg_line, input$reg_loess)

	# specifying plot heights
	nrVars <- length(as.character(attr(result$terms,'variables'))[-1])

	result$plotHeight <- 650
	result$plotWidth <- 650

	if(input$reg_plots == 'histlist') result$plotHeight <- 325 * ceiling(nrVars / 2)

	if(input$reg_plots == 'dashboard') result$plotHeight <- 630 + 375

	if(input$reg_plots == 'correlations') {
		result$plotHeight <- 150 * nrVars
		result$plotWidth <- 150 * nrVars
	}

	if(input$reg_plots %in% c('scatterlist','leverage_plots','resid_vs_predictorlist')) {
		result$plotHeight <- 325 * ceiling((nrVars-1) / 2)
	}

	result
})

observe({
  if(is.null(input$regressionReport) || input$regressionReport == 0) return()
  isolate({
		inp <- list(input$dataset, input$reg_var1, input$reg_var2, input$reg_var3, input$reg_intsel,
			input$reg_interactions, input$reg_predict_buttons, input$reg_predict, input$reg_predict_data,
      input$reg_standardize, input$reg_sumsquares, input$reg_confint, input$reg_conf_level, input$reg_rmse,
      input$reg_vif, input$reg_stepwise, input$reg_plots, input$reg_line,
      input$reg_loess)

		#
		#
		# make sure to drop the arguments that are not used/missing/empty (string)
		# from the UI
		#
		#


		updateReport(inp,"regression", round(7 * reg_plotWidth()/650,2), round(7 * reg_plotHeight()/650,2))
  })
})
