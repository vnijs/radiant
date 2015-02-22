################################################################
# Regression - UI
################################################################
reg_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
reg_predict <- c("None" = "", "Data" = "data","Command" = "cmd")
reg_check <- c("RMSE" = "rmse", "Sum of squares" = "sumsquares",
               "VIF" = "vif", "Confidence intervals" = "confint",
               "Standardized coefficients" = "standardize",
               "Stepwise selection" = "stepwise")

reg_lines <- list("Line" = "line", "Loess" = "loess")
reg_plots <- list("None" = "", "Histograms" = "hist",
                  "Correlations" = "correlations", "Scatter" = "scatter",
                  "Dashboard" = "dashboard",
                  "Residual vs predictor" = "resid_pred",
                  "Coefficient plot" = "coef",
                  "Leverage plots" = "leverage")

reg_args <- as.list(formals(regression))

# list of function inputs selected by user
reg_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(reg_args))
    reg_args[[i]] <- input[[i]]
  if(!input$show_filter) reg_args$data_filter = ""
  reg_args
})

output$ui_reg_dep_var <- renderUI({
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  selectInput(inputId = "reg_dep_var", label = "Dependent variable:", choices = vars,
  	selected = state_single("reg_dep_var",vars), multiple = FALSE)
})

output$ui_reg_indep_var <- renderUI({
	notChar <- "character" != getdata_class()
  vars <- varnames()[notChar]
  if(input$reg_dep_var %>% not_available) vars <- character(0)
  if(length(vars) > 0 ) vars <- vars[-which(vars == input$reg_dep_var)]
  selectInput(inputId = "reg_indep_var", label = "Independent variables:", choices = vars,
    # the reference to reg_indep_var below should help ensure that variables
    # remain selected even if the dv changes
  	# selected = state_multiple("reg_indep_var", vars, isolate(input$reg_indep_var)),
  	selected = state_multiple("reg_indep_var", vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_reg_test_var <- renderUI({
	if(input$reg_indep_var %>% not_available) return()
  # vars <- varnames()
 	# vars <- vars[which(vars %in% input$reg_indep_var)]
  # vars <- varnames() %>% .[which(. %in% c("a","b"))]
    # vars <- c(vars,input$reg_int_var)
  vars <- input$reg_indep_var

  # adding interaction terms as needed
	if(!is.null(input$reg_int_var) && input$reg_interactions != '')
		vars <- c(input$reg_indep_var, input$reg_int_var)

  selectizeInput(inputId = "reg_test_var", label = "Variables to test:",
    choices = vars, selected = state_multiple("reg_test_var", vars),
    multiple = TRUE,
    options = list(placeholder = 'None', plugins = list('remove_button'))
  )
})

output$ui_reg_interactions <- renderUI({
	if(input$reg_indep_var %>% not_available) return()
 	sel <- ""
 	if(length(input$reg_indep_var) > 1)
 		sel <- state_init("reg_interactions")

  radioButtons(inputId = "reg_interactions", label = "Interactions:", reg_interactions,
  	selected = sel, inline = TRUE)
 })

# create vector of possible interaction terms to select

# int_vec <- function(reg_vars, nway) {
#   if(nway == "") return(character(0))
#   int_vec <- c()
#   for(i in 2:nway) {
#     int_vec %<>% {c(., combn(reg_vars, i) %>% apply( 2, paste, collapse = ":" ))}
#   }
#   int_vec
# }

output$ui_reg_int_var <- renderUI({
	if(input$reg_interactions %>% is_empty) return()
  vars <- input$reg_indep_var
	if(vars %>% not_available || length(vars) < 2) return()
 	choices <- int_vec(vars, input$reg_interactions)
	selectInput("reg_int_var", label = NULL, choices = choices,
  	selected = state_multiple("reg_int_var", choices),
  	multiple = TRUE, selectize = FALSE)
})

output$ui_regression <- renderUI({
  tagList(
  	wellPanel(
		  conditionalPanel(condition = "input.tabs_regression == 'Plot'",
		    selectInput("reg_plots", "Regression plots:", choices = reg_plots,
			  	selected = state_single("reg_plots", reg_plots)),
		  	conditionalPanel(condition = "input.reg_plots == 'coef'",
        	checkboxInput("reg_coef_int", "Include intercept", state_init("reg_coef_int", FALSE))
        ),
        checkboxGroupInput("reg_lines", NULL, reg_lines,
          selected = state_init("reg_lines"), inline = TRUE)
		  ),
	    uiOutput("ui_reg_dep_var"),
	    uiOutput("ui_reg_indep_var"),
			uiOutput("ui_reg_interactions"),
		  # radioButtons(inputId = "reg_interactions", label = "Interactions:", reg_interactions,
	   #  	selected = state_init("reg_interactions"),
	   #  	inline = TRUE),
		  conditionalPanel(condition = "input.reg_interactions != ''",
				uiOutput("ui_reg_int_var")
			),
		  conditionalPanel(condition = "input.tabs_regression == 'Summary'",
   	    radioButtons(inputId = "reg_predict", label = "Prediction:", reg_predict,
	      	selected = state_init("reg_predict", ""),
	      	inline = TRUE),

        conditionalPanel(condition = "input.reg_predict == 'cmd'",
          returnTextAreaInput("reg_predict_cmd", "Prediction command:",
	    		  value = state_init("reg_predict_cmd"))
        ),
        conditionalPanel(condition = "input.reg_predict == 'data'",
          selectizeInput(inputId = "reg_predict_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_init("reg_predict_data"), multiple = FALSE)
        ),
		    uiOutput("ui_reg_test_var"),
        checkboxGroupInput("reg_check", NULL, reg_check,
          selected = state_init("reg_check"), inline = TRUE)
			),
	    conditionalPanel(condition = "input.reg_predict != '' |
	                     input.reg_check.indexOf('confint') >= 0 |
	                     input.reg_plots == 'coef'",
 					 sliderInput("reg_conf_level", "Adjust confidence level:", min = 0.70,
 					             max = 0.99, value = state_init("reg_conf_level",.95),
 					             step = 0.01)
		  ), br(),
      HTML("<label>Save:</label>"), br(),
      actionButton("reg_saveres", "Residuals"),
      downloadButton("reg_savepred", "Predictions")
	  ),
  	help_and_report(modal_title = "Linear regression (OLS)",
  	                fun_name = "regression",
  	                help_file = inclRmd("../quant/tools/help/regression.Rmd"))
	)
})

reg_plot_width <- function() {
	.regression() %>%
  { if(class(.)[1] == "character") 650 else .$plot_width }
}

reg_plot_height <- function() {
  .regression() %>%
  { if(class(.)[1] == "character") 500 else .$plot_height }
}

# output is called from the main radiant ui.R
output$regression <- renderUI({

		register_print_output("summary_regression", ".regression")
		register_plot_output("plot_regression", ".regression",
                         height_fun = "reg_plot_height",
                         width_fun = "reg_plot_width")

		# two separate tabs
		reg_output_panels <- tabsetPanel(
	    id = "tabs_regression",
	    tabPanel("Summary", verbatimTextOutput("summary_regression")),
	    tabPanel("Plot", plotOutput("plot_regression", width = "100%", height = "100%"))
	  )

		statTabPanel2(menu = "Regression",
		              tool = "Linear (OLS)",
		              tool_ui = "ui_regression",
		             	output_panels = reg_output_panels)

})

.regression <- reactive({
	if(input$reg_dep_var %>% not_available)
		return("This analysis requires a dependent variable of type integer\nor numeric and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("diamonds"))

	if(input$reg_indep_var %>% not_available)
		return("Please select one or more independent variables.\n\n" %>% suggest_data("diamonds"))

	do.call(regression, reg_inputs())
})

observe({
  if(input$regression_report %>% not_pressed) return()
  isolate({
		outputs <- c("summary","plot")
  	if(reg_inputs()$reg_plots == "") outputs = c("summary")
		update_report(inp = clean_args(reg_inputs(), reg_args), fun_name = "regression",
		              outputs = outputs,
		              fig.width = round(7 * reg_plot_width()/650,2),
		              fig.height = round(7 * reg_plot_height()/500,2))
  })
})

observe({
	if(input$reg_saveres %>% not_pressed) return()
	isolate({
		result <- .regression()
		if(result %>% is.character) return()
		result$model$residuals %>% data.frame %>% changedata("residuals")
	})
})

output$reg_savepred <- downloadHandler(
  filename = function() { "reg_savepred.csv" },
  content = function(file) {
    summary(.regression(), savepred = TRUE) %>%
      write.csv(., file = file, row.names = FALSE)
  }
)
