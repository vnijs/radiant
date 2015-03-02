################################################################
# Regression - UI
################################################################
reg_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
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
  if(!input$show_filter) reg_args$data_filter <- ""
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
  	selected = state_multiple("reg_indep_var", vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

# adding interaction terms as needed
output$ui_reg_test_var <- renderUI({
  vars <- input$reg_indep_var
	# if(!is.null(input$reg_int_var) && input$reg_show_interactions != '')
  if(!is.null(input$reg_int_var)) vars <- c(input$reg_indep_var, input$reg_int_var)

  selectizeInput(inputId = "reg_test_var", label = "Variables to test:",
    choices = vars, selected = state_multiple("reg_test_var", vars),
    multiple = TRUE,
    options = list(placeholder = 'None', plugins = list('remove_button'))
  )
})

output$ui_reg_show_interactions <- renderUI({
  if (length(input$reg_indep_var) == 2)
    choices <- reg_show_interactions[1:2]
  else if (length(input$reg_indep_var) > 2)
    choices <- reg_show_interactions
  else
    choices <- reg_show_interactions[1]

  radioButtons(inputId = "reg_show_interactions", label = "Interactions:",
               choices = choices,
               selected = state_init("reg_show_interactions"), inline = TRUE)
 })

output$ui_reg_int_var <- renderUI({
	# if(input$reg_show_interactions %>% is_empty) return()
  if(is_empty(input$reg_show_interactions)) {
    choices <- character(0)
  } else {
    vars <- input$reg_indep_var
    if(not_available(vars) || length(vars) < 2) return()
    choices <- int_vec(vars, input$reg_show_interactions)       # create list of interactions to show user
  }
	selectInput("reg_int_var", label = NULL, choices = choices,
  	selected = state_multiple("reg_int_var", choices),
  	multiple = TRUE, size = min(4,length(choices)), selectize = FALSE)
})

# create vector of possible interaction terms to select
# calling function from glm_reg
# int_vec <- function(reg_vars, nway) {
#   if(nway == "") return(character(0))
#   int_vec <- c()
#   for(i in 2:nway) {
#     int_vec %<>% {c(., combn(reg_vars, i) %>% apply( 2, paste, collapse = ":" ))}
#   }
#   int_vec
# }

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

      conditionalPanel(condition = "input.reg_indep_var != null",

  			uiOutput("ui_reg_show_interactions"),
  		  # conditionalPanel(condition = "output.ui_reg_show_interactions != null & input.reg_show_interactions != ''",
        conditionalPanel(condition = "input.reg_show_interactions != ''",
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
        conditionalPanel(condition = "input.reg_predict == 'cmd' |
                         input.reg_predict == 'data' |
  	                     input.reg_check.indexOf('confint') >= 0 |
  	                     input.reg_plots == 'coef'",
   					 sliderInput("reg_conf_level", "Adjust confidence level:", min = 0.70,
   					             max = 0.99, value = state_init("reg_conf_level",.95),
   					             step = 0.01)
  		  ), br(),
        HTML("<label>Save:</label>"), br(),
        actionButton("reg_saveres", "Residuals"),
        downloadButton("reg_savepred", "Predictions")
      )
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

# cleaning up the arguments for data_filter passed to report
clean_args2 <- function(rep_args, rep_default = list()) {
  if(rep_args$data_filter == "")
    rep_args$data_filter  <- NULL
  else
    rep_args$data_filter %<>% gsub("\\n","", .) %>% gsub("\"","\'",.)

  if(length(rep_default) == 0) rep_default[names(rep_args)] <- ""

  # removing default arguments before sending to report feature
  for(i in names(rep_args))
    if(rep_args[[i]][1] == rep_default[[i]]) rep_args[[i]] <- NULL
  rep_args
}

.regression <- reactive({
	if(input$reg_dep_var %>% not_available)
		return("This analysis requires a dependent variable of type integer\nor numeric and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("diamonds"))

	if(input$reg_indep_var %>% not_available)
		return("Please select one or more independent variables.\n\n" %>% suggest_data("diamonds"))

	do.call(regression, reg_inputs())
})

# observe({
#   # if(!is_empty(input$reg_show_interactions)) return()
#   if(is_empty(input$reg_show_interactions)) {
#     isolate({
#       # if(is_empty(input$reg_int_var)) return()
#       # if(length(input$reg_int_var) == 0) return()
#       updateSelectInput(session = session, inputId = "reg_int_var", selected = character(0))
#       # inp <- reg_inputs()
#       # if(is_empty(input$reg_show_interactions)) reg_args$reg_int_var <- ""
#     })
#   }
# })

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
