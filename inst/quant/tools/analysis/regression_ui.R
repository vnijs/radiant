################################################################
# Regression - UI
################################################################
reg_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
reg_predict <- c("None" = "", "Data" = "data","Command" = "cmd")
reg_check <- c("Standardized coefficients" = "standardize",
               "Stepwise selection" = "stepwise")

reg_sum_check <- c("RMSE" = "rmse", "Sum of squares" = "sumsquares",
               "VIF" = "vif", "Confidence intervals" = "confint")

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

# need the ::: because summary is an S3 method and not an exported function
reg_sum_args <- as.list(formals(radiant:::summary.regression))

# list of function inputs selected by user
reg_sum_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(reg_sum_args))
    reg_sum_args[[i]] <- input[[i]]
  reg_sum_args
})

# need the ::: because plot is an S3 method and not an exported function
reg_plot_args <- as.list(formals(radiant:::plot.regression))

# list of function inputs selected by user
reg_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(reg_plot_args))
    reg_plot_args[[i]] <- input[[i]]
  reg_plot_args
})

# need the ::: because plot is an S3 method and not an exported function
reg_pred_args <- as.list(formals(radiant:::predict.regression))
# reg_pred_args <- list()

# list of function inputs selected by user
reg_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(reg_pred_args))
    reg_pred_args[[i]] <- input[[i]]

  reg_pred_args$reg_predict_cmd <- reg_pred_args$reg_predict_data <- ""
  if(input$reg_predict == "cmd")
    reg_pred_args$reg_predict_cmd <- gsub('\\s', '', input$reg_predict_cmd)

  if(input$reg_predict == "data")
    reg_pred_args$reg_predict_data <- input$reg_predict_data

  # reg_pred_args$reg_conf_level <- input$reg_conf_level

  # if(not_pressed(input$reg_save_pred))
  #   reg_pred_args$reg_save_pred <- FALSE
  # else
  #   reg_pred_args$reg_save_pred <- TRUE

  reg_pred_args
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
    choices = vars, selected = state_multiple("reg_test_var", vars, ""),
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


# SAVE PREDICTIONS TO EXCEL
# REPORT
# USE INPUT LIST
# RENAME getdata TO GETDATA AND changedata TO CHANGEDATA



# create vector of possible interaction terms to select
# using function from glm_reg
# int_vec <- function(reg_vars, nway) {
#   if(nway == "") return(character(0))
#   int_vec <- c()
#   for(i in 2:nway) {
#     int_vec %<>% {c(., combn(reg_vars, i) %>% apply( 2, paste, collapse = ":" ))}
#   }
#   int_vec
# }

output$ui_regression <- renderUI({
#   cp_reg_conf_level <- "input.reg_predict == 'cmd' | input.reg_predict == 'data' |  (input.reg_sum_check !== undefined & input.reg_sum_check.indexOf('confint') >= 0) | input.reg_plots == 'coef'"
#   cp_reg_conf_level <- "input.reg_predict == 'cmd' | input.reg_predict == 'data' |  try{input.reg_sum_check.indexOf('confint') >= 0}catch(e){false} | input.reg_plots == 'coef'"

# cp_reg_conf_level <- "input.reg_predict == 'cmd' | input.reg_predict == 'data' |  (input.reg_sum_check !== undefined && input.reg_sum_check.indexOf('confint') >= 0) | input.reg_plots == 'coef'"

  tagList(
  	wellPanel(
		  conditionalPanel(condition = "input.tabs_regression == 'Plot'",
		    selectInput("reg_plots", "Regression plots:", choices = reg_plots,
			  	selected = state_single("reg_plots", reg_plots)),
        conditionalPanel(condition = "input.reg_plots == 'coef'",
        	checkboxInput("reg_coef_int", "Include intercept", state_init("reg_coef_int", FALSE))
        ),
        conditionalPanel(condition = "input.reg_plots == 'scatter' |
                                      input.reg_plots == 'dashboard' |
                                      input.reg_plots == 'resid_pred'",
          checkboxGroupInput("reg_lines", NULL, reg_lines,
            selected = state_init("reg_lines"), inline = TRUE)
        )
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
  	    		  value = state_init("reg_predict_cmd", ""))
          ),
          conditionalPanel(condition = "input.reg_predict == 'data'",
            selectizeInput(inputId = "reg_predict_data", label = "Predict for profiles:",
                        choices = c("None" = "",r_data$datasetlist),
                        selected = state_init("reg_predict_data", ""), multiple = FALSE)
          ),
  		    uiOutput("ui_reg_test_var"),
          checkboxGroupInput("reg_check", NULL, reg_check,
            selected = state_init("reg_check"), inline = TRUE),
          checkboxGroupInput("reg_sum_check", NULL, reg_sum_check,
            selected = state_init("reg_sum_check"), inline = TRUE)
  			),
        conditionalPanel(condition = "input.reg_predict == 'cmd' |
                         input.reg_predict == 'data' |
  	                     (input.reg_sum_check && input.reg_sum_check.indexOf('confint') >= 0) |
  	                     input.reg_plots == 'coef'",
   					 sliderInput("reg_conf_level", "Adjust confidence level:", min = 0.70,
   					             max = 0.99, value = state_init("reg_conf_level",.95),
   					             step = 0.01)
  		  ), br(),
        HTML("<label>Save:</label>"), br(),
        actionButton("reg_saveres", "Residuals"),
        downloadButton("reg_save_pred", "Predictions")
      )
	  ),
  	help_and_report(modal_title = "Linear regression (OLS)",
  	                fun_name = "regression",
  	                help_file = inclRmd("../quant/tools/help/regression.Rmd"))
	)
})


reg_plot <- reactive({

  # specifying plot heights
  plot_height <- 500
  plot_width <- 650
  nrVars <- length(input$reg_indep_var) + 1

  if(input$reg_plots == 'hist') plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
  if(input$reg_plots == 'dashboard') plot_height <- 1.5 * plot_height
  if(input$reg_plots == 'correlations') { plot_height <- 150 * nrVars; plot_width <- 150 * nrVars }
  if(input$reg_plots == 'coef') plot_height <- 300 + 20 * length(.regression()$model$coefficients)
  if(input$reg_plots %in% c('scatter','leverage','resid_pred'))
    plot_height <- (plot_height/2) * ceiling((nrVars-1) / 2)

  list(plot_width = plot_width, plot_height = plot_height)
})

reg_plot_width <- function()
  reg_plot() %>% { if (is.list(.)) .$plot_width else 650 }

reg_plot_height <- function()
  reg_plot() %>% { if (is.list(.)) .$plot_height else 500 }

# output is called from the main radiant ui.R
output$regression <- renderUI({

		register_print_output2("summary_regression", ".summary_regression")
    register_print_output2("predict_regression", ".predict_regression")
		register_plot_output2("plot_regression", ".plot_regression",
                         height_fun = "reg_plot_height",
                         width_fun = "reg_plot_width")

		# two separate tabs
		reg_output_panels <- tabsetPanel(
	    id = "tabs_regression",
	    tabPanel("Summary", verbatimTextOutput("summary_regression"),
               verbatimTextOutput("predict_regression")),
	    tabPanel("Plot", plotOutput("plot_regression", width = "100%", height = "100%"))
	  )

		statTabPanel2(menu = "Regression",
		              tool = "Linear (OLS)",
		              tool_ui = "ui_regression",
		             	output_panels = reg_output_panels)

})

# cleaning up the arguments for data_filter passed to report
# clean_args2 <- function(rep_args, rep_default = list()) {
#   if(rep_args$data_filter == "")
#     rep_args$data_filter  <- NULL
#   else
#     rep_args$data_filter %<>% gsub("\\n","", .) %>% gsub("\"","\'",.)

#   if(length(rep_default) == 0) rep_default[names(rep_args)] <- ""

#   # removing default arguments before sending to report feature
#   for(i in names(rep_args))
#     if(rep_args[[i]][1] == rep_default[[i]]) rep_args[[i]] <- NULL
#   rep_args
# }

.regression <- reactive({

  # if(not_available(input$reg_indep_var))
  #   return("Please select one or more independent variables.\n\n" %>% suggest_data("diamonds"))

	do.call(regression, reg_inputs())
})

.summary_regression <- reactive({
  if(not_available(input$reg_dep_var))
    return("This analysis requires a dependent variable of type integer\nor numeric and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("diamonds"))

  if(not_available(input$reg_indep_var))
    return("Please select one or more independent variables.\n\n" %>% suggest_data("diamonds"))

  # print(reg_sum_inputs())
  # summary(.regression(), reg_sum_check = input$reg_sum_check,
  #                        reg_conf_level = input$reg_conf_level,
  #                        reg_test_var = input$reg_test_var)
  do.call(summary, c(list(result = .regression()), reg_sum_inputs()))
})

.predict_regression <- reactive({
  if(not_available(input$reg_dep_var))
    return(invisible())

  if(not_available(input$reg_indep_var))
    return(invisible())

  if(is_empty(input$reg_predict, ""))
    return(invisible())

  # reg_predict_cmd <- reg_predict_data <- ""
  # if(input$reg_predict == "cmd")
  #   reg_predict_cmd <- gsub('\\s', '', input$reg_predict_cmd)

  # if(input$reg_predict == "data")
  #   reg_predict_data <- input$reg_predict_data

  # reg_save_pred <- TRUE
  # if(not_pressed(input$reg_save_pred))
  #   reg_save_pred <- FALSE

  # predict(.regression(), reg_predict_cmd = reg_predict_cmd,
  #                        reg_predict_data = reg_predict_data,
  #                        reg_conf_level = input$reg_conf_level,
  #                        reg_save_pred = reg_save_pred)
  # print(reg_pred_inputs())
  do.call(predict, c(list(result = .regression()), reg_pred_inputs()))
})

.plot_regression <- reactive({

  if(not_available(input$reg_dep_var))
    return("This analysis requires a dependent variable of type integer\nor numeric and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("diamonds"))

  if(not_available(input$reg_indep_var))
    return("Please select one or more independent variables.\n\n" %>% suggest_data("diamonds"))

  if(is_empty(input$reg_plots, ""))
    return("Please select a regression plot from the drop-down menu")

  # plot(.regression(), reg_plots = input$reg_plots,
  #                     reg_lines = input$reg_lines,
  #                     reg_conf_level = input$reg_conf_level,
  #                     reg_coef_int = input$reg_coef_int)
  do.call(plot, c(list(result = .regression()), reg_plot_inputs()))
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

# observe({
#   if(not_pressed(input$regression_report)) return()
#   isolate({
# 		outputs <- c("summary","plot")
#   	# if(reg_inputs()$reg_plots == "") outputs = c("summary")
# 		update_report(inp = clean_args(reg_inputs(), reg_args), fun_name = "regression",
# 		              outputs = outputs,
# 		              fig.width = round(7 * reg_plot_width()/650,2),
# 		              fig.height = round(7 * reg_plot_height()/500,2))
#   })
# })

observe({
  if(not_pressed(input$regression_report)) return()
  isolate({
    outputs <- c("summary")
    inp_out <- list("","","")
    inp_out[[1]] <- clean_args(reg_sum_inputs(), reg_sum_args[-1])
    figs <- FALSE
    if(!is_empty(input$reg_plots)) {
      inp_out[[2]] <- clean_args(reg_plot_inputs(), reg_plot_args[-1])
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
    if(!is_empty(input$reg_predict)) {
      inp_out[[3]] <- clean_args(c(reg_pred_inputs(), list(reg_save_pred = TRUE)), reg_pred_args[-1])
      outputs <- c(outputs, "pred <- predict")
    }
    update_report2(inp_main = clean_args(reg_inputs(), reg_args),
                  fun_name = "regression",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * reg_plot_width()/650,2),
                  fig.height = round(7 * reg_plot_height()/650,2),
                  xcmd = paste0("pred %T>% print %>% write.csv(., file = '~/reg_sav_pred.csv')\nsave_reg_resid(result)"))
  })
})

observe({
	if(not_pressed(input$reg_saveres)) return()
	isolate({
    .regression() %>% { if(is.list(.)) save_reg_resid(.) }
	})
})

output$reg_save_pred <- downloadHandler(
  filename = function() { "reg_save_pred.csv" },
  content = function(file) {
    do.call(predict, c(list(result = .regression()), reg_pred_inputs(), list(reg_save_pred = TRUE))) %>%
      write.csv(., file = file, row.names = FALSE)
  }
)
