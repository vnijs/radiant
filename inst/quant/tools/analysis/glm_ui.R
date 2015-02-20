glm_link <- c("Logit" = "logit", "Probit" = "probit")
glm_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
glm_predict <- c("None" = "", "Data" = "data","Command" = "cmd")
glm_check <- c("VIF" = "vif", "Confidence intervals" = "confint", "Odds" = "odds",
               "Standardized coefficients" = "standardize",
               "Stepwise selection" = "stepwise")
glm_plots <- list("None" = "", "Histograms" = "hist",
                  "Scatter" = "scatter", "Dashboard" = "dashboard",
                  "Coefficient plot" = "coef")


# list of function arguments
glm_args <- as.list(formals(glm_reg))

# list of function inputs selected by user
glm_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(glm_args))
    glm_args[[i]] <- input[[i]]
  if(!input$show_filter) glm_args$data_filter = ""
  glm_args
})

output$ui_glm_dep_var <- renderUI({
 	vars <- two_level_vars()
  selectInput(inputId = "glm_dep_var", label = "Dependent variable:", choices = vars,
  	selected = state_single("glm_dep_var",vars), multiple = FALSE)
})

output$ui_glm_indep_var <- renderUI({
	notChar <- "character" != getdata_class()
  vars <- varnames()[notChar]
  if(input$glm_dep_var %>% not_available) vars <- character(0)
  if(length(vars) > 0 ) vars <- vars[-which(vars == input$glm_dep_var)]
  selectInput(inputId = "glm_indep_var", label = "Independent variables:", choices = vars,
  	selected = state_multiple("glm_indep_var", vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_glm_test_var <- renderUI({
	if(input$glm_indep_var %>% not_available) return()

 	vars <- input$glm_indep_var
	if(!is.null(input$glm_int_var) && input$glm_interactions != '')
		vars <- c(vars,input$glm_int_var)

  selectizeInput(inputId = "glm_test_var", label = "Variables to test:",
    choices = vars, selected = state_multiple("glm_test_var", vars),
    multiple = TRUE,
    options = list(placeholder = 'None', plugins = list('remove_button'))
  )
})

output$ui_glm_interactions <- renderUI({
	if(input$glm_indep_var %>% not_available) return()
 	sel <- ""
 	if(length(input$glm_indep_var) > 1)
 		sel <- state_init("glm_interactions")

  radioButtons(inputId = "glm_interactions", label = "Interactions:", glm_interactions,
  	selected = sel, inline = TRUE)
 })

# create vector of possible interaction terms
int_vec <- function(vars, nway) {
  if(nway == "") return(character(0))
  int_vec <- c()
  for(i in 2:nway) {
    int_vec %<>% {c(., combn(vars, i) %>% apply( 2, paste, collapse = ":" ))}
  }
  int_vec
}

output$ui_glm_int_var <- renderUI({
	if(input$glm_interactions %>% is_empty) return()
  vars <- input$glm_indep_var
	if(vars %>% not_available || length(vars) < 2) return()
 	choices <- int_vec(vars, input$glm_interactions)
	selectInput("glm_int_var", label = NULL, choices = choices,
  	selected = state_multiple("glm_int_var", choices),
  	multiple = TRUE, selectize = FALSE)
})

output$ui_glm_levels <- renderUI({
  if(input$glm_dep_var %>% not_available)
    levs <- c()
  else
    levs <- getdata()[1,input$glm_dep_var] %>% as.factor %>% levels

  selectInput(inputId = "glm_levels", label = "Choose level:",
              choices = levs,
              selected = state_single("glm_levels",levs), multiple = FALSE)
})

output$ui_glm_reg <- renderUI({
  tagList(
  	wellPanel(
    	radioButtons(inputId = "glm_link", label = NULL, glm_link,
    		selected = state_init("glm_link","logit"), inline = TRUE),
		  conditionalPanel(condition = "input.tabs_glm_reg == 'Plot'",
		    selectInput("glm_plots", "GLM plots:", choices = glm_plots,
			  	selected = state_single("glm_plots", glm_plots)),
		  	conditionalPanel(condition = "input.glm_plots == 'coef'",
        	checkboxInput("glm_coef_int", "Include intercept", state_init("glm_coef_int", FALSE))
        )
		  ),
	    uiOutput("ui_glm_dep_var"),
      uiOutput("ui_glm_levels"),
	    uiOutput("ui_glm_indep_var"),
			# uiOutput("ui_glm_interactions"),
		 #  conditionalPanel(condition = "input.glm_interactions != ''",
			# 	uiOutput("ui_glm_int_var")
			# ),
		  conditionalPanel(condition = "input.tabs_glm_reg == 'Summary'",
   	    radioButtons(inputId = "glm_predict", label = "Prediction:", glm_predict,
	      	selected = state_init("glm_predict", ""),
	      	inline = TRUE),

        conditionalPanel(condition = "input.glm_predict == 'cmd'",
          returnTextAreaInput("glm_predict_cmd", "Prediction command:",
	    		  value = state_init("glm_predict_cmd"))
        ),
        conditionalPanel(condition = "input.glm_predict == 'data'",
          selectizeInput(inputId = "glm_predict_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_init("glm_predict_data"), multiple = FALSE)
        ),
		    uiOutput("ui_glm_test_var"),
        checkboxGroupInput("glm_check", NULL, glm_check,
          selected = state_init("glm_check"), inline = TRUE)
			),
	    # conditionalPanel(condition = "input.glm_predict != '' |
	    conditionalPanel(condition = "input.glm_check.indexOf('confint') >= 0 |
	                     input.glm_plots == 'coef'",
 					 sliderInput("glm_conf_level", "Adjust confidence level:", min = 0.70,
 					             max = 0.99, value = state_init("glm_conf_level",.95),
 					             step = 0.01)
		  ), br(),
		  actionButton("glm_saveres", "Save residuals")
	  ),
  	help_and_report(modal_title = "GLM",
  	                fun_name = "glm_reg",
  	                help_file = inclMD("../quant/tools/help/glm_reg.md"))
	)
})

glm_plot_width <- function() {
	.glm_reg() %>%
  { if(class(.)[1] == "character") 650 else .$plot_width }
}

glm_plot_height <- function() {
  .glm_reg() %>%
  { if(class(.)[1] == "character") 500 else .$plot_height }
}

# output is called from the main radiant ui.R
output$glm_reg <- renderUI({

		register_print_output("summary_glm_reg", ".glm_reg")
		register_plot_output("plot_glm_reg", ".glm_reg",
                         height_fun = "glm_plot_height",
                         width_fun = "glm_plot_width")

		# two separate tabs
		glm_output_panels <- tabsetPanel(
	    id = "tabs_glm_reg",
	    tabPanel("Summary", verbatimTextOutput("summary_glm_reg")),
	    tabPanel("Plot", plotOutput("plot_glm_reg", width = "100%", height = "100%"))
	  )

		statTabPanel2(menu = "Regression",
		              tool = "GLM",
		              tool_ui = "ui_glm_reg",
		             	output_panels = glm_output_panels)

})

.glm_reg <- reactive({
	if(input$glm_dep_var %>% not_available)
		return("This analysis requires a dependent variable with two levels and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("titanic"))

	if(input$glm_indep_var %>% not_available)
		return("Please select one or more independent variables.\n\n" %>% suggest_data("titanic"))

	do.call(glm_reg, glm_inputs())
})


observe({
  if(input$glm_reg_report %>% not_pressed) return()
  isolate({
		outputs <- c("summary","plot")
  	if(glm_inputs()$glm_plots == "") outputs = c("summary")
		update_report(inp = clean_args(glm_inputs(), glm_args), fun_name = "glm_reg",
		              outputs = outputs,
		              fig.width = round(7 * glm_plot_width()/650,2),
		              fig.height = round(7 * glm_plot_height()/500,2))
  })
})

observe({
	if(input$glm_saveres %>% not_pressed) return()
	isolate({
		result <- .glm_reg()
		if(result %>% is.character) return()
		result$model$residuals %>% data.frame %>% changedata("residuals")
	})
})
