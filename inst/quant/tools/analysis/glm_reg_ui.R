glm_link <- c("Logit" = "logit", "Probit" = "probit")
glm_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
glm_predict <- c("None" = "", "Data" = "data","Command" = "cmd")
glm_check <- c("Standardized coefficients" = "standardize",
               "Stepwise selection" = "stepwise")
glm_sum_check <- c("VIF" = "vif", "Confidence intervals" = "confint",
                   "Odds" = "odds")
glm_plots <- c("None" = "", "Histograms" = "hist",
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

# need the ::: because summary is an S3 method and not an exported function
glm_sum_args <- as.list(formals(radiant:::summary.glm_reg))

# list of function inputs selected by user
glm_sum_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(glm_sum_args))
    glm_sum_args[[i]] <- input[[i]]
  glm_sum_args
})

# need the ::: because plot is an S3 method and not an exported function
glm_plot_args <- as.list(formals(radiant:::plot.glm_reg))

# list of function inputs selected by user
glm_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(glm_plot_args))
    glm_plot_args[[i]] <- input[[i]]
  glm_plot_args
})

# need the ::: because plot is an S3 method and not an exported function
glm_pred_args <- as.list(formals(radiant:::predict.glm_reg))

# list of function inputs selected by user
glm_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(glm_pred_args))
    glm_pred_args[[i]] <- input[[i]]

  glm_pred_args$glm_predict_cmd <- glm_pred_args$glm_predict_data <- ""
  if(input$glm_predict == "cmd")
    glm_pred_args$glm_predict_cmd <- gsub('\\s', '', input$glm_predict_cmd)

  if(input$glm_predict == "data")
    glm_pred_args$glm_predict_data <- input$glm_predict_data

  glm_pred_args
})

# need the ::: because plot is an S3 method and not an exported function
glm_pred_plot_args <- as.list(formals(radiant:::plot.glm_predict))

# list of function inputs selected by user
glm_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(glm_pred_plot_args))
    glm_pred_plot_args[[i]] <- input[[i]]
  glm_pred_plot_args
})

output$ui_glm_dep_var <- renderUI({
 	vars <- two_level_vars()
  selectInput(inputId = "glm_dep_var", label = "Dependent variable:", choices = vars,
  	selected = state_single("glm_dep_var",vars), multiple = FALSE)
})

output$ui_glm_levels <- renderUI({
  levs <- c()
  if(!not_available(input$glm_dep_var))
    levs <- .getdata()[,input$glm_dep_var] %>% as.factor %>% levels
  selectInput(inputId = "glm_levels", label = "Choose level:",
              choices = levs,
              selected = state_single("glm_levels",levs), multiple = FALSE)
})

output$ui_glm_indep_var <- renderUI({
	notChar <- "character" != getdata_class()
  vars <- varnames()[notChar]
  if(not_available(input$glm_dep_var)) vars <- character(0)
  if(length(vars) > 0 ) vars <- vars[-which(vars == input$glm_dep_var)]
  selectInput(inputId = "glm_indep_var", label = "Independent variables:", choices = vars,
  	selected = state_multiple("glm_indep_var", vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_glm_test_var <- renderUI({
	# if(input$glm_indep_var %>% not_available) return()
 	vars <- input$glm_indep_var
	if(!is.null(input$glm_int_var)) vars <- c(vars,input$glm_int_var)

  selectizeInput(inputId = "glm_test_var", label = "Variables to test:",
    choices = vars, selected = state_multiple("glm_test_var", vars),
    multiple = TRUE,
    options = list(placeholder = 'None', plugins = list('remove_button'))
  )
})

output$ui_glm_show_interactions <- renderUI({
  if (length(input$glm_indep_var) == 2)
    choices <- glm_show_interactions[1:2]
  else if (length(input$glm_indep_var) > 2)
    choices <- glm_show_interactions
  else
    choices <- glm_show_interactions[1]

  radioButtons(inputId = "glm_show_interactions", label = "Interactions:",
               choices = choices,
               selected = state_init("glm_show_interactions"), inline = TRUE)
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
  if(is_empty(input$glm_show_interactions)) {
    choices <- character(0)
  } else {
    vars <- input$glm_indep_var
    if(not_available(vars) || length(vars) < 2) return()
    # vector of possible interaction terms to sel from glm_reg
    choices <- int_vec(vars, input$glm_show_interactions)
  }
	selectInput("glm_int_var", label = NULL, choices = choices,
  	selected = state_multiple("glm_int_var", choices),
  	multiple = TRUE, size = min(4,length(choices)), selectize = FALSE)
})

# X - variable
output$ui_glm_xvar <- renderUI({
  vars <- input$glm_indep_var
  selectizeInput(inputId = "glm_xvar", label = "X-variable:", choices = vars,
    selected = state_multiple("glm_xvar",vars),
    multiple = FALSE)
})

output$ui_glm_facet_row <- renderUI({
  vars <- input$glm_indep_var
  vars <- c("None" = ".", vars)
  selectizeInput("glm_facet_row", "Facet row", vars,
                 selected = state_single("glm_facet_row", vars, "."),
                 multiple = FALSE)
})

output$ui_glm_facet_col <- renderUI({
  vars <- input$glm_indep_var
  vars <- c("None" = ".", vars)
  selectizeInput("glm_facet_col", 'Facet column', vars,
                 selected = state_single("glm_facet_col", vars, "."),
                 multiple = FALSE)
})

output$ui_glm_color <- renderUI({
  vars <- c("None" = "none", input$glm_indep_var)
  sel <- state_single("glm_color", vars, "none")
  selectizeInput("glm_color", "Color", vars, selected = sel,
                 multiple = FALSE)
})

output$ui_glm_reg <- renderUI({
  tagList(
    conditionalPanel(condition = "input.tabs_glm_reg == 'Predict'",
      wellPanel(
        radioButtons(inputId = "glm_predict", label = "Prediction:", glm_predict,
          selected = state_init("glm_predict", ""),
          inline = TRUE),
        conditionalPanel(condition = "input.glm_predict == 'cmd'",
          returnTextAreaInput("glm_predict_cmd", "Prediction command:",
            value = state_init("glm_predict_cmd",""))
        ),
        conditionalPanel(condition = "input.glm_predict == 'data'",
          selectizeInput(inputId = "glm_predict_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_init("glm_predict_data"), multiple = FALSE)
        ),
        conditionalPanel(condition = "input.glm_predict != ''",
          uiOutput("ui_glm_xvar"),
          uiOutput("ui_glm_facet_row"),
          uiOutput("ui_glm_facet_col"),
          uiOutput("ui_glm_color"),
          downloadButton("glm_save_pred", "Save predictions")
        )
      )
    ),
    conditionalPanel(condition = "input.tabs_glm_reg == 'Plot'",
      wellPanel(
        selectInput("glm_plots", "GLM plots:", choices = glm_plots,
          selected = state_single("glm_plots", glm_plots)),
        conditionalPanel(condition = "input.glm_plots == 'coef'",
          checkboxInput("glm_coef_int", "Include intercept", state_init("glm_coef_int", FALSE)))
      )
    ),
    wellPanel(
    	radioButtons(inputId = "glm_link", label = NULL, glm_link,
    		selected = state_init("glm_link","logit"), inline = TRUE),
	    uiOutput("ui_glm_dep_var"),
      uiOutput("ui_glm_levels"),
	    uiOutput("ui_glm_indep_var"),

      conditionalPanel(condition = "input.glm_indep_var != null",

  			uiOutput("ui_glm_show_interactions"),
  		  conditionalPanel(condition = "input.glm_show_interactions != ''",
  				uiOutput("ui_glm_int_var")
  			),
        conditionalPanel(condition = "input.tabs_glm_reg == 'Summary'",
  		    uiOutput("ui_glm_test_var"),
          checkboxGroupInput("glm_check", NULL, glm_check,
            selected = state_init("glm_check"), inline = TRUE),
          checkboxGroupInput("glm_sum_check", NULL, glm_sum_check,
            selected = state_init("glm_sum_check"), inline = TRUE)
  			),
  	    conditionalPanel(condition = "(input.glm_sum_check && (input.glm_sum_check.indexOf('odds') >= 0 |
                         input.glm_sum_check.indexOf('confint') >= 0)) |
  	                     input.glm_plots == 'coef' |
                         input.tabs_glm_reg == 'Predict'",
   					 sliderInput("glm_conf_level", "Adjust confidence level:", min = 0.70,
   					             max = 0.99, value = state_init("glm_conf_level",.95),
   					             step = 0.01)
  		  ),
        conditionalPanel(condition = "input.tabs_glm_reg == 'Summary'",
  		    actionButton("glm_save_res", "Save residuals")
        )
      )
	  ),
  	help_and_report(modal_title = "GLM",
  	                fun_name = "glm_reg",
  	                help_file = inclRmd("../quant/tools/help/glm_reg.Rmd"))
	)
})

glm_plot <- reactive({

  plot_height <- 500
  plot_width <- 650
  nrVars <- length(input$glm_indep_var) + 1

  if(input$glm_plots == 'hist') plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
  if(input$glm_plots == 'dashboard') plot_height <- 1.5 * plot_height
  if(input$glm_plots == 'scatter') plot_height <- 300 * nrVars
  if(input$glm_plots == 'coef') plot_height <- 300 + 20 * length(.glm_reg()$model$coefficients)

  list(plot_width = plot_width, plot_height = plot_height)
})

glm_plot_width <- function()
  glm_plot() %>% { if (is.list(.)) .$plot_width else 650 }

glm_plot_height <- function()
  glm_plot() %>% { if (is.list(.)) .$plot_height else 500 }

glm_pred_plot_height <- function()
  if(input$tabs_glm_reg == "Predict" && is.null(r_data$glm_pred)) 0 else 500

# output is called from the main radiant ui.R
output$glm_reg <- renderUI({

		register_print_output("summary_glm_reg", ".summary_glm_reg")
    register_print_output("predict_glm_reg", ".predict_glm_reg")
    register_plot_output("predict_plot_glm_reg", ".predict_plot_glm_reg",
                          height_fun = "glm_pred_plot_height")
		register_plot_output("plot_glm_reg", ".plot_glm_reg",
                          height_fun = "glm_plot_height",
                          width_fun = "glm_plot_width")

		# two separate tabs
		glm_output_panels <- tabsetPanel(
	    id = "tabs_glm_reg",
	    tabPanel("Summary", verbatimTextOutput("summary_glm_reg")),
      tabPanel("Predict", plotOutput("predict_plot_glm_reg", width = "100%", height = "100%"),
               verbatimTextOutput("predict_glm_reg")),
	    tabPanel("Plot", plotOutput("plot_glm_reg", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Regression",
		              tool = "GLM",
		              tool_ui = "ui_glm_reg",
		             	output_panels = glm_output_panels)

})

.glm_reg <- reactive({
	do.call(glm_reg, glm_inputs())
})

.summary_glm_reg <- reactive({

  if(not_available(input$glm_dep_var))
    return("This analysis requires a dependent variable with two levels and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("titanic"))

  if(not_available(input$glm_indep_var))
    return("Please select one or more independent variables.\n\n" %>% suggest_data("titanic"))

  do.call(summary, c(list(object = .glm_reg()), glm_sum_inputs()))
})

.predict_glm_reg <- reactive({
  r_data$glm_pred <- NULL

  if(not_available(input$glm_dep_var))
    return(invisible())

  if(not_available(input$glm_indep_var))
    return(invisible())

  if(is_empty(input$glm_predict, ""))
    return(invisible())

  r_data$glm_pred <- do.call(predict, c(list(object = .glm_reg()), glm_pred_inputs()))
})

.predict_plot_glm_reg <- reactive({

  if(is_empty(input$glm_predict, ""))
    return(invisible())

  if(is.null(r_data$glm_pred))
    return(invisible())

  do.call(plot, c(list(x = r_data$glm_pred), glm_pred_plot_inputs()))
})

.plot_glm_reg <- reactive({

  if(not_available(input$glm_dep_var))
    return("This analysis requires a dependent variable with two levels and one or more independent variables.\nIf these variables are not available please select another dataset.\n\n" %>% suggest_data("titanic"))

  if(not_available(input$glm_indep_var))
    return("Please select one or more independent variables.\n\n" %>% suggest_data("titanic"))

  if(is_empty(input$glm_plots, ""))
    return("Please select a regression plot from the drop-down menu")

  do.call(plot, c(list(x = .glm_reg()), glm_plot_inputs()))
})

observe({
  if(not_pressed(input$glm_reg_report)) return()
  isolate({
    outputs <- c("summary","# save_glm_resid")
    inp_out <- list("","")
    inp_out[[1]] <- clean_args(glm_sum_inputs(), glm_sum_args[-1])
    figs <- FALSE
    if(!is_empty(input$glm_plots)) {
      inp_out[[3]] <- clean_args(glm_plot_inputs(), glm_plot_args[-1])
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
    xcmd <- ""
    if(!is.null(r_data$glm_pred)) {
      inp_out[[3 + figs]] <- clean_args(glm_pred_inputs(), glm_pred_args[-1])
      outputs <- c(outputs,"result <- predict")
      xcmd <- "# write.csv(result, file = '~/glm_sav_pred.csv', row.names = FALSE)"
      if(!is_empty(input$glm_xvar)) {
        inp_out[[4 + figs]] <- clean_args(glm_pred_plot_inputs(), glm_pred_plot_args[-1])
        outputs <- c(outputs, "plot")
        figs <- TRUE
      }
    }

    update_report(inp_main = clean_args(glm_inputs(), glm_args),
                  fun_name = "glm_reg",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * glm_plot_width()/650,2),
                  fig.height = round(7 * glm_plot_height()/650,2),
                  xcmd = xcmd)
  })
})

observe({
  if(not_pressed(input$glm_save_res)) return()
  isolate({
    .glm_reg() %>% { if(is.list(.)) save_glm_resid(.) }
  })
})

output$glm_save_pred <- downloadHandler(
  filename = function() { "glm_save_pred.csv" },
  content = function(file) {
    do.call(predict, c(list(result = .glm_reg()), glm_pred_inputs(),
            list(glm_save_pred = TRUE))) %>%
      write.csv(., file = file, row.names = FALSE)
  }
)
