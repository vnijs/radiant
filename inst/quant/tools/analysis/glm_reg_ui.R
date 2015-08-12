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

## list of function arguments
glm_args <- as.list(formals(glm_reg))

## list of function inputs selected by user
glm_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  glm_args$data_filter <- if (input$show_filter) input$data_filter else ""
  glm_args$dataset <- input$dataset
  for (i in r_drop(names(glm_args)))
    glm_args[[i]] <- input[[paste0("glm_",i)]]
  glm_args
})

glm_sum_args <- as.list(if (exists("summary.glm_reg")) formals(summary.glm_reg)
                        else formals(radiant:::summary.glm_reg))

## list of function inputs selected by user
glm_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(glm_sum_args))
    glm_sum_args[[i]] <- input[[paste0("glm_",i)]]
  glm_sum_args
})

glm_plot_args <- as.list(if (exists("plot.glm_reg")) formals(plot.glm_reg)
                         else formals(radiant:::plot.glm_reg))

## list of function inputs selected by user
glm_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(glm_plot_args))
    glm_plot_args[[i]] <- input[[paste0("glm_",i)]]
  glm_plot_args
})

glm_pred_args <- as.list(if (exists("predict.glm_reg")) formals(predict.glm_reg)
                         else formals(radiant:::predict.glm_reg))

# list of function inputs selected by user
glm_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(glm_pred_args))
    glm_pred_args[[i]] <- input[[paste0("glm_",i)]]

  glm_pred_args$pred_cmd <- glm_pred_args$pred_data <- ""
  if (input$glm_predict == "cmd")
    glm_pred_args$pred_cmd <- gsub('\\s', '', input$glm_pred_cmd)

  if (input$glm_predict == "data")
    glm_pred_args$pred_data <- input$glm_pred_data

  glm_pred_args
})

glm_pred_plot_args <- as.list(if (exists("plot.glm_predict")) formals(plot.glm_predict)
                              else formals(radiant:::plot.glm_predict))

# list of function inputs selected by user
glm_pred_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(glm_pred_plot_args))
    glm_pred_plot_args[[i]] <- input[[paste0("glm_",i)]]
  glm_pred_plot_args
})

output$ui_glm_dep_var <- renderUI({
 	vars <- two_level_vars()
  selectInput(inputId = "glm_dep_var", label = "Dependent variable:", choices = vars,
  	selected = state_single("glm_dep_var",vars), multiple = FALSE)
})

output$ui_glm_lev <- renderUI({
  levs <- c()
  if (!not_available(input$glm_dep_var))
    levs <- .getdata()[,input$glm_dep_var] %>% as.factor %>% levels
  selectInput(inputId = "glm_lev", label = "Choose level:",
              choices = levs,
              selected = state_single("glm_lev",levs), multiple = FALSE)
})

output$ui_glm_indep_var <- renderUI({
	notChar <- "character" != .getclass()
  vars <- varnames()[notChar]
  if (not_available(input$glm_dep_var)) vars <- character(0)
  if (length(vars) > 0 ) vars <- vars[-which(vars == input$glm_dep_var)]
  selectInput(inputId = "glm_indep_var", label = "Independent variables:", choices = vars,
  	selected = state_multiple("glm_indep_var", vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_glm_test_var <- renderUI({
 	vars <- input$glm_indep_var
	if (!is.null(input$glm_int_var)) vars <- c(vars,input$glm_int_var)

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

output$ui_glm_int_var <- renderUI({
  if (is_empty(input$glm_show_interactions)) {
    choices <- character(0)
  } else {
    vars <- input$glm_indep_var
    if (not_available(vars) || length(vars) < 2) return()
    # vector of possible interaction terms to sel from glm_reg
    choices <- iterms(vars, input$glm_show_interactions)
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
          selected = state_init("glm_predict", ""), inline = TRUE),
        conditionalPanel(condition = "input.glm_predict == 'cmd'",
          returnTextAreaInput("glm_pred_cmd", "Prediction command:",
            value = state_init("glm_pred_cmd",""))
        ),
        conditionalPanel(condition = "input.glm_predict == 'data'",
          selectizeInput(inputId = "glm_pred_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_init("glm_pred_data"), multiple = FALSE)
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
          checkboxInput("glm_intercept", "Include intercept", state_init("glm_intercept", FALSE)))
      )
    ),
    wellPanel(
    	radioButtons(inputId = "glm_link", label = NULL, glm_link,
    		selected = state_init("glm_link","logit"), inline = TRUE),
	    uiOutput("ui_glm_dep_var"),
      uiOutput("ui_glm_lev"),
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
   					 sliderInput("glm_conf_lev", "Adjust confidence level:", min = 0.70,
   					             max = 0.99, value = state_init("glm_conf_lev",.95),
   					             step = 0.01)
  		  ),
        conditionalPanel(condition = "input.tabs_glm_reg == 'Summary'",
  		    actionButton("glm_store_res", "Store residuals")
        )
      )
	  ),
  	help_and_report(modal_title = "GLM",
  	                fun_name = "glm_reg",
  	                help_file = inclRmd(file.path(r_path,"quant/tools/help/glm_reg.Rmd")))
	)
})

glm_plot <- reactive({

  if (glm_available() != "available") return()
  if(is_empty(input$glm_plots)) return()

  plot_height <- 500
  plot_width <- 650
  nrVars <- length(input$glm_indep_var) + 1

  if (input$glm_plots == 'hist') plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
  if (input$glm_plots == 'dashboard') plot_height <- 1.5 * plot_height
  if (input$glm_plots == 'scatter') plot_height <- 300 * nrVars
  if (input$glm_plots == 'coef') plot_height <- 300 + 20 * length(.glm_reg()$model$coefficients)

  list(plot_width = plot_width, plot_height = plot_height)
})

glm_plot_width <- function()
  glm_plot() %>% { if (is.list(.)) .$plot_width else 650 }

glm_plot_height <- function()
  glm_plot() %>% { if (is.list(.)) .$plot_height else 500 }

glm_pred_plot_height <- function()
  if (input$tabs_glm_reg == "Predict" && is.null(r_data$glm_pred)) 0 else 500

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
      tabPanel("Predict",
               plot_downloader("glm_reg", height = glm_pred_plot_height(), po = "dlp_", pre = ".predict_plot_"),
               plotOutput("predict_plot_glm_reg", width = "100%", height = "100%"),
               verbatimTextOutput("predict_glm_reg")),
	    tabPanel("Plot", plot_downloader("glm_reg", height = glm_plot_height()),
               plotOutput("plot_glm_reg", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Regression",
		              tool = "GLM",
		              tool_ui = "ui_glm_reg",
		             	output_panels = glm_output_panels)

})

glm_available <- reactive({
  if (not_available(input$glm_dep_var))
    return("This analysis requires a dependent variable with two levels and one\nor more independent variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))

  if (not_available(input$glm_indep_var))
    return("Please select one or more independent variables.\n\n" %>% suggest_data("titanic"))

  "available"
})

.glm_reg <- reactive({
	do.call(glm_reg, glm_inputs())
})

.summary_glm_reg <- reactive({
  if (glm_available() != "available") return(glm_available())
  do.call(summary, c(list(object = .glm_reg()), glm_sum_inputs()))
})

.predict_glm_reg <- reactive({
  r_data$glm_pred <- NULL
  if (glm_available() != "available") return(glm_available())
  if (is_empty(input$glm_predict)) return(invisible())
  r_data$glm_pred <- do.call(predict, c(list(object = .glm_reg()), glm_pred_inputs()))
})

.predict_plot_glm_reg <- reactive({
  if (glm_available() != "available") return(glm_available())
  if (is_empty(input$glm_predict) || is.null(r_data$glm_pred)) return(" ")
  do.call(plot, c(list(x = r_data$glm_pred), glm_pred_plot_inputs()))
})

.plot_glm_reg <- reactive({
  if (glm_available() != "available") return(glm_available())
  if (is_empty(input$glm_plots))
    return("Please select a regression plot from the drop-down menu")

  glm_plot_inputs() %>% { .$shiny <- TRUE; . } %>%
    { do.call(plot, c(list(x = .glm_reg()), .)) }
})

observeEvent(input$glm_reg_report, {
  isolate({
    outputs <- c("summary","# store_glm_resid")
    inp_out <- list("","")
    inp_out[[1]] <- clean_args(glm_sum_inputs(), glm_sum_args[-1])
    figs <- FALSE
    if (!is_empty(input$glm_plots)) {
      inp_out[[3]] <- clean_args(glm_plot_inputs(), glm_plot_args[-1])
      outputs <- c(outputs, "plot")
      figs <- TRUE
    }
    xcmd <- ""
    if (!is.null(r_data$glm_pred)) {
      inp_out[[3 + figs]] <- clean_args(glm_pred_inputs(), glm_pred_args[-1])
      outputs <- c(outputs,"result <- predict")
      xcmd <- "# write.csv(result, file = '~/glm_sav_pred.csv', row.names = FALSE)"
      if (!is_empty(input$glm_xvar)) {
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

observeEvent(input$glm_store_res, {
  isolate({
    .glm_reg() %>% { if (is.list(.)) store_glm_resid(.) }
  })
})

output$glm_save_pred <- downloadHandler(
  filename = function() { "glm_save_pred.csv" },
  content = function(file) {
    do.call(predict, c(list(object = .glm_reg()), glm_pred_inputs(),
            list(glm_save_pred = TRUE))) %>%
      write.csv(., file = file, row.names = FALSE)
  }
)
