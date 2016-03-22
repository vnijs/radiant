glm_link <- c("Logit" = "logit", "Probit" = "probit")
glm_show_interactions <- c("None" = "", "2-way" = 2, "3-way" = 3)
# glm_predict <- c("None" = "none", "Variable" = "vars", "Data" = "data","Command" = "cmd")
glm_predict <- c("None" = "none", "Data" = "data","Command" = "cmd", "Data & Command" = "datacmd")
glm_check <- c("Standardize" = "standardize", "Center" = "center",
               "Stepwise" = "stepwise")
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

  # isolate(cat(paste0(names(glm_args), " ", glm_args, collapse = ", "), file = stderr(), "\n"))
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

  # cat(paste0(names(glm_plot_args), " ", glm_plot_args, collapse = ", "), file = stderr(), "\n")
  glm_plot_args
})

glm_pred_args <- as.list(if (exists("predict.glm_reg")) formals(predict.glm_reg)
                         else formals(radiant:::predict.glm_reg))

# list of function inputs selected by user
glm_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(glm_pred_args))
    glm_pred_args[[i]] <- input[[paste0("glm_",i)]]

  glm_pred_args$pred_cmd <- glm_pred_args$pred_data <- glm_pred_args$pred_vars <- ""
  if (input$glm_predict == "cmd") {
    glm_pred_args$pred_cmd <- gsub("\\s", "", input$glm_pred_cmd) %>% gsub("\"","\'",.)
  } else if (input$glm_predict == "data") {
    glm_pred_args$pred_data <- input$glm_pred_data
  } else if (input$glm_predict == "datacmd") {
    glm_pred_args$pred_cmd <- gsub("\\s", "", input$glm_pred_cmd) %>% gsub("\"","\'",.)
    glm_pred_args$pred_data <- input$glm_pred_data
  } else if (input$glm_predict == "vars") {
    glm_pred_args$pred_vars <- input$glm_pred_vars
  }

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

output$ui_glm_rvar <- renderUI({
  # req(input$dataset)
 	vars <- two_level_vars()
  selectInput(inputId = "glm_rvar", label = "Response variable:", choices = vars,
  	selected = state_single("glm_rvar",vars), multiple = FALSE)
})

output$ui_glm_lev <- renderUI({
  req(input$glm_rvar)
  if (available(input$glm_rvar))
    levs <- .getdata()[[input$glm_rvar]] %>% as.factor %>% levels
  else
    levs <- c()
  selectInput(inputId = "glm_lev", label = "Choose level:",
    # choices = levs, selected = use_input_nonvar("glm_lev", levs))
    choices = levs, selected = state_init("glm_lev"))
})

output$ui_glm_evar <- renderUI({
  req(available(input$glm_rvar))
	notChar <- "character" != .getclass()
  vars <- varnames()[notChar]
  if (length(vars) > 0 && input$glm_rvar %in% vars)
    vars <- vars[-which(vars == input$glm_rvar)]

  selectInput(inputId = "glm_evar", label = "Explanatory variables:", choices = vars,
    selected = state_multiple("glm_evar", vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_glm_wts <- renderUI({
  req(available(input$glm_rvar), available(input$glm_evar))
  isNum <- .getclass() %in% c("numeric","integer")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$glm_evar)) {
    vars <- setdiff(vars, input$glm_evar)
    names(vars) <- varnames() %>% {.[match(vars, .)]} %>% names
  }
  vars <- c("None", vars)

  selectInput(inputId = "glm_wts", label = "Weights:", choices = vars,
    selected = state_single("glm_wts", vars),
    multiple = FALSE)
})


output$ui_glm_pred_var <- renderUI({
  vars <- input$glm_evar
  selectInput("glm_pred_var", label = "Predict for variables:",
    choices = vars, selected = state_multiple("glm_pred_var", vars),
    multiple = TRUE, size = min(4, length(vars)), selectize = FALSE)
})

output$ui_glm_test_var <- renderUI({
  req(available(input$glm_evar))
 	vars <- input$glm_evar
	if (!is.null(input$glm_int)) vars <- c(vars,input$glm_int)

  selectizeInput(inputId = "glm_test_var", label = "Variables to test:",
    choices = vars,
    selected = state_multiple("glm_test_var", vars),
    multiple = TRUE,
    options = list(placeholder = "None", plugins = list("remove_button"))
  )
})

output$ui_glm_show_interactions <- renderUI({
  # req(input$glm_evar)
  choices <- glm_show_interactions[1:max(min(3,length(input$glm_evar)),1)]
  radioButtons(inputId = "glm_show_interactions", label = "Interactions:",
    choices = choices,
    # selected = use_input_nonvar("glm_show_interactions", choices, init = ""),
    selected = state_init("glm_show_interactions"),
    inline = TRUE)
})

output$ui_glm_int <- renderUI({
  if (isolate("glm_show_interactions" %in% names(input)) &&
      is_empty(input$glm_show_interactions)) {
    choices <- character(0)
  } else if (is_empty(input$glm_show_interactions)) {
    return()
  } else {
    vars <- input$glm_evar
    if (not_available(vars) || length(vars) < 2) return()
    ## list of interaction terms to list
    choices <- iterms(vars, input$glm_show_interactions)
  }

	selectInput("glm_int", label = NULL, choices = choices,
    # selected = use_input_nonvar("glm_int", choices),
    selected = state_init("glm_int"),
  	multiple = TRUE, size = min(4,length(choices)), selectize = FALSE)
})

## X - variable
output$ui_glm_xvar <- renderUI({
  vars <- input$glm_evar
  selectizeInput(inputId = "glm_xvar", label = "X-variable:", choices = vars,
    selected = state_multiple("glm_xvar",vars),
    multiple = FALSE)
})

output$ui_glm_facet_row <- renderUI({
  vars <- input$glm_evar
  vars <- c("None" = ".", vars)
  selectizeInput("glm_facet_row", "Facet row", vars,
                 selected = state_single("glm_facet_row", vars, "."),
                 multiple = FALSE)
})

output$ui_glm_facet_col <- renderUI({
  vars <- input$glm_evar
  vars <- c("None" = ".", vars)
  selectizeInput("glm_facet_col", "Facet column", vars,
                 selected = state_single("glm_facet_col", vars, "."),
                 multiple = FALSE)
})

output$ui_glm_color <- renderUI({
  vars <- c("None" = "none", input$glm_evar)
  sel <- state_single("glm_color", vars, "none")
  selectizeInput("glm_color", "Color", vars, selected = sel,
                 multiple = FALSE)
})

output$ui_glm_reg <- renderUI({
  req(input$dataset)
  tagList(
    # conditionalPanel(condition = "input.tabs_glm_reg == 'Summary'",
      wellPanel(
        actionButton("glm_run", "Estimate", width = "100%")
      ),
    # ),
    conditionalPanel(condition = "input.tabs_glm_reg == 'Predict'",
      wellPanel(

        selectInput("glm_predict", label = "Prediction input:", glm_predict,
          selected = state_single("glm_predict", glm_predict, "none")),
        conditionalPanel(condition = "input.glm_predict == 'vars'",
          uiOutput("ui_glm_pred_var")
        ),

        # radioButtons(inputId = "glm_predict", label = "Prediction:", glm_predict,
        #   selected = state_init("glm_predict", ""), inline = TRUE),
        conditionalPanel("input.glm_predict == 'data' | input.glm_predict == 'datacmd'",
          selectizeInput(inputId = "glm_pred_data", label = "Predict for profiles:",
                      choices = c("None" = "",r_data$datasetlist),
                      selected = state_init("glm_pred_data"), multiple = FALSE)
        ),
        conditionalPanel("input.glm_predict == 'cmd' | input.glm_predict == 'datacmd'",
          returnTextAreaInput("glm_pred_cmd", "Prediction command:",
            value = state_init("glm_pred_cmd",""))
        ),
        conditionalPanel(condition = "input.glm_predict != 'none'",
          checkboxInput("glm_pred_plot", "Plot predictions", state_init("glm_pred_plot", FALSE)),
          conditionalPanel("input.glm_pred_plot == true",
            uiOutput("ui_glm_xvar"),
            uiOutput("ui_glm_facet_row"),
            uiOutput("ui_glm_facet_col"),
            uiOutput("ui_glm_color")
          )
        ),
        ## only show if full data is used for prediction
        conditionalPanel("input.glm_predict == 'data' | input.glm_predict == 'datacmd'",
          tags$table(
            tags$td(textInput("glm_store_pred_name", "Store predictions:", state_init("glm_store_pred_name","predict_glm"))),
            tags$td(actionButton("glm_store_pred", "Store"), style="padding-top:30px;")
          )
        )
        # conditionalPanel("input.glm_predict == 'cmd'",
        #   tags$table(
        #     tags$td(textInput("glm_store_pred_dat_name", "Store predicted dataset:", state_init("glm_store_pred_dat_name",paste0(input$dataset,"_pred")))),
        #     tags$td(actionButton("glm_store_pred", "Store"), style="padding-top:30px;")
        #   )
        # )
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
      # checkboxInput("glm_pause", "Pause estimation", state_init("glm_pause", FALSE)),
    	radioButtons(inputId = "glm_link", label = NULL, glm_link,
    		selected = state_init("glm_link","logit"), inline = TRUE),
	    uiOutput("ui_glm_rvar"),
      uiOutput("ui_glm_lev"),
	    uiOutput("ui_glm_evar"),
      uiOutput("ui_glm_wts"),
      conditionalPanel(condition = "input.glm_evar != null",

  			uiOutput("ui_glm_show_interactions"),
  		  conditionalPanel(condition = "input.glm_show_interactions != ''",
  				uiOutput("ui_glm_int")
  			),
        conditionalPanel(condition = "input.tabs_glm_reg == 'Summary'",
  		    uiOutput("ui_glm_test_var"),
          checkboxGroupInput("glm_check", NULL, glm_check,
            selected = state_init("glm_check"), inline = TRUE),
          checkboxGroupInput("glm_sum_check", NULL, glm_sum_check,
            selected = state_init("glm_sum_check", ""), inline = TRUE)
  			),
        ## Using && to check that input.glm_sum_check is not null (must be &&)
  	    conditionalPanel(condition = "(input.glm_sum_check && (input.glm_sum_check.indexOf('odds') >= 0 |
                         input.glm_sum_check.indexOf('confint') >= 0)) |
  	                     input.glm_plots == 'coef' |
                         input.tabs_glm_reg == 'Predict'",
   					 sliderInput("glm_conf_lev", "Confidence level:", min = 0.80,
   					             max = 0.99, value = state_init("glm_conf_lev",.95),
   					             step = 0.01)
  		  ),
        ## Only save residuals when filter is off
        conditionalPanel(condition = "input.tabs_glm_reg == 'Summary'",
          tags$table(
            tags$td(textInput("glm_store_res_name", "Store residuals:", state_init("glm_store_res_name","residuals_glm"))),
            tags$td(actionButton("glm_store_res", "Store"), style="padding-top:30px;")
          )
        )
      )
	  ),
  	help_and_report(modal_title = "Logist regression (GLM)", fun_name = "glm_reg",
  	                help_file = inclRmd(file.path(r_path,"quant/tools/help/glm_reg.Rmd")))
	)
})

glm_plot <- reactive({

  if (glm_available() != "available") return()
  if (is_empty(input$glm_plots)) return()

  plot_height <- 500
  plot_width <- 650
  nrVars <- length(input$glm_evar) + 1

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
  if (input$glm_pred_plot) 500 else 0

## output is called from the main radiant ui.R
output$glm_reg <- renderUI({

		register_print_output("summary_glm_reg", ".summary_glm_reg")
    register_print_output("predict_glm_reg", ".predict_glm_reg")
    register_plot_output("predict_plot_glm_reg", ".predict_plot_glm_reg",
                          height_fun = "glm_pred_plot_height")
		register_plot_output("plot_glm_reg", ".plot_glm_reg",
                          height_fun = "glm_plot_height",
                          width_fun = "glm_plot_width")

		## two separate tabs
		glm_output_panels <- tabsetPanel(
	    id = "tabs_glm_reg",
	    tabPanel("Summary",
        downloadLink("dl_glm_coef", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("summary_glm_reg")
      ),
      tabPanel("Predict",
        conditionalPanel("input.glm_pred_plot == true",
          plot_downloader("glm_reg", height = glm_pred_plot_height(), po = "dlp_", pre = ".predict_plot_"),
          plotOutput("predict_plot_glm_reg", width = "100%", height = "100%")
        ),
        downloadLink("dl_glm_pred", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("predict_glm_reg")
      ),
	    tabPanel("Plot",
               plot_downloader("glm_reg", height = glm_plot_height()),
               plotOutput("plot_glm_reg", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Regression", tool = "Logistic regression (GLM)",
                   tool_ui = "ui_glm_reg", output_panels = glm_output_panels)
})

glm_available <- reactive({
  if (not_available(input$glm_rvar))
    return("This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))

  if (not_available(input$glm_evar))
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("titanic"))

  "available"
})

# .glm_reg <- reactive({
#   # if (is.null(input$glm_pause) || input$glm_pause == TRUE) {
#   if (pressed(input$glm_store_pred) || pressed(input$glm_store_res)) {
#     isolate(.glm_reg2())
#     # cancelOutput()
#   } else {
#     .glm_reg2()
#   }
# })

.glm_reg <- eventReactive(input$glm_run, {

  # isolate({
  #   if (pressed(input$glm_store_pred) || pressed(input$glm_store_res))
  #     cancelOutput()
  #   if (!is.null(input$a_button) && input$a_button > 0) cancelOutput()
  # })

  # req(input$glm_pause == FALSE, cancelOutput = TRUE)

  req(available(input$glm_rvar), available(input$glm_evar))
  # req(input$glm_lev, input$glm_wts)
  req(input$glm_lev)
  req(input$glm_wts == "None" || available(input$glm_wts))

  ## need dependency on glm_int so I can have names(input) in isolate
  # input$glm_int
  # isolate(req("glm_int" %in% names(input)))

  withProgress(message = 'Estimating model', value = 0,
    do.call(glm_reg, glm_inputs())
  )
})

.summary_glm_reg <- reactive({
  if (glm_available() != "available") return(glm_available())
  if (not_pressed(input$glm_run)) return("** Press the Estimate button to estimate the model **")
  do.call(summary, c(list(object = .glm_reg()), glm_sum_inputs()))
})

.plot_glm_reg <- reactive({
  if (glm_available() != "available") return(glm_available())
  if (is_empty(input$glm_plots))
    return("Please select a regression plot from the drop-down menu")
  if (not_pressed(input$glm_run)) return("** Press the Estimate button to estimate the model **")

  pinp <- glm_plot_inputs()
  pinp$shiny <- TRUE
  do.call(plot, c(list(x = .glm_reg()), pinp))
})

.predict_glm_reg <- reactive({
  if (glm_available() != "available") return(glm_available())
  if (not_pressed(input$glm_run)) return("** Press the Estimate button to estimate the model **")
  if (is_empty(input$glm_predict, "none")) return("** Select prediction input **")
  # req(!is_empty(input$glm_predict, "none"),
  # req(!is_empty(input$glm_pred_data) || !is_empty(input$glm_pred_cmd))

  # if(is_empty(input$glm_pred_data) && is_empty(input$glm_pred_cmd))
    # return("** Select prediction data or commands **")

  if((input$glm_predict == "data" || input$glm_predict == "datacmd") && is_empty(input$glm_pred_data))
    return("** Select data for prediction **")
  if(input$glm_predict == "cmd" && is_empty(input$glm_pred_cmd))
    return("** Enter prediction commands **")

  withProgress(message = "Generating predictions", value = 0, {
    do.call(predict, c(list(object = .glm_reg()), glm_pred_inputs()))
  })
})

# observeEvent(input$store_glm_cmd, {
#   view_store(input$dataset, input$view_vars, input$view_dat, data_filter, input$dataviewer_rows_all)
# })

# store_glm_cmd <- function(dataset)
#   r_data[[dataset]] <- .getdata_transform()
#   r_data[[paste0(dataset,"_descr")]] <- "New data"
#   r_data[['datasetlist']] %<>% c(dataset,.) %>% unique
# }

.predict_plot_glm_reg <- reactive({
  if (glm_available() != "available") return(glm_available())
  req(input$glm_pred_plot, available(input$glm_xvar))
  if (not_pressed(input$glm_run)) return(invisible())
  if (is_empty(input$glm_predict, "none")) return(invisible())
  if((input$glm_predict == "data" || input$glm_predict == "datacmd") && is_empty(input$glm_pred_data))
    return(invisible())
  if(input$glm_predict == "cmd" && is_empty(input$glm_pred_cmd))
    return(invisible())
  do.call(plot, c(list(x = .predict_glm_reg()), glm_pred_plot_inputs()))
})

observeEvent(input$glm_reg_report, {
  outputs <- c("summary")
  inp_out <- list("","")
  inp_out[[1]] <- clean_args(glm_sum_inputs(), glm_sum_args[-1])
  figs <- FALSE
  if (!is_empty(input$glm_plots)) {
    inp_out[[2]] <- clean_args(glm_plot_inputs(), glm_plot_args[-1])
    outputs <- c(outputs, "plot")
    figs <- TRUE
  }
  xcmd <- ""
  if (!is_empty(input$glm_predict, "none") &&
      (!is_empty(input$glm_pred_data) || !is_empty(input$glm_pred_cmd))) {

    pred_args <- clean_args(glm_pred_inputs(), glm_pred_args[-1])
    pred_args[["prn"]] <- 10
    inp_out[[2 + figs]] <- pred_args

    outputs <- c(outputs,"pred <- predict")
    dataset <- if (input$glm_predict %in% c("data","datacmd")) input$glm_pred_data else input$dataset
    xcmd <-
      # paste0("store_glm(pred, data = '", dataset, "', type = 'prediction', name = '", input$glm_store_pred_name,"')\n") %>%
      paste0("store(pred, data = '", dataset, "', name = '", input$glm_store_pred_name,"')\n") %>%
      paste0("# write.csv(pred, file = '~/glm_predictions.csv', row.names = FALSE)")

    if (input$glm_predict == "cmd") xcmd <- ""

    if (input$glm_pred_plot && !is_empty(input$glm_xvar)) {
      inp_out[[3 + figs]] <- clean_args(glm_pred_plot_inputs(), glm_pred_plot_args[-1])
      inp_out[[3 + figs]]$result <- "pred"
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

observeEvent(input$glm_store_res, {
  req(pressed(input$glm_run))
  robj <- .glm_reg()
  if (!is.list(robj)) return()
  # if (length(robj$model$residuals) != nrow(getdata(input$dataset, filt = "", na.rm = FALSE)))
  #   return(message("The number of residuals is not equal to the number of rows in the data. If the data has missing values these will need to be removed."))
  # store_glm(robj, data = input$dataset, type = "residuals", name = input$glm_store_res_name)
  withProgress(message = 'Storing residuals', value = 0,
    store(robj, name = input$glm_store_res_name)
  )
})

observeEvent(input$glm_store_pred, {
  req(!is_empty(input$glm_pred_data), pressed(input$glm_run))
  pred <- .predict_glm_reg()
  if (is.null(pred)) return()
  # if (nrow(pred) != nrow(getdata(input$glm_pred_data, filt = "", na.rm = FALSE)))
  #   return(message("The number of predicted values is not equal to the number of rows in the data. If the data has missing values these will need to be removed."))
  withProgress(message = 'Storing predictions', value = 0,
    store(pred, data = input$glm_pred_data, name = input$glm_store_pred_name)
  )
})

output$dl_glm_coef <- downloadHandler(
  filename = function() { "glm_coefficients.csv" },
  content = function(file) {
    if (pressed(input$glm_run)) {
      ret <- .glm_reg()[["coeff"]][-1,]
      if ("standardize" %in% input$glm_check) {
        ret$importance <- pmax(ret$OR, 1/ret$OR)
        cat("Standardized coefficients selected\n\n", file = file)
        sshhr(write.table(ret, sep = ",", append = TRUE, file = file, row.names = FALSE))
      } else {
        cat("Standardized coefficients not selected\n\n", file = file)
        sshhr(write.table(ret, sep = ",", append = TRUE, file = file, row.names = FALSE))
      }
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)

output$dl_glm_pred <- downloadHandler(
  filename = function() { "glm_predictions.csv" },
  content = function(file) {
    if (pressed(input$glm_run)) {
      .predict_glm_reg() %>%
        write.csv(file = file, row.names = FALSE)
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)
