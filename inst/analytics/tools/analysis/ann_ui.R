## list of function arguments
ann_args <- as.list(formals(ann))

## list of function inputs selected by user
ann_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  ann_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ann_args$dataset <- input$dataset
  for (i in r_drop(names(ann_args)))
    ann_args[[i]] <- input[[paste0("ann_",i)]]
  ann_args
})

output$ui_ann_rvar <- renderUI({
 	vars <- two_level_vars()
  selectInput(inputId = "ann_rvar", label = "Response variable:", choices = vars,
  	selected = state_single("ann_rvar",vars), multiple = FALSE)
})

output$ui_ann_lev <- renderUI({
  if (is_empty(input$ann_rvar)) return()
  if (available(input$ann_rvar))
    levs <- .getdata()[[input$ann_rvar]] %>% as.factor %>% levels
  else
    levs <- c()

  selectInput(inputId = "ann_lev", label = "Choose level:",
              choices = levs,
              selected = state_init("ann_lev"))
              # selected = use_input_nonvar("ann_lev", levs))
})

output$ui_ann_evar <- renderUI({
  if (not_available(input$ann_rvar)) return()
	notChar <- "character" != .getclass()
  vars <- varnames()[notChar]
  if (length(vars) > 0)
    vars <- vars[-which(vars == input$ann_rvar)]

  selectInput(inputId = "ann_evar", label = "Explanatory variables:", choices = vars,
    # selected = use_input("ann_evar", vars, fun = "state_multiple"),
    selected = state_multiple("ann_evar", vars, input$glm_evar),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_ann_wts <- renderUI({
  # req(available(input$ann_rvar), available(input$ann_evar))
  isNum <- .getclass() %in% c("numeric","integer")
  vars <- varnames()[isNum]
  if (length(vars) > 0 && any(vars %in% input$ann_evar)) {
    vars <- setdiff(vars, input$ann_evar)
    names(vars) <- varnames() %>% {.[match(vars, .)]} %>% names
  }
  vars <- c("None", vars)

  selectInput(inputId = "ann_wts", label = "Weights:", choices = vars,
    # selected = use_input("ann_wts", vars),
    selected = state_single("ann_wts", vars),
    multiple = FALSE)
})


output$ui_ann <- renderUI({
  # req(input$dataset)
  tagList(
    wellPanel(
      actionButton("ann_run", "Estimate", width = "100%")
    ),
    wellPanel(
      # checkboxInput("ann_pause", "Pause estimation", state_init("ann_pause", FALSE)),
	    uiOutput("ui_ann_rvar"),
      uiOutput("ui_ann_lev"),
	    uiOutput("ui_ann_evar"),
      uiOutput("ui_ann_wts"),
      tags$table(
        tags$td(numericInput("ann_size", label = "Size:", min = 1, max = 20,
          value = state_init("ann_size",1), width = "77px")),
        tags$td(numericInput("ann_decay", label = "Decay:", min = 0, max = 1,
          step = .1, value = state_init("ann_decay",.5), width = "77px")),
        tags$td(numericInput("ann_seed", label = "Seed:",
          value = state_init("ann_seed", 1234), width = "77px"))
      )
    ),
    wellPanel(
      selectizeInput(inputId = "ann_pred_data", label = "Predict for data:",
         choices = c("None",r_data$datasetlist),
         selected = input$dataset, multiple = FALSE),
         # selected = state_init("ann_pred_data", input$dataset), multiple = FALSE),
      tags$table(
        tags$td(textInput("ann_pred_name", "Store predictions:", "predict_ann")),
        tags$td(actionButton("ann_pred", "Store"), style="padding-top:30px;")
      )
    ),
  	help_and_report(modal_title = "Neural Network (ANN)",
  	                fun_name = "ann",
  	                help_file = inclMD(file.path(r_path,"analytics/tools/help/ann.md")))
	)
})

ann_plot <- reactive({

  if (ann_available() != "available") return()
  plot_height <- max(500, length(.ann()$model$coefnames) * 15)
  plot_width <- 650
  list(plot_width = plot_width, plot_height = plot_height)
})

ann_plot_width <- function()
  ann_plot() %>% { if (is.list(.)) .$plot_width else 650 }

ann_plot_height <- function()
  ann_plot() %>% { if (is.list(.)) .$plot_height else 500 }

## output is called from the main radiant ui.R
output$ann <- renderUI({

		register_print_output("summary_ann", ".summary_ann")
    register_plot_output("plot_ann_net", ".plot_ann_net",
                          height_fun = "ann_plot_height",
                          width_fun = "ann_plot_width")

		register_plot_output("plot_ann", ".plot_ann",
                          height_fun = "ann_plot_height",
                          width_fun = "ann_plot_width")

		## two separate tabs
		ann_output_panels <- tabsetPanel(
	    id = "tabs_ann",
	    tabPanel("Summary",
               downloadLink("dl_ann_pred", "", class = "fa fa-download alignright"), br(),
               verbatimTextOutput("summary_ann"),
               plot_downloader("ann_net", height = ann_plot_height()),
               plotOutput("plot_ann_net", width = "100%", height = "100%")),
	    tabPanel("Plot", plot_downloader("ann", height = ann_plot_height()),
               plotOutput("plot_ann", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Model",
		              tool = "ANN",
		              tool_ui = "ui_ann",
		             	output_panels = ann_output_panels)

})

ann_available <- reactive({
  if (not_available(input$ann_rvar))
    return("This analysis requires a response variable with two levels and one\nor more explanatory variables. If these variables are not available\nplease select another dataset.\n\n" %>% suggest_data("titanic"))

  if (not_available(input$ann_evar))
    return("Please select one or more explanatory variables.\n\n" %>% suggest_data("titanic"))

  "available"
})

# .ann <- eventReactive(input$ann_run | input$ann_pause == TRUE, {
.ann <- eventReactive(input$ann_run, {
  withProgress(message = 'Estimating model', value = 0,
	  do.call(ann, ann_inputs())
  )
})

.summary_ann <- reactive({
  if (ann_available() != "available") return(ann_available())
  if (not_pressed(input$ann_run)) return("** Press the Estimate button to estimate the model **")

  summary(.ann())
})

.plot_ann <- reactive({
  if (ann_available() != "available") return(ann_available())
  if (not_pressed(input$ann_run)) return("** Press the Estimate button to estimate the model **")
  plot(.ann(), shiny = TRUE)
})

.plot_ann_net <- reactive({
  if (ann_available() != "available") return(invisible())
  .ann() %>%
    { if (is.character(.)) invisible()
      else capture_plot( do.call(NeuralNetTools::plotnet, list(mod_in = .$model)) ) }
})

observeEvent(input$ann_report, {
  outputs <- c("summary","plot")
  inp_out <- list("","")
  xcmd <- "NeuralNetTools::plotnet(result$model)\n"
  xcmd <- paste0(xcmd, "pred <- predict(result,'", input$ann_pred_data,"')\n")
  xcmd <-  paste0(xcmd, "store(pred, data = '", input$ann_pred_data, "', name = '", input$ann_pred_name,"')\n")
  xcmd <-  paste0(xcmd, "# write.csv(pred, file = '~/ann_predictions.csv', row.names = FALSE)")
  update_report(inp_main = clean_args(ann_inputs(), ann_args),
                fun_name = "ann",
                inp_out = inp_out,
                outputs = outputs,
                figs = TRUE,
                fig.width = ann_plot_width(),
                fig.height = ann_plot_height(),
                xcmd = xcmd)
})

observeEvent(input$ann_pred, {
  if (ann_available() != "available") return(ann_available())
  if (is_empty(input$ann_pred_data,"None")) return("No data selected for prediction")
  withProgress(message = 'Storing predictions', value = 0,
    predict(.ann(), input$ann_pred_data) %>%
    store(data = input$ann_pred_data, name = input$ann_pred_name)
  )
})

output$dl_ann_pred <- downloadHandler(
  filename = function() { "ann_predictions.csv" },
  content = function(file) {
    if (pressed(input$ann_run)) {
      data.frame(pred_ann = predict(.ann(), input$ann_pred_data)) %>%
        write.csv(file = file, row.names = FALSE)
    } else {
      cat("No output available. Press the Estimate button to generate results", file = file)
    }
  }
)
