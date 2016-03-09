# list of function arguments
crs_args <- as.list(formals(crs))

crs_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  crs_args$data_filter <- if (input$show_filter) input$data_filter else ""
  crs_args$dataset <- input$dataset
  for (i in r_drop(names(crs_args)))
    crs_args[[i]] <- input[[paste0("crs_",i)]]
  crs_args
})

###############################################################
# Evaluate model performance
###############################################################
output$ui_crs_id <- renderUI({
  vars <- varnames()
  selectInput(inputId = "crs_id", label = "User id:", choices = vars,
    selected = state_single("crs_id", vars), multiple = FALSE)
})

# output$ui_crs_uid <- renderUI({
#   req(input$crs_id)
#   if (available(input$crs_id))
#     levs <- .getdata()[[input$crs_id]] %>% as.factor %>% levels
#   else
#     levs <- c()

#   selectInput(inputId = "crs_uid", label = "Choose users to predict for:",
#               choices = levs,
#               selected = use_input_nonvar("crs_uid", levs),
#               multiple = TRUE, size = min(3, length(levs)),
#               selectize = FALSE)
# })

output$ui_crs_prod <- renderUI({
  req(available(input$crs_id))
  vars <- varnames()
  vars <- vars[-which(input$crs_id %in% vars)]

  selectInput(inputId = "crs_prod", label = "Product id:", choices = vars,
    selected = state_single("crs_prod", vars), multiple = FALSE)
})

# output$ui_crs_train <- renderUI({
#   req(input$crs_prod)
#   if (available(input$crs_prod))
#     levs <- .getdata()[[input$crs_prod]] %>% as.factor %>% levels
#   else
#     levs <- c()

#   selectInput(inputId = "crs_train", label = "Choose training products:",
#               choices = levs,
#               selected = use_input_nonvar("crs_train", levs),
#               multiple = TRUE, size = min(3, length(levs)),
#               selectize = FALSE)
# })

output$ui_crs_pred <- renderUI({
  req(input$crs_prod)
  if (available(input$crs_prod))
    levs <- .getdata()[[input$crs_prod]] %>% as.factor %>% levels
  else
    levs <- c()

  selectInput(inputId = "crs_pred", label = "Choose prediction products:",
              choices = levs,
              # selected = use_input_nonvar("crs_pred", levs),
              selected = state_init("crs_pred", levs),
              multiple = TRUE, size = min(3, length(levs)),
              selectize = FALSE)
})

output$ui_crs_rate <- renderUI({
  req(input$crs_prod)
  vars <- varnames()
  vars <- vars[-which(c(input$crs_id, input$crs_prod) %in% vars)]

  selectInput(inputId = "crs_rate", label = "Ratings variable:", choices = vars,
    selected = state_single("crs_rate", vars), multiple = FALSE)
})

# observe({
#   print("---")
#   print(input$crs_pause)
#   print("---")
#   print("crs_pause" %in% names(input))
#   # print(names(input) %in% "crs_pause")
#   print("-=-")
# })

# output$ui_crs_pause <- renderUI({
#   checkboxInput("crs_pause", "Pause prediction", state_init2("crs_pause", FALSE))
# })


output$ui_crs <- renderUI({
  req(input$dataset)
  tagList(
  	wellPanel(
      checkboxInput("crs_pause", "Pause prediction", state_init("crs_pause", FALSE)),
	    # uiOutput("ui_crs_pause"),
      uiOutput("ui_crs_id"),
      # uiOutput("ui_crs_uid"),
      uiOutput("ui_crs_prod"),
      # uiOutput("ui_crs_train"),
      uiOutput("ui_crs_pred"),
      uiOutput("ui_crs_rate")
  	),
    ## to store results
    # wellPanel(
    #   tags$table(
    #     tags$td(textInput("crs_name", "Store predictions:", "avg cf ravg rcf")),
    #     tags$td(actionButton("crs_store_pred", "Store"), style="padding-top:30px;")
    #   )
    # ),
    help_and_report(modal_title = "Collaborative Filtering",
  	                fun_name = "crs",
  	                help_file = inclMD(file.path(r_path,"analytics/tools/help/crs.md")))
	)
})

crs_plot <- reactive({
  plot_height <- 500
  plot_width <- 650
  list(plot_width = plot_width, plot_height = plot_height)
})

crs_plot_width <- function()
  crs_plot() %>% { if (is.list(.)) .$plot_width else 650 }

crs_plot_height <- function()
  crs_plot() %>% { if (is.list(.)) .$plot_height else 500 }

# output is called from the main radiant ui.R
output$crs <- renderUI({
	register_print_output("summary_crs", ".summary_crs")
	register_plot_output("plot_crs", ".plot_crs",
                       	width_fun = "crs_plot_width",
                       	height_fun = "crs_plot_height")

	# one output with components stacked
  crs_output_panels <- tabsetPanel(
     id = "tabs_crs",
     tabPanel("Summary",
       verbatimTextOutput("summary_crs")
     ),
     tabPanel("Plot",
       plot_downloader("crs", height = perf_plot_height()),
       plotOutput("plot_crs", height = "100%")
    )
  )

	stat_tab_panel(menu = "Model",
	              tool = "Collaborative Filter",
	              tool_ui = "ui_crs",
	             	output_panels = crs_output_panels)
})

.crs <- reactive({
  req(available(input$crs_id))
  # req(input$crs_pause == FALSE, cancelOutput = TRUE)

	do.call(crs, crs_inputs())
})

.summary_crs <- reactive({
  if(length(input$crs_pred) < 1) return("Please select one or more products to predict for")
  summary(.crs())
})

.plot_crs <- reactive({
  req(available(input$crs_id))
  if(length(input$crs_pred) < 1) return("Please select one or more products to predict for")
  plot.crs(.crs(), shiny = TRUE)
})

## Add reporting option
# observeEvent(input$performance_report, {
#   if (length(input$perf_plots) > 0) {
#     inp_out <- list(plots = input$perf_plots) %>% list("",.)
#     outputs <- c("summary","plot")
#     figs <- TRUE
#   } else {
#     outputs <- c("summary")
#     inp_out <- list("","")
#     figs <- FALSE
#   }
#   update_report(inp_main = clean_args(perf_inputs(), perf_args),
#                 fun_name = "performance",
#                 inp_out = inp_out,
#                 outputs = outputs,
#                 figs = figs,
#                 fig.width = round(7 * perf_plot_width()/650,2),
#                 fig.height = round(7 * perf_plot_height()/650,2))
# })

# output$dl_perf_tab <- downloadHandler(
#   filename = function() { "performance.csv" },
#   content = function(file) {
#     do.call(summary, c(list(object = .performance()), perf_inputs(),
#             list(prn = FALSE))) %>%
#       write.csv(., file = file, row.names = FALSE)
#   }
# )

## Store results
# observeEvent(input$ann_pred, {
#   if (ann_available() != "available") return(ann_available())
#   if (is_empty(input$ann_pred_data,"None")) return("No data selected for prediction")
#   pred <- predict(.ann(), input$ann_pred_data)
#   store_ann(pred, data = input$ann_pred_data, name = input$ann_pred_name)
# })

## Download predictions
# output$dl_ann_pred <- downloadHandler(
#   filename = function() { "ann_predictions.csv" },
#   content = function(file) {
#     if (ann_available() != "available") {
#       write.csv(ann_available(), file = file, row.names = FALSE)
#     } else {
#       data.frame(pred_ann = predict(.ann(), input$ann_pred_data)) %>%
#         write.csv(file = file, row.names = FALSE)
#     }
#   }
# )
