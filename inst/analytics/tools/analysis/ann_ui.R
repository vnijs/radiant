# ann_predict <- c("None" = "none", "Data" = "data","Command" = "cmd")
# ann_check <- c("Standardized coefficients" = "standardize",
#                "Stepwise selection" = "stepwise")
# ann_sum_check <- c("VIF" = "vif", "Confidence intervals" = "confint",
#                    "Odds" = "odds")
# ann_plots <- c("None" = "", "Histograms" = "hist",
#                "Scatter" = "scatter", "Dashboard" = "dashboard",
#                "Coefficient plot" = "coef")

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

ann_sum_args <- as.list(if (exists("summary.ann")) formals(summary.ann)
                        else formals(radiant:::summary.ann))

## list of function inputs selected by user
ann_sum_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ann_sum_args))
    ann_sum_args[[i]] <- input[[paste0("ann_",i)]]
  ann_sum_args
})

ann_plot_args <- as.list(if (exists("plot.ann")) formals(plot.ann)
                         else formals(radiant:::plot.ann))

## list of function inputs selected by user
ann_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(ann_plot_args))
    ann_plot_args[[i]] <- input[[paste0("ann_",i)]]
  ann_plot_args
})

# ann_pred_args <- as.list(if (exists("predict.ann_reg")) formals(predict.ann_reg)
#                          else formals(radiant:::predict.ann_reg))

# # list of function inputs selected by user
# ann_pred_inputs <- reactive({
#   # loop needed because reactive values don't allow single bracket indexing
#   for (i in names(ann_pred_args))
#     ann_pred_args[[i]] <- input[[paste0("ann_",i)]]

#   ann_pred_args$pred_cmd <- ann_pred_args$pred_data <- ann_pred_args$pred_vars <- ""
#   if (input$ann_predict == "cmd")
#     ann_pred_args$pred_cmd <- gsub("\\s", "", input$ann_pred_cmd) %>% gsub("\"","\'",.)
#   else if (input$ann_predict == "data")
#     ann_pred_args$pred_data <- input$ann_pred_data
#   else if (input$ann_predict == "vars")
#     ann_pred_args$pred_vars <- input$ann_pred_vars

#   ann_pred_args
# })

# ann_pred_plot_args <- as.list(if (exists("plot.ann_predict")) formals(plot.ann_predict)
#                               else formals(radiant:::plot.ann_predict))

# # list of function inputs selected by user
# ann_pred_plot_inputs <- reactive({
#   # loop needed because reactive values don't allow single bracket indexing
#   for (i in names(ann_pred_plot_args))
#     ann_pred_plot_args[[i]] <- input[[paste0("ann_",i)]]
#   ann_pred_plot_args
# })

output$ui_ann_rvar <- renderUI({
 	vars <- two_level_vars()
  selectInput(inputId = "ann_rvar", label = "Response variable:", choices = vars,
  	selected = isolate(use_input("ann_rvar",vars)), multiple = FALSE)
})

output$ui_ann_lev <- renderUI({
  if (is_empty(input$ann_rvar)) return()
  if (available(input$ann_rvar))
    levs <- .getdata()[[input$ann_rvar]] %>% as.factor %>% levels
  else
    levs <- c()

  selectInput(inputId = "ann_lev", label = "Choose level:",
              choices = levs,
              selected = isolate(use_input_nonvar("ann_lev", levs)))
})

output$ui_ann_evar <- renderUI({
  if (not_available(input$ann_rvar)) return()
	notChar <- "character" != .getclass()
  vars <- varnames()[notChar]
  if (length(vars) > 0)
    vars <- vars[-which(vars == input$ann_rvar)]

  selectInput(inputId = "ann_evar", label = "Explanatory variables:", choices = vars,
    selected = isolate(use_input("ann_evar", vars, fun = "state_multiple")),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

# output$ui_ann_tvar <- renderUI({
#   if (not_available(input$ann_rvar) ||
#       not_available(input$ann_evar)) return()
#   notChar <- "character" != .getclass()
#   vars <- varnames()[notChar]
#   if (length(vars) > 0)
#     vars <- vars[-which(vars == input$ann_rvar)]
#   if (length(vars) > 0)
#     vars <- vars[-which(vars == input$ann_evar)]
#   vars <- c("None",vars)

#   selectInput(inputId = "ann_tvar", label = "Training variable:", choices = vars,
#     selected = isolate(use_input("ann_tvar", vars, fun = "state_single")),
#     multiple = FALSE)
# })

# output$ui_ann_pred_var <- renderUI({
#   vars <- input$ann_evar
#   selectInput("ann_pred_var", label = "Predict for variables:",
#     choices = vars, selected = state_multiple("ann_pred_var", vars),
#     multiple = TRUE, size = min(4, length(vars)), selectize = FALSE)
# })

## X - variable
# output$ui_ann_xvar <- renderUI({
#   vars <- input$ann_evar
#   selectizeInput(inputId = "ann_xvar", label = "X-variable:", choices = vars,
#     selected = state_multiple("ann_xvar",vars),
#     multiple = FALSE)
# })

# output$ui_ann_facet_row <- renderUI({
#   vars <- input$ann_evar
#   vars <- c("None" = ".", vars)
#   selectizeInput("ann_facet_row", "Facet row", vars,
#                  selected = state_single("ann_facet_row", vars, "."),
#                  multiple = FALSE)
# })

# output$ui_ann_facet_col <- renderUI({
#   vars <- input$ann_evar
#   vars <- c("None" = ".", vars)
#   selectizeInput("ann_facet_col", "Facet column", vars,
#                  selected = state_single("ann_facet_col", vars, "."),
#                  multiple = FALSE)
# })

# output$ui_ann_color <- renderUI({
#   vars <- c("None" = "none", input$ann_evar)
#   sel <- state_single("ann_color", vars, "none")
#   selectizeInput("ann_color", "Color", vars, selected = sel,
#                  multiple = FALSE)
# })

output$ui_ann <- renderUI({
  tagList(
    # conditionalPanel(condition = "input.tabs_ann_reg == 'Predict'",
    #   wellPanel(

    #     selectInput("ann_predict", label = "Prediction input:", ann_predict,
    #       selected = state_single("ann_predict", ann_predict, "none")),
    #     conditionalPanel(condition = "input.ann_predict == 'vars'",
    #       uiOutput("ui_ann_pred_var")
    #     ),

    #     # radioButtons(inputId = "ann_predict", label = "Prediction:", ann_predict,
    #     #   selected = state_init("ann_predict", ""), inline = TRUE),
    #     conditionalPanel(condition = "input.ann_predict == 'cmd'",
    #       returnTextAreaInput("ann_pred_cmd", "Prediction command:",
    #         value = state_init("ann_pred_cmd",""))
    #     ),
    #     conditionalPanel(condition = "input.ann_predict == 'data'",
    #       selectizeInput(inputId = "ann_pred_data", label = "Predict for profiles:",
    #                   choices = c("None" = "",r_data$datasetlist),
    #                   selected = state_init("ann_pred_data"), multiple = FALSE)
    #     ),
    #     conditionalPanel(condition = "input.ann_predict != 'none'",
    #       checkboxInput("ann_pred_plot", "Plot predictions", state_init("ann_pred_plot", FALSE)),
    #       conditionalPanel("input.ann_pred_plot == true",
    #         uiOutput("ui_ann_xvar"),
    #         uiOutput("ui_ann_facet_row"),
    #         uiOutput("ui_ann_facet_col"),
    #         uiOutput("ui_ann_color")
    #       )
    #     ),
    #     ## only show if full data is used for prediction
    #     conditionalPanel("input.ann_predict == 'data'",
    #                       # input.ann_pred_data == input.dataset",
    #       tags$table(
    #         tags$td(textInput("ann_store_pred_name", "Store predictions:", "predict_glm")),
    #         tags$td(actionButton("ann_store_pred", "Store"), style="padding-top:30px;")
    #       )
    #     )
    #   )
    # ),
    # conditionalPanel(condition = "input.tabs_ann_reg == 'Plot'",
    #   wellPanel(
    #     selectInput("ann_plots", "GLM plots:", choices = ann_plots,
    #       selected = state_single("ann_plots", ann_plots)),
    #     conditionalPanel(condition = "input.ann_plots == 'coef'",
    #       checkboxInput("ann_intercept", "Include intercept", state_init("ann_intercept", FALSE)))
    #   )
    # ),
    wellPanel(
	    uiOutput("ui_ann_rvar"),
      uiOutput("ui_ann_lev"),
	    uiOutput("ui_ann_evar"),
      # uiOutput("ui_ann_tvar"),
      tags$table(
        tags$td(numericInput("ann_size", label = "Size:", min = 1, max = 20,
          value = state_init("ann_size",1), width = "115px")),
        tags$td(numericInput("ann_decay", label = "Decay:", min = 0, max = 1,
          value = state_init("ann_decay",.5), width = "115px"))
      )
    ),
    wellPanel(
      selectizeInput(inputId = "ann_pred_data", label = "Predict for data:",
         choices = c("None",r_data$datasetlist),
         selected = state_init("ann_pred_data", input$dataset), multiple = FALSE),
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
  plot_height <- 500
  plot_width <- 650
  list(plot_width = plot_width, plot_height = plot_height)
})

ann_plot_width <- function()
  ann_plot() %>% { if (is.list(.)) .$plot_width else 650 }

ann_plot_height <- function()
  ann_plot() %>% { if (is.list(.)) .$plot_height else 500 }

# output is called from the main radiant ui.R
output$ann <- renderUI({

		register_print_output("summary_ann", ".summary_ann")
    register_plot_output("plot_ann_net", ".plot_ann_net",
                          height_fun = "ann_plot_height",
                          width_fun = "ann_plot_width")

		register_plot_output("plot_ann", ".plot_ann",
                          height_fun = "ann_plot_height",
                          width_fun = "ann_plot_width")

		# two separate tabs
		ann_output_panels <- tabsetPanel(
	    id = "tabs_ann",
	    tabPanel("Summary",
               downloadLink("dl_ann_pred", "", class = "fa fa-download alignright"), br(),
               verbatimTextOutput("summary_ann"),
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

.ann <- reactive({
	do.call(ann, ann_inputs())
})

.summary_ann <- reactive({
  if (ann_available() != "available") return(ann_available())
  do.call(summary, c(list(object = .ann()), ann_sum_inputs()))
})

.plot_ann <- reactive({
  if (ann_available() != "available") return(ann_available())

  ann_plot_inputs() %>% {.$shiny <- TRUE; .} %>%
    {do.call(plot, c(list(x = .ann()), .))}
})

.plot_ann_net <- reactive({
  if (ann_available() != "available") return(ann_available())
  .ann() %>%
    { if (is.character(.)) .
      else capture_plot( do.call(NeuralNetTools::plotnet, list(mod_in = .$model)) ) }
})

observeEvent(input$ann_reg_report, {
  # isolate({
  #   ## find a way to have the predict call **not** to use 'result'
  #   # outputs <- c("summary", "# store_glm")
  #   outputs <- c("summary")
  #   inp_out <- list("","")
  #   inp_out[[1]] <- clean_args(ann_sum_inputs(), ann_sum_args[-1])
  #   figs <- FALSE
  #   if (!is_empty(input$ann_plots)) {
  #     inp_out[[2]] <- clean_args(ann_plot_inputs(), ann_plot_args[-1])
  #     outputs <- c(outputs, "plot")
  #     figs <- TRUE
  #   }
  #   xcmd <- ""
  #   if (!is.null(r_data$ann_pred) && !is_empty(input$ann_predict, "none")) {
  #     inp_out[[2 + figs]] <- clean_args(ann_pred_inputs(), ann_pred_args[-1])
  #     outputs <- c(outputs,"result <- predict")
  #     dataset <- if (input$ann_predict == "data") input$ann_pred_data else input$dataset
  #     xcmd <-
  #       paste0("# store_glm(result, data = '", dataset, "', type = 'prediction', name = '", input$ann_store_pred_name,"')\n") %>%
  #       paste0("# write.csv(result, file = '~/ann_predictions.csv', row.names = FALSE)")
  #     if (!is_empty(input$ann_xvar)) {
  #       inp_out[[3 + figs]] <- clean_args(ann_pred_plot_inputs(), ann_pred_plot_args[-1])
  #       outputs <- c(outputs, "plot")
  #       figs <- TRUE
  #     }
  #   }

  #   update_report(inp_main = clean_args(ann_inputs(), ann_args),
  #                 fun_name = "ann_reg",
  #                 inp_out = inp_out,
  #                 outputs = outputs,
  #                 figs = figs,
  #                 fig.width = round(7 * ann_plot_width()/650,2),
  #                 fig.height = round(7 * ann_plot_height()/650,2),
  #                 xcmd = xcmd)
  # })
})

observeEvent(input$ann_pred, {
  isolate({
    if (ann_available() != "available") return(ann_available())
    if (is_empty(input$ann_pred_data,"None")) return("No data selected for prediction")
    pred <- predict(.ann()$model, getdata(input$ann_pred_data, filt = "", na.rm = FALSE))
    # if (nrow(pred) != nrow(getdata(input$ann_pred_data, filt = "", na.rm = FALSE)))
      # return(message("The number of predicted values is not equal to the number of rows in the data. If the data has missing values these will need to be removed."))
    store_ann(pred[,1], data = input$ann_pred_data, name = input$ann_pred_name)
  })
})

output$dl_ann_pred <- downloadHandler(
  filename = function() { "ann_predictions.csv" },
  content = function(file) {
    if (ann_available() != "available") {
      write.csv(ann_available(), file = file, row.names = FALSE)
    } else {
      predict(.ann()$model, getdata(input$ann_pred_data, filt = "", na.rm = FALSE)) %>%
        set_colnames(c("predict_ann")) %>%
        write.csv(file = file, row.names = FALSE)
    }
  }
)
