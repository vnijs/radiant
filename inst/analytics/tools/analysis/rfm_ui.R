# rfm_method <- list("xtile" = "xtile", "ntile" = "ntile")
# rfm_plots <- list("Lift" = "lift", "Gains" = "gains")
# rfm_train <- list("Training" = "train", "Validation" = "valid")

# # list of function arguments
# rfm_args <- as.list(formals(performance))

# rfm_inputs <- reactive({
#   # loop needed because reactive values don't allow single bracket indexing
#   rfm_args$data_filter <- if (input$show_filter) input$data_filter else ""
#   rfm_args$dataset <- input$dataset
#   for (i in r_drop(names(rfm_args)))
#     rfm_args[[i]] <- input[[paste0("rfm_",i)]]
#   rfm_args
# })

# ###############################################################
# # Evaluate model performance
# ###############################################################
# output$ui_rfm_rvar <- renderUI({

#   vars <- two_level_vars()
#   selectInput(inputId = "rfm_rvar", label = "Response variable:", choices = vars,
#     selected = state_single("rfm_rvar",vars), multiple = FALSE)
# })

# output$ui_rfm_lev <- renderUI({
#   levs <- c()
#   if (available(input$rfm_rvar))
#     levs <- .getdata()[[input$rfm_rvar]] %>% as.factor %>% levels

#   isolate({
#     sel <-
#       input$rfm_lev %>%
#       {if (!is_empty(.) && . %in% levs) . else levs[1]}
#   })

#   selectInput(inputId = "rfm_lev", label = "Choose level:",
#               choices = levs,
#               selected = state_single("rfm_lev", levs, sel))
# })

# output$ui_rfm_pred <- renderUI({
#   isNum <- .getclass() %in% c("integer","numeric")
#   vars <- varnames()[isNum]
#   # if (length(vars) > 0 && !is_empty(input$rfm_rvar) && input$rfm_rvar %in% vars)
#   # if (length(vars) > 0)
#     # vars <- vars[-which(vars == input$rfm_rvar)]

#   # isolate({
#   #   init <- input$rfm_pred %>%
#   #   {if (!is_empty(.) && . %in% vars) . else character(0)}
#   #   if (length(init) > 0) r_state$rfm_pred <<- init
#   # })

#   isolate({
#     sel <- use_input("rfm_pred", vars, fun = "state_multiple")
#     # selected = state_multiple("rfm_pred", vars, sel),
#   })

#   selectInput(inputId = "rfm_pred", label = "Predictor:", choices = vars,
#     selected = sel,
#     multiple = TRUE, size = min(4, length(vars)), selectize = FALSE)
# })

# output$ui_rfm_train <- renderUI({

#   vars <- c("None", two_level_vars())

#   isolate({
#     sel <- use_input("rfm_train", vars, "None")
#   })

#   selectInput(inputId = "rfm_train", label = "Training variable:", choices = vars,
#     selected = sel, multiple = FALSE)
# })

# output$ui_performance <- renderUI({
#   tagList(
#   	wellPanel(
# 	    uiOutput("ui_rfm_rvar"),
#       uiOutput("ui_rfm_lev"),
#       uiOutput("ui_rfm_pred"),
#       checkboxGroupInput("rfm_plots", "Plots:", rfm_plots,
#         selected = state_init("rfm_plots", ""),
#         inline = TRUE),
#       radioButtons("rfm_method", label = "Method:", rfm_method,
#         selected = state_init("rfm_method", "xtile"),
#         inline = TRUE),
#       numericInput("rfm_qnt", label = "# quantiles:",
#                    value = state_init("rfm_qnt", 10), min = 2)
#       # , uiOutput("ui_rfm_train"),
#       # conditionalPanel("input.rfm_train != 'None'",
#       #   checkboxGroupInput("rfm_tplots", "Show plots for:", rfm_train,
#       #     selected = state_init("rfm_tplots", ""),
#       #     inline = TRUE)
#       # )
#   	),
#   	help_and_report(modal_title = "Model performance",
#   	                fun_name = "performance",
#   	                help_file = inclMD(file.path(r_path,"analytics/tools/help/performance.md")))
# 	)
# })

# rfm_plot_width <- function() {
#   ifelse(length(input$rfm_pred) > 1, 600, 500)
# }
# rfm_plot_height <- function() {
#   length(input$rfm_plots) * 500
# }

# # output is called from the main radiant ui.R
# output$performance <- renderUI({

# 		register_print_output("summary_performance", ".summary_performance")
# 		register_plot_output("plot_performance", ".plot_performance",
#                          	width_fun = "rfm_plot_width",
#                          	height_fun = "rfm_plot_height")

# 		# one output with components stacked
# 		rfm_output_panels <- tagList(
#        downloadLink("dl_rfm_tab", "", class = "fa fa-download alignright"), br(),
# 	     tabPanel("Summary", verbatimTextOutput("summary_performance")),
# 	     tabPanel("Plot",
#                 plot_downloader("performance", height = rfm_plot_height()),
#                 plotOutput("plot_performance", height = "100%"))
# 	  )

# 		stat_tab_panel(menu = "Model",
# 		              tool = "Model performance",
# 		              tool_ui = "ui_performance",
# 		             	output_panels = rfm_output_panels)

# })

# .performance <- reactive({
# 	do.call(performance, rfm_inputs())
# })

# .summary_performance <- reactive({
#   if (not_available(input$rfm_rvar) || not_available(input$rfm_pred) ||
#       is_empty(input$rfm_lev))
#     return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
#   summary(.performance())
# })

# .plot_performance <- reactive({
#   if (not_available(input$rfm_rvar) || not_available(input$rfm_pred) ||
#       is_empty(input$rfm_lev)) {
#     return(" ")
#   }
#   perf <- .performance()
#   perf %>% plot(plots = input$rfm_plots, shiny = TRUE)
# })

# observeEvent(input$performance_report, {
#   isolate({
#     if (length(input$rfm_plots) > 0) {
#       inp_out <- list(plots = input$rfm_plots) %>% list("",.)
#       outputs <- c("summary","plot")
#       figs <- TRUE
#     } else {
#       outputs <- c("summary")
#       inp_out <- list("","")
#       figs <- FALSE
#     }
#     update_report(inp_main = clean_args(rfm_inputs(), rfm_args),
#                   fun_name = "performance",
#                   inp_out = inp_out,
#                   outputs = outputs,
#                   figs = figs,
#                   fig.width = round(7 * rfm_plot_width()/650,2),
#                   fig.height = round(7 * rfm_plot_height()/650,2))
#   })
# })

# output$dl_rfm_tab <- downloadHandler(
#   filename = function() { "performance.csv" },
#   content = function(file) {
#     do.call(summary, c(list(object = .performance()), rfm_inputs(),
#             list(prn = FALSE))) %>%
#       write.csv(., file = file, row.names = FALSE)
#   }
# )
