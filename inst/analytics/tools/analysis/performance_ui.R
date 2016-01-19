perf_method <- list("xtile" = "xtile", "ntile" = "ntile")
perf_plots <- list("Lift" = "lift", "Gains" = "gains")
perf_train <- list("Training" = "train", "Validation" = "valid")

# list of function arguments
perf_args <- as.list(formals(performance))

perf_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  perf_args$data_filter <- if (input$show_filter) input$data_filter else ""
  perf_args$dataset <- input$dataset
  for (i in r_drop(names(perf_args)))
    perf_args[[i]] <- input[[paste0("perf_",i)]]
  perf_args
})

###############################################################
# Evaluate model performance
###############################################################
output$ui_perf_rvar <- renderUI({

  vars <- two_level_vars()
  selectInput(inputId = "perf_rvar", label = "Response variable:", choices = vars,
    selected = state_single("perf_rvar",vars), multiple = FALSE)
})

output$ui_perf_lev <- renderUI({
  levs <- c()
  if (available(input$perf_rvar))
    levs <- .getdata()[[input$perf_rvar]] %>% as.factor %>% levels

  isolate({
    sel <-
      input$perf_lev %>%
      {if (!is_empty(.) && . %in% levs) . else levs[1]}
  })

  selectInput(inputId = "perf_lev", label = "Choose level:",
              choices = levs,
              selected = state_single("perf_lev", levs, sel))
})

output$ui_perf_pred <- renderUI({
  isNum <- .getclass() %in% c("integer","numeric")
  vars <- varnames()[isNum]
  # if (length(vars) > 0 && !is_empty(input$perf_rvar) && input$perf_rvar %in% vars)
  # if (length(vars) > 0)
    # vars <- vars[-which(vars == input$perf_rvar)]

  # isolate({
  #   init <- input$perf_pred %>%
  #   {if (!is_empty(.) && . %in% vars) . else character(0)}
  #   if (length(init) > 0) r_state$perf_pred <<- init
  # })

  isolate({
    sel <- use_input("perf_pred", vars, fun = "state_multiple")
    # selected = state_multiple("perf_pred", vars, sel),
  })

  selectInput(inputId = "perf_pred", label = "Predictor:", choices = vars,
    selected = sel,
    multiple = TRUE, size = min(4, length(vars)), selectize = FALSE)
})

output$ui_perf_train <- renderUI({

  vars <- c("None", two_level_vars())

  isolate({
    sel <- use_input("perf_train", vars, "None")
  })

  selectInput(inputId = "perf_train", label = "Training variable:", choices = vars,
    selected = sel, multiple = FALSE)
})

output$ui_performance <- renderUI({
  tagList(
  	wellPanel(
	    uiOutput("ui_perf_rvar"),
      uiOutput("ui_perf_lev"),
      uiOutput("ui_perf_pred"),
      checkboxGroupInput("perf_plots", "Plots:", perf_plots,
        selected = state_init("perf_plots", ""),
        inline = TRUE),
      radioButtons("perf_method", label = "Method:", perf_method,
        selected = state_init("perf_method", "xtile"),
        inline = TRUE),
      numericInput("perf_qnt", label = "# quantiles:",
                   value = state_init("perf_qnt", 10), min = 2)
      # , uiOutput("ui_perf_train"),
      # conditionalPanel("input.perf_train != 'None'",
      #   checkboxGroupInput("perf_tplots", "Show plots for:", perf_train,
      #     selected = state_init("perf_tplots", ""),
      #     inline = TRUE)
      # )
  	),
  	help_and_report(modal_title = "Model performance",
  	                fun_name = "performance",
  	                help_file = inclMD(file.path(r_path,"analytics/tools/help/performance.md")))
	)
})

perf_plot_width <- function() {
  ifelse(length(input$perf_pred) > 1, 600, 500)
}
perf_plot_height <- function() {
  length(input$perf_plots) * 500
}

# output is called from the main radiant ui.R
output$performance <- renderUI({

		register_print_output("summary_performance", ".summary_performance")
		register_plot_output("plot_performance", ".plot_performance",
                         	width_fun = "perf_plot_width",
                         	height_fun = "perf_plot_height")

		# one output with components stacked
		perf_output_panels <- tagList(
       downloadLink("dl_perf_tab", "", class = "fa fa-download alignright"), br(),
	     tabPanel("Summary", verbatimTextOutput("summary_performance")),
	     tabPanel("Plot",
                plot_downloader("performance", height = perf_plot_height()),
                plotOutput("plot_performance", height = "100%"))
	  )

		stat_tab_panel(menu = "Model",
		              tool = "Model performance",
		              tool_ui = "ui_performance",
		             	output_panels = perf_output_panels)

})

.performance <- reactive({
	do.call(performance, perf_inputs())
})

.summary_performance <- reactive({
  if (not_available(input$perf_rvar) || not_available(input$perf_pred) ||
      is_empty(input$perf_lev))
    return("This analysis requires a response variable of type factor and one or more\npredictors of type numeric. If these variable types are not available please\nselect another dataset.\n\n" %>% suggest_data("titanic"))
  summary(.performance())
})

.plot_performance <- reactive({
  if (not_available(input$perf_rvar) || not_available(input$perf_pred) ||
      is_empty(input$perf_lev)) {
    return(" ")
  }
  perf <- .performance()
  perf %>% plot(plots = input$perf_plots, shiny = TRUE)
})

observeEvent(input$performance_report, {
  isolate({
    if (length(input$perf_plots) > 0) {
      inp_out <- list(plots = input$perf_plots) %>% list("",.)
      outputs <- c("summary","plot")
      figs <- TRUE
    } else {
      outputs <- c("summary")
      inp_out <- list("","")
      figs <- FALSE
    }
    update_report(inp_main = clean_args(perf_inputs(), perf_args),
                  fun_name = "performance",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * perf_plot_width()/650,2),
                  fig.height = round(7 * perf_plot_height()/650,2))
  })
})

output$dl_perf_tab <- downloadHandler(
  filename = function() { "performance.csv" },
  content = function(file) {
    do.call(summary, c(list(object = .performance()), perf_inputs(),
            list(prn = FALSE))) %>%
      write.csv(., file = file, row.names = FALSE)
  }
)
