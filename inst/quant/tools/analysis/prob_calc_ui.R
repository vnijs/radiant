# alternative hypothesis options
# pc_alt <- c("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
# pc_plots <- c("Histogram" = "hist", "Simulate" = "simulate")

pc_dist <- c("Normal" = "normal")

# list of function arguments
# pc_args <- as.list(formals(prob_calc))

# list of function inputs selected by user
# sm_inputs <- reactive({
#   # loop needed because reactive values don't allow single bracket indexing
#   sm_args$data_filter <- if (input$show_filter) input$data_filter else ""
#   sm_args$dataset <- input$dataset
#   for (i in r_drop(names(sm_args)))
#     sm_args[[i]] <- input[[paste0("sm_",i)]]
#   sm_args
# })

# output$ui_pc_var <- renderUI({
#   isNum <- "numeric" == .getclass() | "integer" == .getclass()
#   vars <- varnames()[isNum]
#   selectInput(inputId = "sm_var", label = "Variable (select one):",
#     choices = vars, selected = state_single("sm_var",vars), multiple = FALSE)
# })

output$ui_prob_calc <- renderUI({
  tagList(
    wellPanel(
      selectizeInput("pc_dist", label = "Distribution:",
        choices = pc_dist,
        selected = state_single("pc_dist", pc_dist, "normal"),
        multiple = FALSE),
    	numericInput("pc_mean", "Mean:", state_init('pc_mean',0)),
      numericInput("pc_stdev", "St. dev:", state_init('pc_stdev',1)),
      sliderInput('pc_range',"Range:", min = -3, max = max,
        value = state_init('pc_range',c(-3,3), step = 0.1))
    )
  	# help_and_report(modal_title = 'Single mean', fun_name = 'single_mean',
  	#                 help_file = inclMD(file.path(r_path,"quant/tools/help/single_mean.md")))
 	)
})

pc_plot_width <- function() 600

pc_plot_height <- function() 400

# output$sample_size <- renderUI({

#     register_print_output("summary_sample_size", ".summary_sample_size")

#     ## one output with components stacked
#     ss_output_panels <- tagList(
#        tabPanel("Summary", verbatimTextOutput("summary_sample_size"))
#     )

#     stat_tab_panel(menu = "Sample",
#                   tool = "Sample size",
#                   data = NULL,
#                   tool_ui = "ui_sample_size",
#                   output_panels = ss_output_panels)
# })

output$prob_calc <- renderUI({

    register_print_output("summary_prob_calc", ".summary_prob_calc")
    register_plot_output("plot_prob_calc", ".plot_prob_calc",
                          width_fun = "pc_plot_width",
                          height_fun = "pc_plot_height")

    # one output with components stacked
    pc_output_panels <- tagList(
       tabPanel("Summary", verbatimTextOutput("summary_prob_calc")),
       tabPanel("Plot", plotOutput("plot_prob_calc", height = "100%"))
    )

    stat_tab_panel(menu = "Sample",
                   tool = "Probability calculator",
                  tool_ui = "ui_prob_calc",
                  output_panels = pc_output_panels)

})

.prob_calc <- reactive({
  1:10
  # do.call(proc_calc, sm_inputs())
})

.summary_prob_calc <- reactive({

  # if (not_available(input$sm_var))
  #   return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")

  # if (is.na(input$sm_comp_value))
  #   return("Please choose a comparison value")

  summary(.prob_calc())
})

.plot_prob_calc <- reactive({

  # if (not_available(input$sm_var))
  #   return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")

  # if (is.na(input$sm_comp_value))
  #   return("Please choose a comparison value")

  # plot(.proc_calc(), shiny = TRUE)
  plot(.prob_calc())
})

# observe({
#   if (not_pressed(input$single_mean_report)) return()
#   isolate({
#     outputs <- c("summary","plot")
#     inp_out <- list(plots = input$sm_plots) %>% list("",.)
#     figs <- TRUE
#     if (length(input$sm_plots) == 0) {
#       figs <- FALSE
#       outputs <- c("summary")
#       inp_out <- list("","")
#     }
#     update_report(inp_main = clean_args(sm_inputs(), sm_args),
#                   fun_name = "single_mean", inp_out = inp_out,
#                   outputs = outputs, figs = figs,
#                   fig.width = round(7 * sm_plot_width()/650,2),
#                   fig.height = round(7 * sm_plot_height()/650,2))
#   })
# })
