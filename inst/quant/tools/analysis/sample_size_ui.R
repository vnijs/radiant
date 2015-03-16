###############################
# Sample size
###############################
ss_type <- c("Mean" = "mean", "Proportion" = "proportion")
ss_pop_correction <- c("Yes" = "yes", "No" = "no")

# list of function arguments
ss_args <- as.list(formals(sample_size))

# list of function inputs selected by user
ss_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(ss_args))
    ss_args[[i]] <- input[[i]]
  ss_args
})

output$ui_sample_size <- renderUI({
  tagList(
  	wellPanel(
		  radioButtons(inputId = "ss_type", label = NULL, choices = ss_type,
	  	  selected = state_init("ss_type", "mean"), inline = TRUE),
		  conditionalPanel(condition = "input.ss_type == 'mean'",
		    numericInput("ss_mean_err", "Acceptable Error (e.g., $10):", min = 0,
		  		value = state_init("ss_mean_err", 2), step = .1),
		    numericInput("ss_mean_s", "Sample std. deviation:", min = 0,
		  		value = state_init("ss_mean_s", 10), step = .1)
	  	),
		  conditionalPanel(condition = "input.ss_type != 'mean'",
		  	numericInput("ss_prop_err", "Acceptable Error (e.g., .03):", min = 0,
          max = 1, value = state_init("ss_prop_err", .1), step = .01),
		    numericInput("ss_prop_p", "Sample proportion:", min = 0, max = 1,
		  		value = state_init("ss_prop_p", .5), step = .05)
	  	),
	    numericInput("ss_z", "Confidence level (z-value):", min = 0,
	  		value = state_init("ss_z", 1.96), step = .1),
	    numericInput("ss_incidence", "Incidence rate:", min = 0, max = 1,
	  		value = state_init("ss_incidence", 1), step = .05),
	    numericInput("ss_response", "Response rate:", min = 0, max = 1,
	  		value = state_init("ss_response", 1), step = .05),
      radioButtons(inputId = "ss_pop_correction",
        choices = ss_pop_correction,
        label = "Correct for population size:",
        selected = state_init("ss_pop_correction", "no"),
        inline = TRUE),
		  conditionalPanel(condition = "input.ss_pop_correction == 'yes'",
		    numericInput("ss_pop_size", "Population size:", min = 1,
		  		value = state_init("ss_pop_size", 10^6), step = 1000))
		),
    help_and_report(modal_title = 'Sample size', fun_name = 'sample_size',
                    help_file = inclRmd("../quant/tools/help/sample_size.Rmd"))
 	)
})

output$sample_size <- renderUI({

    register_print_output("summary_sample_size", ".summary_sample_size")

    # one output with components stacked
    ss_output_panels <- tagList(
       tabPanel("Summary", verbatimTextOutput("summary_sample_size"))
    )

    stat_tab_panel(menu = "Sample",
                  tool = "Sample size",
                  tool_ui = "ui_sample_size",
                  output_panels = ss_output_panels)
})

.sample_size <- reactive({
  do.call(sample_size, ss_inputs())
})

.summary_sample_size <- reactive({
	if(is.null(input$ss_type)) return(invisible())
  summary(.sample_size())
})

observe({
  if(not_pressed(input$sample_size_report)) return()
  isolate({
    update_report(inp_main = clean_args(ss_inputs(), ss_args),
                   fun_name = "sample_size", outputs = "summary", figs = FALSE)
  })
})
