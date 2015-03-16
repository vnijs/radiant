###############################
# Sampling
###############################

# list of function arguments
smp_args <- as.list(formals(sampling))

# list of function inputs selected by user
smp_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(smp_args))
    smp_args[[i]] <- input[[i]]
  if(!input$show_filter) smp_args$data_filter = ""
  smp_args
})

output$ui_smp_var <- renderUI({
	isChar <- "character" == getdata_class()
  vars <- varnames()[isChar]
  selectInput(inputId = "smp_var", label = "ID variable:",
              choices = vars, selected = state_single("smp_var",vars),
              multiple = FALSE)
})

output$ui_sampling <- renderUI({
  list(
  	wellPanel(
	 	 	uiOutput("ui_smp_var"),
	  	numericInput("smp_sample_size", "Sample size:", min = 1,
                   value = state_init("smp_sample_size",1))),
    help_and_report(modal_title = 'Sampling', fun_name = 'sampling',
                    help_file = inclMD("../quant/tools/help/sampling.md"))
 	)
})

output$sampling <- renderUI({

    register_print_output("summary_sampling", ".summary_sampling")

    # one output with components stacked
    smp_output_panels <- tagList(
       tabPanel("Summary", verbatimTextOutput("summary_sampling"))
    )

    stat_tab_panel(menu = "Sample",
                  tool = "Sampling",
                  tool_ui = "ui_sampling",
                  output_panels = smp_output_panels)
})

.sampling <- reactive({
  do.call(sampling, smp_inputs())
})

.summary_sampling <- reactive({

  "This analysis requires a variable of type character.\nEntries should be unique (i.e., no duplicates).\nIf a variable of this type is not available please select another dataset.\n\n" %>%
  suggest_data("rndnames") -> rt

  if(not_available(input$smp_var)) return(rt)
  if(is.na(input$smp_sample_size)) return("Please select a sample size of 1 or greater.")
  if(has_duplicates(getdata(input$dataset, vars = input$smp_var))) return(rt)

  summary(.sampling())
})

observe({
  if(not_pressed(input$sampling_report)) return()
  isolate({
    update_report(inp_main = clean_args(smp_inputs(), smp_args),
                  fun_name = "sampling", outputs = "summary", figs = FALSE)
  })
})
