###############################
# Pre-factor analysis
###############################

## list of function arguments
pf_args <- as.list(formals(pre_factor))

## list of function inputs selected by user
pf_inputs <- reactive({
  pf_args$data_filter <- if (input$show_filter) input$data_filter else ""
  pf_args$dataset <- input$dataset
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in r_drop(names(pf_args)))
    pf_args[[i]] <- input[[paste0("pf_",i)]]
  pf_args
})

output$ui_pf_vars <- renderUI({
	isNum <- "numeric" == .getclass() | "integer" == .getclass()
 	vars <- varnames()[isNum]
  selectInput(inputId = "pf_vars", label = "Variables:", choices = vars,
  	selected = state_multiple("pf_vars",vars),
  	multiple = TRUE, size = min(15, length(vars)), selectize = FALSE)
})

output$ui_pre_factor <- renderUI({
  list(
  	wellPanel(
	  	uiOutput("ui_pf_vars")
	  ),
    help_and_report(modal_title = "Pre-factor analysis",
                    fun_name = "pre_factor",
                    help_file = inclMD("tools/help/pre_factor.md"))
 	)
})

pf_plot <- reactive({
  list(plot_width = 600, plot_height = 500)
})

pf_plot_width <- function()
  pf_plot() %>% { if (is.list(.)) .$plot_width else 600 }

pf_plot_height <- function()
  pf_plot() %>% { if (is.list(.)) .$plot_height else 500 }

output$pre_factor <- renderUI({

	register_print_output("summary_pre_factor", ".summary_pre_factor")
	register_plot_output("plot_pre_factor", ".plot_pre_factor",
                       	width_fun = "pf_plot_width",
                       	height_fun = "pf_plot_height")

	# one output with components stacked
	pf_output_panels <- tagList(
     tabPanel("Summary", verbatimTextOutput("summary_pre_factor")),
     tabPanel("Plot", plotOutput("plot_pre_factor", height = "100%"))
  )

	stat_tab_panel(menu = "Factor",
	              tool = "Pre-factor",
	              tool_ui = "ui_pre_factor",
	             	output_panels = pf_output_panels)
})

.pre_factor <- reactive({
  do.call(pre_factor, pf_inputs())
})

.summary_pre_factor <- reactive({
	if (not_available(input$pf_vars))
		return("This analysis requires multiple variables of type numeric or integer.\nIf these variables are not available please select another dataset.")
	if (length(input$pf_vars) < 2) return("Please select two or more numeric variables")

  summary(.pre_factor())
})

.plot_pre_factor <- reactive({

  if (not_available(input$pf_vars) || length(input$pf_vars) < 2)
    return(invisible())

  plot(.pre_factor())
})

observe({
  if (not_pressed(input$pre_factor_report)) return()
  isolate({
    update_report(inp_main = clean_args(pf_inputs(), pf_args),
                  fun_name = "pre_factor")
  })
})
