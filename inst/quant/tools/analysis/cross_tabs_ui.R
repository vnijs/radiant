# alternative hypothesis options
base_alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

# list of function arguments
ct_args <- as.list(formals(cross_tabs))

# list of function inputs selected by user
ct_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(ct_args))
    ct_args[[i]] <- input[[i]]
  if(!input$show_filter) ct_args$data_filter = ""
  ct_args
})

###############################
# Cross-tabs
###############################
output$ui_ct_var1 <- renderUI({
	isFct <- "factor" == getdata_class()
  vars <- varnames()[isFct]
  selectInput(inputId = "ct_var1", label = "Select a grouping factor:", choices = vars,
  	selected = state_single("ct_var1",vars), multiple = FALSE)
})

output$ui_ct_var2 <- renderUI({
	isFct <- "factor" == getdata_class()
  vars <- varnames()[isFct]
  if(input$ct_var1 %>% not_available) vars <- character(0)
  if(length(vars) > 0) vars <- vars[-which(vars == input$ct_var1)]
  selectInput(inputId = "ct_var2", label = "Select a factor:", choices = vars,
  	selected = state_single("ct_var2",vars), multiple = FALSE)
})

output$ui_cross_tabs <- renderUI({
  list(
  	wellPanel(
	    uiOutput("ui_ct_var1"),
	    uiOutput("ui_ct_var2"),
		  checkboxInput("ct_observed", label = "Observed values",
	     	value = state_init("ct_observed",TRUE)),
		  checkboxInput("ct_expected", label = "Expected values",
	     	value = state_init("ct_expected",FALSE)),
	    conditionalPanel(condition = "input.tabs_cross_tabs == 'Summary'",
			  checkboxInput("ct_contrib", label = "Difference (o - e)^2 / e",
	     	value = state_init("ct_contrib",FALSE))),
		  checkboxInput("ct_std_residuals", label = "Deviation (standarized)",
	     	value = state_init("ct_std_residuals",FALSE)),
		  checkboxInput("ct_deviation", label = "Deviation (percentage)",
	     	value = state_init("ct_deviation",FALSE))
		),
  	help_and_report(modal_title = "Cross-tabs",
  	                fun_name = "cross_tabs",
  	                help_file = inclMD("../quant/tools/help/cross_tabs.md"))
  )
})

ct_plot_width <- function()
	.cross_tabs() %>% { if(is.list(.)) .$plot_width else 650 }

ct_plot_height <- function()
	.cross_tabs() %>% { if(is.list(.)) .$plot_height else 650 }

# output is called from the main radiant ui.R
output$cross_tabs <- renderUI({

		register_print_output("summary_cross_tabs", ".cross_tabs")
		register_plot_output("plot_cross_tabs", ".cross_tabs",
                         height_fun = "ct_plot_height",
                         width_fun = "ct_plot_width")

		# two separate tabs
		ct_output_panels <- tabsetPanel(
	    id = "tabs_cross_tabs",
	    tabPanel("Summary", verbatimTextOutput("summary_cross_tabs")),
	    tabPanel("Plot", plotOutput("plot_cross_tabs", width = "100%", height = "100%"))
	  )

		statTabPanel2(menu = "Base",
		              tool = "Cross-tabs",
		              tool_ui = "ui_cross_tabs",
		             	output_panels = ct_output_panels)

})

.cross_tabs <- reactive({
	if(input$ct_var2 %>% not_available)
		return("This analysis requires variables of type factor.\nIf none are available please select another dataset.")

	do.call(cross_tabs, ct_inputs())
})

observe({
  if(input$cross_tabs_report %>% not_pressed) return()
  isolate({
		outputs <- c("summary","plot")
		nrPlots <- 0
  	for(i in c("ct_observed", "ct_expected", "ct_std_residuals", "ct_deviation"))
  		nrPlots <- nrPlots + ct_inputs()[[i]]
  	if(nrPlots == 0) outputs = c("summary")
		update_report(inp = clean_args(ct_inputs(), ct_args), fun_name = "cross_tabs",
		             outputs = outputs,
		             fig.width = round(7 * ct_plot_width()/650,2),
		             fig.height = round(7 * ct_plot_height()/650,2))
  })
})
