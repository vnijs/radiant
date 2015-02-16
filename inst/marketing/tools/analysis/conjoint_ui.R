################################################################
# Conjoint regression - UI
################################################################

# list of function arguments
ca_args <- as.list(formals(conjoint))

# list of function inputs selected by user
ca_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(ca_args))
    ca_args[[i]] <- input[[i]]
  if(!input$show_filter) ca_args$data_filter = ""
  ca_args
})

output$ui_ca_var1 <- renderUI({
	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  selectInput(inputId = "ca_var1", label = "Profile evaluations:", choices = vars,
   	selected = state_single("ca_var1",vars), multiple = FALSE)
})

output$ui_ca_var2 <- renderUI({
	isFct <- "factor" == getdata_class()
 	vars <- varnames()[isFct]
  selectInput(inputId = "ca_var2", label = "Attributes:", choices = vars,
  	selected = state_multiple("ca_var2", vars), multiple = TRUE, selectize = FALSE)
})

ca_plots <- list("Part-worths" = "pw", "Importance-weights" = "iw")

output$ui_conjoint <- renderUI({
  list(
  	wellPanel(
	    uiOutput("ui_ca_var1"),
	    uiOutput("ui_ca_var2"),
		  checkboxInput("ca_rev", label = "Reverse evaluation scores",
		  	value = state_init('ca_rev',FALSE)),
	    conditionalPanel(condition = "input.tabs_conjoint == 'Summary'",
		    checkboxInput(inputId = "ca_vif", label = "Calculate VIF-values",
			  	value = state_init('ca_vif',FALSE)),
		  	downloadButton('downloadPWs', 'Save PWs')
	  	),
	    conditionalPanel(condition = "input.tabs_conjoint == 'Plot'",
		    checkboxInput(inputId = "ca_scale_plot", label = "Scale PW plots",
			  	value = state_init('ca_scale_plot',FALSE)),
	      selectInput("ca_plots", "Conjoint plots:", choices = ca_plots,
	  	  	selected = state_single("ca_plots", ca_plots, "pw")))
	  ),
  	help_and_report(modal_title = "Conjoint",
  	                fun_name = "conjoint",
  	                help_file = inclMD("tools/help/conjoint.md"))
	)
})

ca_plot_width <- function()
	.conjoint() %>% { if(class(.)[1] == "character") 650 else .$plot_width }

ca_plot_height <- function()
	.conjoint() %>%{ if(class(.)[1] == "character") 650 else .$plot_height }

# output is called from the main radiant ui.R
output$conjoint <- renderUI({

		register_print_output("summary_conjoint", ".conjoint")
		register_plot_output("plot_conjoint", ".conjoint",
                         height_fun = "ca_plot_height",
                         width_fun = "ca_plot_width")

		# two separate tabs
		ca_output_panels <- tabsetPanel(
	    id = "tabs_conjoint",
	    tabPanel("Summary", verbatimTextOutput("summary_conjoint")),
	    # tabPanel("Plot", plotOutput("plot_conjoint", width = "100%", height = "100%"))
	    tabPanel("Plot", plotOutput("plot_conjoint"))
	  )

		statTabPanel2(menu = "Conjoint",
		              tool = "Conjoint",
		              tool_ui = "ui_conjoint",
		             	output_panels = ca_output_panels)
})

.conjoint <- reactive({
	if(input$ca_var1 %>% not_available)
		return("This analysis requires a dependent variable of type integer or \nnumeric and one or more independent variables of type factor.\nIf these variables are not available please select another dataset")

	if(input$ca_var2 %>% not_available)
		return("Please select one or more independent variables of type factor.\nIf none are available please choose another dataset ")

	# conjoint(input$dataset, input$ca_var1, input$ca_var2, input$ca_rev, input$ca_vif, input$ca_plots, input$ca_scale_plot)
	do.call(conjoint, ca_inputs())

})

observe({
  if(input$conjoint_report %>% not_pressed) return()
  isolate({
		update_report(inp = clean_args(ca_inputs(), ca_args), fun_name = "conjoint",
		              outputs = c("summary","plot"),
		              fig.width = round(7 * ca_plot_width()/650,2),
		              fig.height = round(7 * ca_plot_height()/500,2))
  })
})

output$downloadPWs <- downloadHandler(
	filename = function() { paste(input$dataset, '_PWs.csv', sep='') },
  content = function(file) {
  	.conjoint()$the_table$PW %>%
	  write.csv(file = file, row.names = FALSE)
  }
)
