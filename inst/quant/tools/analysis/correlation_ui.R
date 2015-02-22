###############################
# Correlation
###############################
cor_type <- c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")

# list of function arguments
cor_args <- as.list(formals(correlation))

# list of functions inputs selecored by user
cor_inputs <- reactive({
  # loop needed because reacorive values don't allow single bracket indexing
  for(i in names(cor_args))
    cor_args[[i]] <- input[[i]]
  if(!input$show_filter) cor_args$data_filter = ""
  cor_args
})

output$ui_cor_var <- renderUI({
	isChar <- "character" == getdata_class()
	vars <- varnames()[!isChar]
  if(length(vars) == 0) return()
  selectInput(inputId = "cor_var", label = "Select variables:", choices = vars,
 		selected = state_multiple("cor_var",vars),
 		multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_correlation <- renderUI({
  list(
  	wellPanel(
	    uiOutput("ui_cor_var"),
		  selectInput(inputId = "cor_type", label = "Method:", choices = cor_type,
  	  	selected = state_single("cor_type", cor_type, "pearson"), multiple = FALSE),
     	numericInput("cor_cutoff", label = "Correlation cutoff:", min = 0, max = 1,
    		value = state_init('cor_cutoff',0), step = 0.05)
	  ),
  	help_and_report(modal_title = "Correlation",
  	                fun_name = "correlation",
  	                help_file = inclMD("../quant/tools/help/correlation.md"))
	)
})

cor_plot_width <- function()
	.correlation() %>% { if(is.list(.)) .$plot_width else 650 }

cor_plot_height <- function()
	.correlation() %>% { if(is.list(.)) .$plot_height else 650 }

# output is called from the main radiant ui.R
output$correlation <- renderUI({

		register_print_output("summary_correlation", ".correlation")
		register_plot_output("plot_correlation", ".correlation",
                         height_fun = "cor_plot_height",
                         width_fun = "cor_plot_width")

		# two separate tabs
		cor_output_panels <- tabsetPanel(
	    id = "tabs_correlation",
	    tabPanel("Summary", verbatimTextOutput("summary_correlation")),
	    tabPanel("Plot", plotOutput("plot_correlation", width = "100%", height = "100%"))
	  )

		statTabPanel2(menu = "Regression",
		              tool = "Correlation",
		              tool_ui = "ui_correlation",
		             	output_panels = cor_output_panels)

})

.correlation <- reactive({
	"Please select two or more variables.\n\n" %>%
		suggest_data("diamonds") -> rt

	if(input$cor_var %>% not_available) return(rt)
	if(length(input$cor_var) < 2) return(rt)

	do.call(correlation, cor_inputs())
})

observe({
	if(input$correlation_report %>% not_pressed) return()
  isolate({
		outputs <- c("summary","plot")
		update_report(inp = clean_args(cor_inputs(), cor_args), fun_name = "correlation",
		              outputs = outputs,
		              fig.width = round(7 * cor_plot_width()/650,2),
		              fig.height = round(7 * cor_plot_height()/650,2))
  })
})
