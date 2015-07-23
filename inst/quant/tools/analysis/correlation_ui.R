###############################
# Correlation
###############################
cor_type <- c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")

# list of function arguments
cor_args <- as.list(formals(correlation))

# list of function inputs selected by user
cor_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  cor_args$data_filter <- if (input$show_filter) input$data_filter else ""
  cor_args$dataset <- input$dataset
  for (i in r_drop(names(cor_args)))
    cor_args[[i]] <- input[[paste0("cor_",i)]]
  cor_args
})

output$ui_cor_vars <- renderUI({
	isChar <- "character" == .getclass()
	vars <- varnames()[!isChar]
  if (length(vars) == 0) return()
  selectInput(inputId = "cor_vars", label = "Select variables:", choices = vars,
 		selected = state_multiple("cor_vars",vars),
 		multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_correlation <- renderUI({
  list(
  	wellPanel(
	    uiOutput("ui_cor_vars"),
		  selectInput(inputId = "cor_type", label = "Method:", choices = cor_type,
  	  	selected = state_single("cor_type", cor_type, "pearson"), multiple = FALSE),
		  conditionalPanel(condition = "input.tabs_correlation == 'Summary'",
     		numericInput("cor_cutoff", label = "Correlation cutoff:", min = 0, max = 1,
    			value = state_init("cor_cutoff",0), step = 0.05)
     	)
	  ),
  	help_and_report(modal_title = "Correlation",
  	                fun_name = "correlation",
  	                help_file = inclMD(file.path(r_path,"quant/tools/help/correlation.md")))
	)
})

cor_plot <- reactive({
	length(input$cor_vars) %>%
  	{ list(plot_width = 400 + 75 * ., plot_height = 400 + 75 * .) }
})

cor_plot_width <- function()
  cor_plot() %>% { if (is.list(.)) .$plot_width else 650 }

cor_plot_height <- function()
  cor_plot() %>% { if (is.list(.)) .$plot_height else 650 }

# output is called from the main radiant ui.R
output$correlation <- renderUI({

		register_print_output("summary_correlation", ".summary_correlation")
		register_plot_output("plot_correlation", ".plot_correlation",
                         	height_fun = "cor_plot_height",
                         	width_fun = "cor_plot_width")

		# two separate tabs
		cor_output_panels <- tabsetPanel(
	    id = "tabs_correlation",
	    tabPanel("Summary", verbatimTextOutput("summary_correlation")),
	    tabPanel("Plot",
               plot_downloader("correlation", height = cor_plot_height()),
	             plotOutput("plot_correlation", width = "100%", height = "100%"))
	  )

		stat_tab_panel(menu = "Regression",
		              tool = "Correlation",
		              tool_ui = "ui_correlation",
		             	output_panels = cor_output_panels)

})

.correlation <- reactive({
	do.call(correlation, cor_inputs())
})

.summary_correlation <- reactive({

	"Please select two or more variables.\n\n" %>%
		suggest_data("diamonds") -> rt

	if (input$cor_vars %>% not_available) return(rt)
	if (length(input$cor_vars) < 2) return(rt)

	summary(.correlation(), cutoff = input$cor_cutoff)
})

.plot_correlation <- reactive({

	"Please select two or more variables.\n\n" %>%
		suggest_data("diamonds") -> rt

	if (not_available(input$cor_vars)) return(rt)
	if (length(input$cor_vars) < 2) return(rt)

	capture_plot(plot(.correlation()))
})

observe({
  if (not_pressed(input$correlation_report)) return()
  isolate({
    inp_out <- list(cutoff = input$cor_cutoff) %>% list(.,"")
    update_report(inp_main = clean_args(cor_inputs(), cor_args),
                  fun_name = "correlation",
                  inp_out = inp_out,
                  fig.width = round(7 * cor_plot_width()/650,2),
                  fig.height = round(7 * cor_plot_height()/650,2))
  })
})
