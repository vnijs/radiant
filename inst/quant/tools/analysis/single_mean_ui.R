###############################
# Single mean - ui
###############################

# alternative hypothesis options
sm_alt <- c("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")
sm_plots <- c("Histogram" = "hist", "Simulate" = "simulate")

# list of function arguments
sm_args <- as.list(formals(single_mean))

# list of function inputs selected by user
sm_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(sm_args))
    sm_args[[i]] <- input[[i]]
  if(!input$show_filter) sm_args$data_filter = ""
  sm_args
})

output$ui_sm_var <- renderUI({
  isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  selectInput(inputId = "sm_var", label = "Variable (select one):",
    choices = vars, selected = state_single("sm_var",vars), multiple = FALSE)
})

output$ui_single_mean <- renderUI({
  tagList(
    conditionalPanel(condition = "input.tabs_single_mean == 'Plot'",
      wellPanel(
        selectizeInput(inputId = "sm_plots", label = "Select plots:",
          choices = sm_plots,
          selected = state_single("sm_plots", sm_plots, "hist"),
          multiple = TRUE,
          options = list(plugins = list('remove_button', 'drag_drop')))
      )
    ),
    wellPanel(
 	   	uiOutput("ui_sm_var"),
  	  selectInput(inputId = "sm_alternative", label = "Alternative hypothesis:",
  	  	choices = sm_alt,
        selected = state_single("sm_alternative", sm_alt, sm_args$sm_alternative),
  	  	multiple = FALSE),
    	sliderInput('sm_sig_level',"Significance level:", min = 0.85, max = 0.99,
    		value = state_init('sm_sig_level',sm_args$sm_sig_level), step = 0.01),
    	numericInput("sm_comp_value", "Comparison value:",
    	  state_init('sm_comp_value',sm_args$sm_comp_value))
  	),
  	help_and_report(modal_title = 'Single mean', fun_name = 'single_mean',
  	                help_file = inclMD("../quant/tools/help/single_mean.md")
    )
 	)
})

sm_plot <- reactive({
  list(plot_width = 650, plot_height = 400 * length(input$sm_plots))
})

sm_plot_width <- function()
  sm_plot() %>% { if (is.list(.)) .$plot_width else 650 }

sm_plot_height <- function()
  sm_plot() %>% { if (is.list(.)) .$plot_height else 400 }

# output is called from the main radiant ui.R
output$single_mean <- renderUI({

		register_print_output("summary_single_mean", ".summary_single_mean")
		register_plot_output("plot_single_mean", ".plot_single_mean",
                         height_fun = "sm_plot_height")

		# two separate tabs
		sm_output_panels <- tabsetPanel(
	    id = "tabs_single_mean",
	    tabPanel("Summary", verbatimTextOutput("summary_single_mean")),
	    tabPanel("Plot", plotOutput("plot_single_mean", height = "100%"))
	  )

		# one output with components stacked
		# sm_output_panels <- tagList(
	  #    tabPanel("Summary", verbatimTextOutput("summary_single_mean")),
	  #    tabPanel("Plot", plotOutput("plot_single_mean", height = "100%"))
	  # )

		stat_tab_panel(menu = "Base",
		              tool = "Single mean",
		              tool_ui = "ui_single_mean",
		             	output_panels = sm_output_panels)

		# add "data = NULL" if the app doesn't doesn't use data
})

.single_mean <- reactive({
	do.call(single_mean, sm_inputs())
})

.summary_single_mean <- reactive({

  if(not_available(input$sm_var))
    return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")

  if(is.na(input$sm_comp_value))
    return("Please choose a comparison value")

  summary(.single_mean())
})

.plot_single_mean <- reactive({

  if(not_available(input$sm_var))
    return("This analysis requires a variable of type numeric or interval.\nIf none are available please select another dataset")

  if(is.na(input$sm_comp_value))
    return("Please choose a comparison value")

  plot(.single_mean(), sm_plots = input$sm_plots)
})

observe({
  if(not_pressed(input$single_mean_report)) return()
  isolate({
    outputs <- c("summary","plot")
    inp_out <- list(sm_plots = input$sm_plots) %>% list("",.)
    figs <- TRUE
    if(length(input$sm_plots) == 0) {
      figs <- FALSE
      outputs <- c("summary")
      inp_out <- list("","")
    }
    update_report(inp_main = clean_args(sm_inputs(), sm_args),
                  fun_name = "single_mean",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * sm_plot_width()/650,2),
                  fig.height = round(7 * sm_plot_height()/650,2))
  })
})
