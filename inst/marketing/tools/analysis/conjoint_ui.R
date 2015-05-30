################################################################
# Conjoint regression - UI
################################################################

ca_plots <- list("Part-worths" = "pw", "Importance-weights" = "iw")

# list of function arguments
ca_args <- as.list(formals(conjoint))

# list of function inputs selected by user
ca_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  ca_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ca_args$dataset <- input$dataset
  for (i in r_drop(names(ca_args)))
    ca_args[[i]] <- input[[paste0("ca_",i)]]
  ca_args
})

output$ui_ca_dep_var <- renderUI({
	isNum <- "numeric" == .getclass() | "integer" == .getclass()
 	vars <- varnames()[isNum]
  selectInput(inputId = "ca_dep_var", label = "Profile evaluations:", choices = vars,
   	selected = state_single("ca_dep_var",vars), multiple = FALSE)
})

output$ui_ca_indep_var <- renderUI({
	isFct <- "factor" == .getclass()
 	vars <- varnames()[isFct]
  selectInput(inputId = "ca_indep_var", label = "Attributes:", choices = vars,
  	selected = state_multiple("ca_indep_var", vars), multiple = TRUE,
  	size = min(10, length(vars)), selectize = FALSE)
})

output$ui_conjoint <- renderUI({
  tagList(
	  conditionalPanel(condition = "input.tabs_conjoint == 'Plot'",
  		wellPanel(
	      selectInput("ca_plots", "Conjoint plots:", choices = ca_plots,
	  	  	selected = state_single("ca_plots", ca_plots, "pw")),
	  		conditionalPanel(condition = "input.ca_plots == 'pw'",
			    checkboxInput(inputId = "ca_scale_plot", label = "Scale PW plots",
				  	value = state_init('ca_scale_plot',FALSE)))
  		)
	  ),
  	wellPanel(
	    uiOutput("ui_ca_dep_var"),
	    uiOutput("ui_ca_indep_var"),
      conditionalPanel(condition = "input.ca_indep_var != null",
			  checkboxInput("ca_reverse", label = "Reverse evaluation scores",
			  	value = state_init('ca_reverse',FALSE)),
		    conditionalPanel(condition = "input.tabs_conjoint == 'Summary'",
			    checkboxInput(inputId = "ca_mc_diag", label = "VIF",
				  	value = state_init('ca_mc_diag',FALSE)),
			  	downloadButton('downloadPWs', 'Save PWs')
		  	)
		  )
	  ),
  	help_and_report(modal_title = "Conjoint",
  	                fun_name = "conjoint",
  	                help_file = inclMD("tools/help/conjoint.md"))
	)
})


ca_plot <- reactive({
	nrVars <- length(input$ca_indep_var)
	plot_height <- plot_width <- 500
	if (input$ca_plots == 'pw') {
		plot_height <- 325 * (1 + floor((nrVars - 1) / 2))
		plot_width <- 325 * min(nrVars,2)
	}

  list(plot_width = plot_width, plot_height = plot_height)
})

ca_plot_width <- function()
  ca_plot() %>% { if (is.list(.)) .$plot_width else 650 }

ca_plot_height <- function()
  ca_plot() %>% { if (is.list(.)) .$plot_height else 400 }

# output is called from the main radiant ui.R
output$conjoint <- renderUI({

		register_print_output("summary_conjoint", ".summary_conjoint")
		register_print_output("predict_conjoint", ".predict_conjoint")
		register_plot_output("plot_conjoint", ".plot_conjoint",
                          height_fun = "ca_plot_height",
                          width_fun = "ca_plot_width")

		# two separate tabs
		ca_output_panels <- tabsetPanel(
	    id = "tabs_conjoint",
	    tabPanel("Summary", verbatimTextOutput("summary_conjoint")),
	    tabPanel("Predict", verbatimTextOutput("predict_conjoint")),
	    tabPanel("Plot", plotOutput("plot_conjoint", width = "100%", height = "100%"))
	    # tabPanel("Plot", plotOutput("plot_conjoint"))
	  )

		stat_tab_panel(menu = "Conjoint",
		              tool = "Conjoint",
		              tool_ui = "ui_conjoint",
		             	output_panels = ca_output_panels)
})

.conjoint <- reactive({
	do.call(conjoint, ca_inputs())
})

.summary_conjoint <- reactive({
	if (not_available(input$ca_dep_var))
		return("This analysis requires a dependent variable of type integer or \nnumeric and one or more independent variables of type factor.\nIf these variables are not available please select another dataset")

	if (not_available(input$ca_indep_var))
		return("Please select one or more independent variables of type factor.\nIf none are available please choose another dataset ")

  summary(.conjoint(), mc_diag = input$ca_mc_diag)
})

.predict_conjoint <- reactive({
	# nothing yet
	return(cat("Feature not yet implemented"))
})

.plot_conjoint <- reactive({
	if (not_available(input$ca_dep_var))
		return("This analysis requires a dependent variable of type integer or \nnumeric and one or more independent variables of type factor.\nIf these variables are not available please select another dataset")

	if (not_available(input$ca_indep_var))
		return("Please select one or more independent variables of type factor.\nIf none are available please choose another dataset ")

  plot(.conjoint(), plots = input$ca_plots,
       							scale_plot = input$ca_scale_plot,
       							shiny = TRUE)
})

observe({
  if (not_pressed(input$conjoint_report)) return()
  isolate({
    outputs <- c("summary","plot")
    inp_out <- list()
    inp_out[[1]] <- list(mc_diag = input$ca_mc_diag)
  	inp_out[[2]] <- list(plots = input$ca_plots, scale_plot = input$ca_scale_plot)
    figs <- TRUE
    if (length(input$ca_plots) == 0) {
      figs <- FALSE
      outputs <- c("summary")
    	inp_out[[2]] <- ""
    }
    update_report(inp_main = clean_args(ca_inputs(), ca_args),
                  fun_name = "conjoint", inp_out = inp_out,
                  outputs = outputs, figs = figs,
                  fig.width = round(7 * ca_plot_width()/650,2),
                  fig.height = round(7 * ca_plot_height()/650,2))
  })
})


output$downloadPWs <- downloadHandler(
	filename = function() { paste(input$dataset, '_PWs.csv', sep='') },
  content = function(file) {
  	.conjoint()$the_table$PW %>%
	  write.csv(file = file, row.names = FALSE)
  }
)
