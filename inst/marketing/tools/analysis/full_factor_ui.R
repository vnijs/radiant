ff_method <- c('Principal components' = 'PCA', 'Maximum Likelihood' = "maxlik")
ff_rotation <- c('Varimax' = 'varimax', 'None' = 'none')

## list of function arguments
ff_args <- as.list(formals(full_factor))

## list of function inputs selected by user
ff_inputs <- reactive({
  ff_args$data_filter <- if (input$show_filter) input$data_filter else ""
  ff_args$dataset <- input$dataset
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in r_drop(names(ff_args)))
    ff_args[[i]] <- input[[paste0("ff_",i)]]
  ff_args
})

###############################
# Factor analysis
###############################
output$ui_ff_vars <- renderUI({

 	isNum <- "numeric" == .getclass() | "integer" == .getclass()
 	vars <- varnames()[isNum]
  selectInput(inputId = "ff_vars", label = "Variables:", choices = vars,
  	selected = state_multiple("ff_vars", vars, input$pf_vars),
  	multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
})

output$ui_full_factor <- renderUI({
	# for ui
	list(
    wellPanel(
      uiOutput("ui_ff_vars"),
      selectInput("ff_method", label = "Method:", choices = ff_method,
      	selected = state_single("ff_method", ff_method, "PCA")),
			div(class="row",
 	    	div(class="col-xs-6", numericInput("ff_nr_fact",
 	    	    label = "# of factors:", min = 1,
 	    	    value = state_init('ff_nr_fact',1))),
	    	div(class="col-xs-6", numericInput("ff_cutoff",
	    	    label = "Cutt-off", min = 0, max = 1,
	    	    value = state_init('ff_cutoff',0), step = .05))
  	  ),
  	  conditionalPanel(condition = "input.tabs_full_factor == 'Summary'",
        checkboxInput("ff_fsort", "Sort", value = state_init("ff_fsort",FALSE))
      ),
      radioButtons("ff_rotation", label = "Rotation:", ff_rotation,
      	selected = state_init("ff_rotation", "varimax"),
      	inline = TRUE),
      conditionalPanel(condition = "input.ff_vars != null",
        HTML("<label>Save:</label>"), br(),
        downloadButton("ff_save_loadings", "Loadings"),
        actionButton("ff_save_scores", "Scores")
      )
  	),
		help_and_report(modal_title = "Factor",
		                fun_name = "full_factor",
		                help_file = inclMD("tools/help/full_factor.md"))
	)
})

ff_plot <- reactive({
 	nrPlots <- (input$ff_nr_fact * (input$ff_nr_fact - 1)) / 2

	plot_height <- plot_width <- 350
	if (nrPlots > 2)
		plot_height <- 350 * ceiling(nrPlots/2)

	if (nrPlots > 1)
		plot_width <- 700

  list(plot_width = plot_width, plot_height = plot_height)
})

ff_plot_width <- function()
  ff_plot() %>% { if (is.list(.)) .$plot_width else 650 }

ff_plot_height <- function()
  ff_plot() %>% { if (is.list(.)) .$plot_height else 400 }

output$full_factor <- renderUI({

		register_print_output("summary_full_factor", ".summary_full_factor")
		register_plot_output("plot_full_factor", ".plot_full_factor",
                         	width_fun = "ff_plot_width",
                         	height_fun = "ff_plot_height")

	  ff_output_panels <- tabsetPanel(
	    id = "tabs_full_factor",
	    tabPanel("Summary", verbatimTextOutput("summary_full_factor")),
	    tabPanel("Plot", plotOutput("plot_full_factor", height = "100%"))
	  )

		stat_tab_panel(menu = "Factor",
		              tool = "Factor",
		              tool_ui = "ui_full_factor",
		             	output_panels = ff_output_panels)
})

.full_factor <- reactive({
	do.call(full_factor, ff_inputs())
})

.summary_full_factor <- reactive({
  if (not_available(input$ff_vars))
		return("This analysis requires multiple variables of type numeric or integer.\nIf these variables are not available please select another dataset.")

	if (length(input$ff_vars) < 2) return("Please select two or more variables")
	if (is.null(input$ff_nr_fact)) return("Number of factors should be > 1.")

  summary(.full_factor(), cutoff = input$ff_cutoff, fsort = input$ff_fsort)
})

.plot_full_factor <- reactive({
  if (not_available(input$ff_vars))
		return("This analysis requires multiple variables of type numeric or integer.\nIf these variables are not available please select another dataset.")

	if (length(input$ff_vars) < 2) return("Please select two or more variables")
	if (is.null(input$ff_nr_fact)) return("Number of factors should be > 1.")

  plot(.full_factor(), shiny = TRUE)
})

observe({
 if (not_pressed(input$full_factor_report)) return()
  isolate({
    outputs <- c("summary","plot")
    inp_out <- list()
    inp_out[[1]] <- list(cutoff = input$ff_cutoff, fsort = input$ff_fsort)
  	inp_out[[2]] <- ""
    xcmd = paste0("# save_factors(result)\n# clean_loadings(result$floadings,", input$ff_cutoff, ",", input$ff_fsort, ") %>% write.csv(file = '~/factor_loadings.csv')")

    update_report(inp_main = clean_args(ff_inputs(), ff_args),
                   fun_name = "full_factor",
                   inp_out = inp_out,
                   fig.width = round(7 * ff_plot_width()/650,2),
                   fig.height = round(7 * ff_plot_height()/650,2),
                   xcmd = xcmd)
  })
})

# save factor loadings when download button is pressed
output$ff_save_loadings <- downloadHandler(
  filename = function() { "loadings.csv" },
  content = function(file) {
    .full_factor() %>%
      { if (is.list(.)) .$floadings else return() } %>%
      clean_loadings(input$ff_cutoff, input$ff_fsort) %>%
      write.csv(file)
  }
)

# save factor scores when action button is pressed
observe({
	if (not_pressed(input$ff_save_scores)) return()
	isolate({
	 .full_factor() %>% { if (!is.character(.)) save_factors(.) }
	})
})
