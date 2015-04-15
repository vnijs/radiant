###############################
# Multidimensional scaling
###############################

mds_dim_number <- c("2-dims" = 2, "3-dims" = 3)
mds_method <- c("metric" = "metric", "non-metric" = "non-metric")

# list of function arguments
mds_args <- as.list(formals(mds))

# list of function inputs selected by user
mds_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(mds_args))
    mds_args[[i]] <- input[[i]]
  if (!input$show_filter) mds_args$data_filter = ""
  mds_args
})

mds_plot_args <- as.list(if (exists("plot.mds")) formals(plot.mds)
                         else formals(radiant:::plot.mds))

# list of function inputs selected by user
mds_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(mds_plot_args))
    mds_plot_args[[i]] <- input[[i]]
  if (!input$show_filter) mds_plot_args$data_filter = ""
  mds_plot_args
})

output$ui_mds_id1 <- renderUI({
	isLabel <- "character" == .getclass() | "factor" == .getclass()
  vars <- varnames()[isLabel]
  selectInput(inputId = "mds_id1", label = "ID 1:", choices = vars,
   	selected = state_single("mds_id1",vars), multiple = FALSE)
})

output$ui_mds_id2 <- renderUI({
  if (not_available(input$mds_id1)) return()
	isLabel <- "character" == .getclass() | "factor" == .getclass()
  vars <- varnames()[isLabel]
  if (length(vars) > 0) vars <- vars[-which(vars == input$mds_id1)]
  selectInput(inputId = "mds_id2", label = "ID 2:", choices = vars,
   	selected = state_single("mds_id2",vars), multiple = FALSE)
})

output$ui_mds_dis <- renderUI({
  if (not_available(input$mds_id2)) return()
 	isNum <- "numeric" == .getclass() | "integer" == .getclass()
 	vars <- varnames()[isNum]
  selectInput(inputId = "mds_dis", label = "Dissimilarity:", choices = vars,
   	selected = state_single("mds_dis",vars), multiple = FALSE)
})

output$ui_mds_rev_dim <- renderUI({
	rev_list <- list()
	rev_list[paste("Dim",1:input$mds_dim_number)] <- 1:input$mds_dim_number
	checkboxGroupInput("mds_rev_dim", "Reverse:", rev_list,
   	selected = state_init("mds_rev_dim", ""),
   	inline = TRUE)
})

output$ui_mds <- renderUI({
  list(
  	wellPanel(
	  	uiOutput("ui_mds_id1"),
	  	uiOutput("ui_mds_id2"),
	  	uiOutput("ui_mds_dis"),
		  radioButtons(inputId = "mds_method", label = NULL, mds_method,
		   	selected = state_init("mds_method", "metric"),
		   	inline = TRUE),
		  radioButtons(inputId = "mds_dim_number", label = NULL, mds_dim_number,
		   	selected = state_init("mds_dim_number", 2),
		   	inline = TRUE),
	 	 	conditionalPanel(condition = "input.tabs_mds == 'Plot'",
	 	 		numericInput("mds_fontsz", "Font size:", state_init("mds_fontsz",1.3), .5, 4, .1),
		  	uiOutput("ui_mds_rev_dim")
	    )
	  ),
		help_and_report(modal_title = "(Dis)similarity based brand maps (MDS)",
		                fun_name = "mds",
		                help_file = inclMD("tools/help/mds.md"))
	)
})

mds_plot <- reactive({
	nrDim <- as.numeric(input$mds_dim_number)
	nrPlots <- (nrDim * (nrDim - 1)) / 2
  list(plot_width = 650, plot_height = 650 * nrPlots)
})

mds_plot_width <- function()
  mds_plot() %>% { if (is.list(.)) .$plot_width else 650 }

mds_plot_height <- function()
  mds_plot() %>% { if (is.list(.)) .$plot_height else 650 }

output$mds <- renderUI({
		register_print_output("summary_mds", ".summary_mds")
		register_plot_output("plot_mds", ".plot_mds",
                         	width_fun = "mds_plot_width",
                         	height_fun = "mds_plot_height")

	  mds_output_panels <- tabsetPanel(
	    id = "tabs_mds",
	    tabPanel("Summary", verbatimTextOutput("summary_mds")),
	    tabPanel("Plot", plotOutput("plot_mds", height = "100%"))
	  )

		stat_tab_panel(menu = "Maps",
		              tool = "(Dis)similarity",
		              tool_ui = "ui_mds",
		             	output_panels = mds_output_panels)
})

.mds <- reactive({
	do.call(mds, mds_inputs())
})

.summary_mds <- reactive({
	if (not_available(input$mds_id2) || not_available(input$mds_dis))
		return("This analysis requires two id-variables of type character or factor and a measure\nof dissimilarity of type numeric or interval. Please select another dataset")
  .mds() %>% { if (is.character(.)) . else summary(., mds_round = 1) }
})

.plot_mds <- reactive({
	if (not_available(input$mds_id2) || not_available(input$mds_dis))
		return("This analysis requires two id-variables of type character or factor and a measure\nof dissimilarity of type numeric or interval. Please select another dataset")

  .mds() %>%
  	{ if (is.character(.)) {
  			.
  		} else {
  			# plot(., mds_rev_dim = input$mds_rev_dim, mds_fontsz = input$mds_fontsz)
        capture_plot( do.call(plot, c(list(x = .), mds_plot_inputs())) )
  		}
    }

})

observe({
 if (not_pressed(input$mds_report)) return()
  isolate({
    outputs <- c("summary","plot")
    inp_out <- list()
  	inp_out[[1]] <- ""
    # inp_out[[2]] <- list(mds_rev_dim = input$mds_rev_dim, mds_fontsz = input$mds_fontsz)
    inp_out[[2]] <- clean_args(mds_plot_inputs(), mds_plot_args[-1])
    update_report(inp_main = clean_args(mds_inputs(), mds_args),
                   fun_name = "mds",
                   inp_out = inp_out,
                   fig.width = round(7 * mds_plot_width()/650,2),
                   fig.height = round(7 * mds_plot_height()/650,2))
  })
})
