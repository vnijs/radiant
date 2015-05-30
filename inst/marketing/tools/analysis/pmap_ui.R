#########################################
# Perceptual map using factor analysis
#########################################

pm_dim_number <- c("2-dims" = 2, "3-dims" = 3)

# list of function arguments
pm_args <- as.list(formals(pmap))

# list of function inputs selected by user
pm_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(pm_args))
    pm_args[[i]] <- input[[i]]
  if (!input$show_filter) pm_args$data_filter = ""
  pm_args
})

pm_plot_args <- as.list(if (exists("plot.pmap")) formals(plot.pmap)
                          else formals(radiant:::plot.pmap))

# list of function inputs selected by user
pm_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(pm_plot_args))
    pm_plot_args[[i]] <- input[[i]]
  if (!input$show_filter) pm_plot_args$data_filter = ""
  pm_plot_args
})

output$ui_pm_brand <- renderUI({
	isLabel <- "character" == .getclass() | "factor" == .getclass()
  vars <- varnames()[isLabel]
 	selectInput(inputId = "pm_brand", label = "Brand:", choices = vars,
   	selected = state_single("pm_brand",vars), multiple = FALSE)
})

output$ui_pm_attr <- renderUI({
 	isNum <- "numeric" == .getclass() | "integer" == .getclass()
 	vars <- varnames()[isNum]
  selectInput(inputId = "pm_attr", label = "Attributes:", choices = vars,
   	selected = state_multiple("pm_attr",vars), multiple = TRUE,
  	size = min(10, length(vars)), selectize = FALSE)
})

output$ui_pm_pref <- renderUI({
  if (not_available(input$pm_attr)) return()
 	isNum <- "numeric" == .getclass() | "integer" == .getclass()
 	vars <- varnames()[isNum]
 	if (length(vars) > 0) vars <- vars[-which(vars %in% input$pm_attr)]
  selectInput(inputId = "pm_pref", label = "Preferences:", choices = vars,
   	selected = state_multiple("pm_pref",vars), multiple = TRUE,
  	size = max(1, min(5, length(vars))), selectize = FALSE)
})

output$ui_pm_plot <- renderUI({
	plot_list <- c("Brands" = "brand", "Attributes" = "attr")
  if (!is.null(input$pm_pref)) plot_list <- c(plot_list, c("Preferences" = "pref"))
	checkboxGroupInput("pm_plot", NULL, plot_list,
   	selected = state_init("pm_plot"),
   	inline = TRUE)
})

output$ui_pmap <- renderUI({
  list(
  	wellPanel(
	  	uiOutput("ui_pm_brand"),
	  	uiOutput("ui_pm_attr"),
	  	uiOutput("ui_pm_pref"),
		  radioButtons(inputId = "pm_dim_number", label = NULL, pm_dim_number,
		   	selected = state_init("pm_dim_number",2),
		   	inline = TRUE),
	 	 	conditionalPanel(condition = "input.tabs_pmap == 'Plot'",
		  	uiOutput("ui_pm_plot"),
	 	    div(class="row",
		    	div(class="col-xs-6", numericInput("pm_scaling", "Arrow scale:",
		    	    state_init("pm_scaling",2.1), .5, 4, .1)),
		      div(class="col-xs-6", numericInput("pm_fontsz", "Font size:",
		          state_init("pm_fontsz",1.3), .5, 4, .1))
		    )
	    ),
	 	 	conditionalPanel(condition = "input.tabs_pmap == 'Summary'",
	    	numericInput("pm_cutoff", label = "Loadings cutoff:", min = 0,
	    	             max = 1, state_init("pm_cutoff",0), step = .05),
      	conditionalPanel(condition = "input.pm_attr != null",
		    	actionButton("pm_save_scores", "Save scores")
		    )
		  )
 		),
		help_and_report(modal_title = "Attribute based brand maps",
		                fun_name = "pmap",
		                help_file = inclMD("tools/help/pmap.md"))
	)
})

pm_plot <- reactive({
	nrDim <- as.numeric(input$pm_dim_number)
	nrPlots <- (nrDim * (nrDim - 1)) / 2
  list(plot_width = 650, plot_height = 650 * nrPlots)
})

pm_plot_width <- function()
  pm_plot() %>% { if (is.list(.)) .$plot_width else 650 }

pm_plot_height <- function()
  pm_plot() %>% { if (is.list(.)) .$plot_height else 650 }

output$pmap <- renderUI({
		register_print_output("summary_pmap", ".summary_pmap")
		register_plot_output("plot_pmap", ".plot_pmap",
                         	width_fun = "pm_plot_width",
                         	height_fun = "pm_plot_height")

	  pm_output_panels <- tabsetPanel(
	    id = "tabs_pmap",
	    tabPanel("Summary", verbatimTextOutput("summary_pmap")),
	    tabPanel("Plot", plotOutput("plot_pmap", height = "100%"))
	  )

		stat_tab_panel(menu = "Maps",
		              tool = "Attributes",
		              tool_ui = "ui_pmap",
		             	output_panels = pm_output_panels)
})

.pmap <- reactive({
	do.call(pmap, pm_inputs())
})

.summary_pmap <- reactive({
	if (not_available(input$pm_brand) || not_available(input$pm_attr))
		return("This analysis requires a brand variable of type factor or character and multiple attribute variables\nof type numeric or integer. If these variables are not available please select another dataset.")

	brand <- .getdata()[,input$pm_brand]
	if (length(unique(brand)) < length(brand))
		return("Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset.")

	if (length(input$pm_attr) < 2) return("Please select two or more attribute variables")

  summary(.pmap(), pm_cutoff = input$pm_cutoff)
})

.plot_pmap <- reactive({

	if (not_available(input$pm_brand) || not_available(input$pm_attr))
		return("This analysis requires a brand variable of type factor or character and multiple attribute variables\nof type numeric or integer. If these variables are not available please select another dataset.")

	brand <- .getdata()[,input$pm_brand]
	if (length(unique(brand)) < length(brand))
		return("Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset.")

	if (length(input$pm_attr) < 2) return("Please select two or more attribute variables")

  capture_plot( do.call(plot, c(list(x = .pmap()), pm_plot_inputs())) )
})

observe({
 if (not_pressed(input$pm_report)) return()
  isolate({
    outputs <- c("summary","plot")
    inp_out <- list("","")
    inp_out[[2]] <- clean_args(pm_plot_inputs(), pm_plot_args[-1])
    update_report(inp_main = clean_args(pm_inputs(), pm_args),
                   fun_name = "pmap", inp_out = inp_out,
                   fig.width = round(7 * pm_plot_width()/650,2),
                   fig.height = round(7 * pm_plot_height()/650,2),
                   xcmd = paste0("# save_factors(result)"))
  })
})

observe({
	if (not_pressed(input$pm_save_scores)) return()
	isolate({
		.pmap() %>% { if (!is.character(.)) save_factors(.) }
	})
})
