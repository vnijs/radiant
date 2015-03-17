#########################################
# Perceptual map using factor analysis
#########################################

pmap_dim_number <- c("2-dims" = 2, "3-dims" = 3)

# list of function arguments
pmap_args <- as.list(formals(pmap))

# list of function inputs selected by user
pmap_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(pmap_args))
    pmap_args[[i]] <- input[[i]]
  if(!input$show_filter) pmap_args$data_filter = ""
  pmap_args
})

pmap_plot_args <- as.list(formals(radiant:::plot.pmap))

# list of function inputs selected by user
pmap_plot_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(pmap_plot_args))
    pmap_plot_args[[i]] <- input[[i]]
  if(!input$show_filter) pmap_plot_args$data_filter = ""
  pmap_plot_args
})

output$ui_pmap_brand <- renderUI({
	isLabel <- "character" == getdata_class() | "factor" == getdata_class()
  vars <- varnames()[isLabel]
 	selectInput(inputId = "pmap_brand", label = "Brand:", choices = vars,
   	selected = state_single("pmap_brand",vars), multiple = FALSE)
})

output$ui_pmap_attr <- renderUI({
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  selectInput(inputId = "pmap_attr", label = "Attributes:", choices = vars,
   	selected = state_multiple("pmap_attr",vars), multiple = TRUE,
  	size = min(10, length(vars)), selectize = FALSE)
})

output$ui_pmap_pref <- renderUI({
  if(not_available(input$pmap_attr)) return()
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
 	if(length(vars) > 0) vars <- vars[-which(vars %in% input$pmap_attr)]
  selectInput(inputId = "pmap_pref", label = "Preferences:", choices = vars,
   	selected = state_multiple("pmap_pref",vars), multiple = TRUE,
  	size = min(5, length(vars)), selectize = FALSE)
})

output$ui_pmap_plot <- renderUI({
	plot_list <- c("Brands" = "brand", "Attributes" = "attr")
  if(!is.null(input$pmap_pref)) plot_list <- c(plot_list, c("Preferences" = "pref"))
	checkboxGroupInput("pmap_plot", NULL, plot_list,
   	selected = state_init("pmap_plot"),
   	inline = TRUE)
})

output$ui_pmap <- renderUI({
  list(
  	wellPanel(
	  	uiOutput("ui_pmap_brand"),
	  	uiOutput("ui_pmap_attr"),
	  	uiOutput("ui_pmap_pref"),
		  radioButtons(inputId = "pmap_dim_number", label = NULL, pmap_dim_number,
		   	selected = state_init("pmap_dim_number",2),
		   	inline = TRUE),
	 	 	conditionalPanel(condition = "input.tabs_pmap == 'Plot'",
		  	uiOutput("ui_pmap_plot"),
	 	    div(class="row",
		    	div(class="col-xs-6", numericInput("pmap_scaling", "Arrow scale:",
		    	    state_init("pmap_scaling",2.1), .5, 4, .1)),
		      div(class="col-xs-6", numericInput("pmap_fontsz", "Font size:",
		          state_init("pmap_fontsz",1.3), .5, 4, .1))
		    )
	    ),
	 	 	conditionalPanel(condition = "input.tabs_pmap == 'Summary'",
	    	numericInput("pmap_cutoff", label = "Loadings cutoff:", min = 0,
	    	             max = 1, state_init("pmap_cutoff",0), step = .05),
      	conditionalPanel(condition = "input.pmap_attr != null",
		    	actionButton("pmap_save_scores", "Save scores")
		    )
		  )
 		),
		help_and_report(modal_title = "Attribute based brand maps",
		                fun_name = "pmap",
		                help_file = inclMD("tools/help/pmap.md"))
	)
})

pmap_plot <- reactive({
	nrDim <- as.numeric(input$pmap_dim_number)
	nrPlots <- (nrDim * (nrDim - 1)) / 2
  list(plot_width = 650, plot_height = 650 * nrPlots)
})

pmap_plot_width <- function()
  pmap_plot() %>% { if (is.list(.)) .$plot_width else 650 }

pmap_plot_height <- function()
  pmap_plot() %>% { if (is.list(.)) .$plot_height else 650 }

output$pmap <- renderUI({
		register_print_output("summary_pmap", ".summary_pmap")
		register_plot_output("plot_pmap", ".plot_pmap",
                         	width_fun = "pmap_plot_width",
                         	height_fun = "pmap_plot_height")

	  pmap_output_panels <- tabsetPanel(
	    id = "tabs_pmap",
	    tabPanel("Summary", verbatimTextOutput("summary_pmap")),
	    tabPanel("Plot", plotOutput("plot_pmap", height = "100%"))
	  )

		stat_tab_panel(menu = "Maps",
		              tool = "Attributes",
		              tool_ui = "ui_pmap",
		             	output_panels = pmap_output_panels)
})

.pmap <- reactive({
	do.call(pmap, pmap_inputs())
})

.summary_pmap <- reactive({
	if(not_available(input$pmap_brand) || not_available(input$pmap_attr))
		return("This analysis requires a brand variable of type factor or character and multiple attribute variables\nof type numeric or integer. If these variables are not available please select another dataset.")

	brand <- .getdata()[,input$pmap_brand]
	if(length(unique(brand)) < length(brand))
		return("Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset.")

	if(length(input$pmap_attr) < 2) return("Please select two or more attribute variables")

  summary(.pmap(), pmap_cutoff = input$pmap_cutoff)
})

.plot_pmap <- reactive({

	if(not_available(input$pmap_brand) || not_available(input$pmap_attr))
		return("This analysis requires a brand variable of type factor or character and multiple attribute variables\nof type numeric or integer. If these variables are not available please select another dataset.")

	brand <- .getdata()[,input$pmap_brand]
	if(length(unique(brand)) < length(brand))
		return("Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset.")

	if(length(input$pmap_attr) < 2) return("Please select two or more attribute variables")

  do.call(plot, c(list(x = .pmap()), pmap_plot_inputs()))
})

observe({
 if(not_pressed(input$pmap_report)) return()
  isolate({
    outputs <- c("summary","plot")
    inp_out <- list("","")
    inp_out[[2]] <- clean_args(pmap_plot_inputs(), pmap_plot_args[-1])
    update_report(inp_main = clean_args(pmap_inputs(), pmap_args),
                   fun_name = "pmap", inp_out = inp_out,
                   fig.width = round(7 * pmap_plot_width()/650,2),
                   fig.height = round(7 * pmap_plot_height()/650,2),
                   xcmd = paste0("# save_factors(result)"))
  })
})

observe({
	if(not_pressed(input$pmap_save_scores)) return()
	isolate({
		.pmap() %>% { if(!is.character(.)) save_factors(.) }
	})
})
