###############################################################
# Kmeans clustering
###############################################################

# list of function arguments
km_args <- as.list(formals(kmeans_clus))

# list of function inputs selected by user
km_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(km_args))
    km_args[[i]] <- input[[i]]
  if(!input$show_filter) km_args$data_filter = ""
  km_args
})

output$ui_km_vars <- renderUI({

 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  selectInput(inputId = "km_vars", label = "Variables:", choices = vars,
	  selected = state_multiple("km_vars", vars, input$hc_vars),
	  multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
})

output$ui_kmeans_clus <- renderUI({
  tagList(
  	wellPanel(
	    uiOutput("ui_km_vars"),
		  checkboxInput(inputId = "km_hc_init", label = "Initial centers from HC",
      	value = state_init('km_hc_init',TRUE)),
	  	conditionalPanel(condition = "input.km_hc_init == true",
	  		wellPanel(
		  		selectInput("km_dist", label = "Distance measure:", choices = hc_dist,
			     	selected = state_single("km_dist", hc_dist, "sq.euclidian"), multiple = FALSE),
	  			selectInput("km_meth", label = "Method:", choices = hc_meth,
			     	selected = state_single("km_meth", hc_meth, "ward.D"), multiple = FALSE)
	  		)
	  	),
	  	conditionalPanel(condition = "input.km_hc_init == false",
		    numericInput("km_seed", "Set random seed:", min = 0,
		    	value = state_init('km_seed',1234))
		  ),
	    numericInput("km_nr_clus", "Number of clusters:", min = 2,
	    	value = state_init('km_nr_clus',2)),

			HTML("<label>Save:</label>"), br(),
      downloadButton("km_save_kmeans", "Means"),
	    actionButton("km_save_membership", "Membership")
  	),
  	help_and_report(modal_title = "K-means cluster analysis",
  	                fun_name = "kmeans_clus",
  	                help_file = inclMD("../marketing/tools/help/kmeans_clus.md"))
	)
})

km_plot <- reactive({
  list(plot_width = 750, plot_height = 300 * length(input$km_vars) / 2)
})

km_plot_width <- function()
  km_plot() %>% { if (is.list(.)) .$plot_width else 650 }

km_plot_height <- function()
  km_plot() %>% { if (is.list(.)) .$plot_height else 400 }

# output is called from the main radiant ui.R
output$kmeans_clus <- renderUI({

		register_print_output("summary_kmeans_clus", ".summary_kmeans_clus")
		register_plot_output("plot_kmeans_clus", ".plot_kmeans_clus",
                         	width_fun = "km_plot_width",
                         	height_fun = "km_plot_height")

	  km_output_panels <- tabsetPanel(
	    id = "tabs_kmeans_clus",
	    tabPanel("Summary", verbatimTextOutput("summary_kmeans_clus")),
	    tabPanel("Plot", plotOutput("plot_kmeans_clus", height = "100%"))
	  )

		stat_tab_panel(menu = "Cluster",
		              tool = "K-means",
		              tool_ui = "ui_kmeans_clus",
		             	output_panels = km_output_panels)
})

.kmeans_clus <- reactive({
	do.call(kmeans_clus, km_inputs())
})

.summary_kmeans_clus <- reactive({
  if(not_available(input$km_vars))
		return("Please select one or more variables of type numeric or integer.\nIf none are available please choose another dataset.")

  summary(.kmeans_clus())
})

.plot_kmeans_clus <- reactive({
  if(not_available(input$km_vars))
		return("Please select one or more variables of type numeric or integer.\nIf none are available please choose another dataset.")

  plot(.kmeans_clus())
})

observe({
  if(not_pressed(input$kmeans_clus_report)) return()
  isolate({
    update_report(inp_main = clean_args(km_inputs(), km_args),
                  fun_name = "kmeans_clus",
                  fig.width = round(7 * km_plot_width()/650,2),
                  fig.height = round(7 * km_plot_height()/650,2),
									xcmd = paste0("# save_membership(result)\n# save_kmeans(result, file = '~/kmeans.csv')"))
  })
})

# save cluster means when download button is pressed
output$km_save_kmeans <- downloadHandler(
  filename = function() { "save_kmeans.csv" },
  content = function(file) {
  	.kmeans_clus() %>% { if(is.list(.)) save_kmeans(., file) }
  }
)

# save cluster membership when action button is pressed
observe({
	if(not_pressed(input$km_save_membership)) return()
	isolate({
		.kmeans_clus() %>% { if(is.list(.)) save_membership(.) }
	})
})
