###############################################################
# Kmeans clustering
###############################################################

# list of function arguments
km_args <- as.list(formals(kmeans_clus))

km_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  km_args$data_filter <- if (input$show_filter) input$data_filter else ""
  km_args$dataset <- input$dataset
  for (i in r_drop(names(km_args)))
    km_args[[i]] <- input[[paste0("km_",i)]]
  km_args
})

output$ui_km_vars <- renderUI({

 	isNum <- "numeric" == .getclass() | "integer" == .getclass()
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
		  		selectInput("km_distance", label = "Distance measure:", choices = hc_distance,
			     	selected = state_single("km_distance", hc_distance, "sq.euclidian"), multiple = FALSE),
	  			selectInput("km_method", label = "Method:", choices = hc_method,
			     	selected = state_single("km_method", hc_method, "ward.D"), multiple = FALSE)
	  		)
	  	),
	  	conditionalPanel(condition = "input.km_hc_init == false",
		    numericInput("km_seed", "Set random seed:", min = 0,
		    	value = state_init('km_seed',1234))
		  ),
	    numericInput("km_nr_clus", "Number of clusters:", min = 2,
	    	value = state_init('km_nr_clus',2)),
      conditionalPanel(condition = "input.km_vars != null",
				HTML("<label>Save:</label>"), br(),
	      downloadButton("km_save_kmeans", "Means"),
		    actionButton("km_save_membership", "Membership")
		  )
  	),
  	help_and_report(modal_title = "K-means cluster analysis",
  	                fun_name = "kmeans_clus",
  	                help_file = inclMD("tools/help/kmeans_clus.md"))
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
  if (not_available(input$km_vars))
		return("Please select one or more variables of type numeric or integer.\nIf none are available please choose another dataset.")

  summary(.kmeans_clus())
})

.plot_kmeans_clus <- reactive({
  if (not_available(input$km_vars))
		return("Please select one or more variables of type numeric or integer.\nIf none are available please choose another dataset.")

  plot(.kmeans_clus(), shiny = TRUE)
})

observe({
  if (not_pressed(input$kmeans_clus_report)) return()
  isolate({
    update_report(inp_main = clean_args(km_inputs(), km_args),
                  fun_name = "kmeans_clus",
                  fig.width = round(7 * km_plot_width()/650,2),
                  fig.height = round(7 * km_plot_height()/650,2),
									xcmd = paste0("# save_membership(result)\n# write.csv(result$clus_means, file = '~/kmeans.csv')"))
  })
})

# save cluster means when download button is pressed
output$km_save_kmeans <- downloadHandler(
  filename = function() { "kmeans.csv" },
  content = function(file) {
  	.kmeans_clus() %>% { if (is.list(.)) write.csv(.$clus_means, file) }
  }
)

# save cluster membership when action button is pressed
observe({
	if (not_pressed(input$km_save_membership)) return()
	isolate({
		.kmeans_clus() %>% { if (is.list(.)) save_membership(.) }
	})
})
