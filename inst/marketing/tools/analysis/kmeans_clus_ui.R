###############################################################
# Kmeans clustering
###############################################################

km_plots <- c("Density" = "density", "Bar" = "bar", "Scatter" = "scatter")

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
  req(input$dataset)
  tagList(
    wellPanel(
      actionButton("km_run", "Estimate", width = "100%")
    ),
  	wellPanel(
	    uiOutput("ui_km_vars"),
		  checkboxInput(inputId = "km_hc_init", label = "Initial centers from HC",
      	value = state_init('km_hc_init',FALSE)),
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
      conditionalPanel(condition = "input.tabs_kmeans_clus == 'Plot'",
        selectInput("km_plots", label = "Plot(s):", choices = km_plots,
                 selected = state_multiple("km_plots", km_plots, "density"),
                 multiple = FALSE)
      ),
      conditionalPanel(condition = "input.km_vars != null",
        tags$table(
          tags$td(textInput("km_store_name", "Store membership:", state_init("km_store_name","kclus"))),
          tags$td(actionButton("km_store", "Store"), style="padding-top:30px;")
        )
		  )
  	),
  	help_and_report(modal_title = "K-means cluster analysis",
  	                fun_name = "kmeans_clus",
  	                help_file = inclMD(file.path(r_path,"marketing/tools/help/kmeans_clus.md")))
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
	    tabPanel("Summary",
        downloadLink("dl_km_means", "", class = "fa fa-download alignright"), br(),
        verbatimTextOutput("summary_kmeans_clus")),
	    tabPanel("Plot",
        plot_downloader("kmeans_clus", height = km_plot_height()),
        plotOutput("plot_kmeans_clus", height = "100%"))
	  )

		stat_tab_panel(menu = "Cluster",
		              tool = "K-means",
		              tool_ui = "ui_kmeans_clus",
		             	output_panels = km_output_panels)
})

.km_available <- reactive({
  if (not_available(input$km_vars))
    return("This analysis requires one or more variables of type numeric or integer.\nIf these variable types are not available please select another dataset.\n\n" %>% suggest_data("toothpaste"))
  if (not_pressed(input$km_run)) return("** Press the Estimate button to generate the cluster solution **")

  "available"
})

.kmeans_clus <- eventReactive(input$km_run, {
  withProgress(message = 'Estimating cluster solution', value = 0,
    # do.call(ann, ann_inputs())
    do.call(kmeans_clus, km_inputs())
  )
})

.summary_kmeans_clus <- reactive({
  if (.km_available() != "available") return(.km_available())
  summary(.kmeans_clus())
})

.plot_kmeans_clus <- reactive({
  if (.km_available() != "available") return(.km_available())
  plot(.kmeans_clus(), plots = input$km_plots, shiny = TRUE)
})

observeEvent(input$kmeans_clus_report, {
  update_report(inp_main = clean_args(km_inputs(), km_args),
                fun_name = "kmeans_clus",
                fig.width = round(7 * km_plot_width()/650,2),
                fig.height = round(7 * km_plot_height()/650,2),
								xcmd = paste0("# store(result, name = '", input$km_store_name,"')\n# write.csv(result$clus_means, file = '~/kmeans.csv')"))
})

output$dl_km_means <- downloadHandler(
  filename = function() { "kmeans.csv" },
  content = function(file) {
    if (pressed(input$km_run)) {
      .kmeans_clus() %>% { if (is.list(.)) write.csv(.$clus_means, file = file) }
    } else {
      cat("No output available. Press the Estimate button to generate the cluster solution", file = file)
    }
  }
)

## store cluster membership
observeEvent(input$km_store, {
  if (pressed(input$km_run)) {
    .kmeans_clus() %>% { if (is.list(.)) store(., name = input$km_store_name) }
  }
})
