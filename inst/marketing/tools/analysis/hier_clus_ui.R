hc_method <- list("Ward's" = "ward.D", "Single" = "single", "Complete" = "complete", "Average" = "average",
	"McQuitty" =  "mcquitty", "Median" = "median", "Centroid" = "centroid")

hc_distance <- c("Squared euclidean" = "sq.euclidian", "Euclidian" = "euclidean",
	"Maximum" = "maximum", "Manhattan" = "manhattan", "Canberra" = "canberra", "Binary" = "binary", "Minkowski" = "minkowski")

hc_plots <- c("Scree" = "scree", "Difference" = "diff", "Dendrogram" = "dendro")

# list of function arguments
hc_args <- as.list(formals(hier_clus))

hc_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  hc_args$data_filter <- if (input$show_filter) input$data_filter else ""
  hc_args$dataset <- input$dataset
  for (i in r_drop(names(hc_args)))
    hc_args[[i]] <- input[[paste0("hc_",i)]]
  hc_args
})


###############################################################
# Hierarchical clustering
###############################################################
output$ui_hc_vars <- renderUI({

	isNum <- "numeric" == .getclass() | "integer" == .getclass()
	vars <- varnames()[isNum]
  selectInput(inputId = "hc_vars", label = "Variables:", choices = vars,
   	selected = state_multiple("hc_vars", vars),
	  multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
})


output$ui_hier_clus <- renderUI({
  tagList(
  	wellPanel(
	    uiOutput("ui_hc_vars"),
	    selectInput("hc_distance", label = "Distance measure:", choices =hc_distance ,
	     	selected = state_single("hc_distance",hc_distance , "sq.euclidean"),
	     	multiple = FALSE),
	    selectInput("hc_method", label = "Method:", choices = hc_method,
	     	selected = state_single("hc_method", hc_method, "ward.D"), multiple = FALSE),
 			selectizeInput("hc_plots", label = "Plot(s):", choices = hc_plots,
               selected = state_multiple("hc_plots", hc_plots, c("scree","diff")),
               multiple = TRUE,
               options = list(placeholder = 'Select plot(s)',
                              plugins = list('remove_button', 'drag_drop'))),
	    numericInput("hc_cutoff", "Plot cutoff:", min = 0, max = 1,
	    	value = state_init('hc_cutoff',0.05), step = .02)
  	),
  	help_and_report(modal_title = "Hierarchical cluster analysis",
  	                fun_name = "hier_clus",
  	                help_file = inclMD("tools/help/hier_clus.md"))
	)
})

hc_plot <- reactive({
	input$hc_plots %>% { if (length(.) == 1 && . == "dendro") 800 else 400 } -> bh
  list(plot_width = 650, plot_height = bh * length(input$hc_plots))
})

hc_plot_width <- function()
  hc_plot() %>% { if (is.list(.)) .$plot_width else 650 }

hc_plot_height <- function()
  hc_plot() %>% { if (is.list(.)) .$plot_height else 400 }

# output is called from the main radiant ui.R
output$hier_clus <- renderUI({

		register_print_output("summary_hier_clus", ".summary_hier_clus")
		register_plot_output("plot_hier_clus", ".plot_hier_clus",
                         	width_fun = "hc_plot_width",
                         	height_fun = "hc_plot_height")

		# one output with components stacked
		hc_output_panels <- tagList(
	     tabPanel("Summary", verbatimTextOutput("summary_hier_clus")),
	     tabPanel("Plot", plotOutput("plot_hier_clus", height = "100%"))
	  )

		stat_tab_panel(menu = "Cluster",
		              tool = "Hierarchical",
		              tool_ui = "ui_hier_clus",
		             	output_panels = hc_output_panels)

})

.hier_clus <- reactive({
	do.call(hier_clus, hc_inputs())
})

.summary_hier_clus <- reactive({
  if (not_available(input$hc_vars)) {
    cat("Please select one or more variables of type numeric or integer.\nIf none are available please choose another dataset.")
		return(invisible())
  }

  summary(.hier_clus())
})

.plot_hier_clus <- reactive({
  if (not_available(input$hc_vars))
		return(" ")

  .hier_clus() %>%
    { if ("dendro" %in% input$hc_plots && length(.$hc_out$height) > 100) {
        capture_plot( plot(., plots = input$hc_plots, cutoff = input$hc_cutoff) )
      } else {
        plot(., plots = input$hc_plots, cutoff = input$hc_cutoff, shiny = TRUE)
      }
    }
})

observe({
  if (not_pressed(input$hier_clus_report)) return()
  isolate({
    if (length(input$hc_plots) > 0) {
      inp_out <- list(plots = input$hc_plots) %>% list("",.)
      outputs <- c("summary","plot")
      figs <- TRUE
    } else {
      outputs <- c("summary")
      inp_out <- list("","")
      figs <- FALSE
    }
    update_report(inp_main = clean_args(hc_inputs(), hc_args),
                  fun_name = "hier_clus",
                  inp_out = inp_out,
                  outputs = outputs,
                  figs = figs,
                  fig.width = round(7 * hc_plot_width()/650,2),
                  fig.height = round(7 * hc_plot_height()/650,2))
  })
})
