#######################################
# Vizualize data
#######################################
output$uiVizvars1 <- renderUI({
	# vars <- c("None" = "", varnames())
	# selectInput(inputId = "vizvars1", label = "X-variable", choices = vars,
 #  	selected = state_multvar("vizvars1",vars), multiple = input$viz_multiple == 'multiple')
	vars <- varnames()
	if(input$viz_multiple == 'single') vars <- c("None" = "", vars)
	selectInput(inputId = "vizvars1", label = "X-variable", choices = vars,
  	# selected = state_multvar("vizvars1",vars), multiple = input$viz_multiple == 'multiple', selectize = FALSE)
  	selected = state_multvar("vizvars1",vars), multiple = input$viz_multiple == 'multiple',
  	selectize = input$viz_multiple == 'single')
})

output$uiVizvars2 <- renderUI({
	# if(is.null(input$vizvars1)) return() 	# can't have an XY plot without an X
	# if(is.null(inChecker(input$vizvars1))) return()
	vars <- varnames()
	selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "", vars),
  	selected = state_singlevar("vizvars2",vars), multiple = FALSE)
})

output$uiViz_color <- renderUI({
	if(is.null(input$vizvars2)) return() 	# can't have an XY plot without an X
	# if(length(input$vizvars2) > 1) return()
	vars <- c("None" = "", varnames())
	selectInput("viz_color", "Color", vars,
  	selected = state_init_list("viz_color","", vars), multiple = FALSE)
})

output$uiViz_facet_row <- renderUI({
	# if(length(input$vizvars2) > 1) return()
	isFct <- "factor" == getdata_class()
	vars <- c("None" = ".", varnames()[isFct])
	selectInput("viz_facet_row", "Facet row", vars,
  	selected = state_init_list("viz_facet_row", ".", vars), multiple = FALSE)
})

output$uiViz_facet_col <- renderUI({
	# if(length(input$vizvars2) > 1) return()
	isFct <- "factor" == getdata_class()
	vars <- c("None" = ".", varnames()[isFct])
	selectInput("viz_facet_col", 'Facet col', vars,
  	selected = state_init_list("viz_facet_col", ".", vars), multiple = FALSE)
})

viz_multiple <- c("Single" = "single", "Multiple" = "multiple")

output$ui_Visualize <- renderUI({
	list(wellPanel(
      radioButtons("viz_multiple", "Number of plots:", viz_multiple, state_init_list("viz_multiple","single", viz_multiple)),
			uiOutput("uiVizvars1"),
			uiOutput("uiVizvars2"),
			conditionalPanel(condition = "input.viz_multiple == 'single'",
			  uiOutput("uiViz_facet_row"),
			  uiOutput("uiViz_facet_col")
			),
			conditionalPanel(condition = "input.vizvars2 != ''",
			  uiOutput("uiViz_color"),
			  checkboxInput('viz_line', 'Line', value = state_init("viz_line", FALSE)),
			  checkboxInput('viz_loess', 'Loess', value = state_init("viz_loess", FALSE)),
			  checkboxInput('viz_jitter', 'Jitter', value = state_init("viz_jitter", FALSE))
			),
      returnTextInput("viz_select", "Subset (e.g., price > 5000)", state_init("viz_select")),
			div(class="row-fluid",
	    	div(class="span6",numericInput("viz_plot_height", label = "Plot height:", min = 100, step = 50,
	    		value = state_init("viz_plot_height", values$plotHeight))),
	      div(class="span6", numericInput("viz_plot_width", label = "Plot width:", min = 100, step = 50,
	    		value = state_init("viz_plot_width", values$plotWidth)))
	    )
			# ,div(class="row-fluid",
			# 	div(class="span6",
			# 		dateInput("date_start", "From:", value = Sys.Date()-14)),
			# 	div(class="span6",
			# 		dateInput("date_end", "To:", value = Sys.Date())),
			# 	tags$style(type="text/css", '#date_start {width: 80%}'),
			# 	tags$style(type="text/css", '#date_end {width: 80%}')
			# )
		),
	  helpAndReport('Visualize','visualize',inclMD("../base/tools/help/visualize.md"))
  )
})

viz_plot_width <- reactive({
	ifelse(is.null(input$viz_plot_width), return(values$plotWidth), return(input$viz_plot_width))
})

viz_plot_height <- reactive({
	# if(is.null(input$viz_plot_height)) return(values$plotHeight)
	if(is.null(input$viz_plot_height)) return(values$plotHeight)
	if(input$viz_multiple == "multiple") {
		nrPlots <- length(input$vizvars1)
		ifelse(nrPlots > 1, return((input$viz_plot_height/2) * ceiling(nrPlots / 2)), return(input$viz_plot_height))
	} else {
 		return(input$viz_plot_height)
	}
})

output$visualize <- renderPlot({

	# if(isolate(input$datatabs) != 'Visualize') return(invisible())

 	if(is.null(input$viz_facet_col)) return()
	# if(input$vizvars1 == "")
	if(is.null(input$vizvars1) || input$vizvars1 == "")
		return(plot(x = 1, type = 'n', main="Please select variables from the dropdown menus to create a plot.", axes = FALSE, xlab = "", ylab = ""))

  withProgress(message = 'Making plot', value = 0, {
	  plots <- .visualize()
  })

	if(!is.null(plots)) return(plots)

}, width = viz_plot_width, height = viz_plot_height)

.visualize <- reactive({
	# need dependency on ..
	input$viz_plot_height; input$viz_plot_width
	visualize(input$datasets, input$vizvars1, input$vizvars2, input$viz_select, input$viz_multiple, input$viz_facet_row,
			input$viz_facet_col, input$viz_color, input$viz_line, input$viz_loess, input$viz_jitter)
})

observe({
  if(is.null(input$visualizeReport) || input$visualizeReport == 0) return()
  isolate({
		inp <- list(input$datasets, input$vizvars1, input$vizvars2, input$viz_select, input$viz_multiple, input$viz_facet_row,
			input$viz_facet_col, input$viz_color, input$viz_line, input$viz_loess, input$viz_jitter)
		updateReportViz(inp,"visualize", round(7 * viz_plot_width()/650,2), round(7 * viz_plot_height()/650,2))
  })
})

visualize <- function(datasets, vizvars1, vizvars2, viz_select, viz_multiple, viz_facet_row, viz_facet_col,
	viz_color, viz_line, viz_loess, viz_jitter) {

	# inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
	dat <- values[[datasets]]
	if(sum(vizvars1 %in% colnames(dat)) != length(vizvars1)) return()

  if(viz_select != '') {
    selcom <- viz_select
    selcom <- gsub(" ", "", selcom)
    seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
    if(!is(seldat, 'try-error')) {
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
      }
    }
  }

	plots <- list()
	if(vizvars2 == "") {

		for(i in vizvars1) {
			plots[[i]] <- ggplot(dat, aes_string(x=i)) + geom_histogram()
			if(viz_multiple == "single") {
				# if(is.null(viz_facet_col) || is.null(viz_facet_row)) return()
			  facets <- paste(viz_facet_row, '~', viz_facet_col)
			  if (facets != '. ~ .')
				  plots[[i]] <- plots[[i]] + facet_grid(facets)
			}
		}
	} else {
		# if(sum(vizvars2 %in% colnames(dat)) != length(vizvars2)) return()
		for(i in vizvars1) {
			for(j in vizvars2) {
				if(is.factor(dat[,i])) {
				  if(is.factor(dat[,j])) {
				  	plots[[i]] <- ggplot(dat, aes_string(x=i, fill=j)) + geom_bar(position = "fill", alpha=.3) +
				  		labs(list(y = ""))
				  } else {
					  plots[[i]] <- ggplot(dat, aes_string(x=i, y=j, fill=i)) + geom_boxplot(alpha = .3)
					}
			  } else if(is.factor(dat[,j])) {
				  plots[[i]] <- ggplot(dat, aes_string(x=j, y=i, fill=j)) + geom_boxplot(alpha = .3) +
				 		coord_flip()
			  } else {
				  plots[[i]] <- ggplot(dat, aes_string(x=i, y=j)) + geom_point()
			  }

		    if(viz_multiple == "single") {
			    facets <- paste(viz_facet_row, '~', viz_facet_col)
			    if (facets != '. ~ .')
					  plots[[i]] <- plots[[i]] + facet_grid(facets)
				}

				if(!(is.factor(dat[,i]) & is.factor(dat[,j]))) {
			    if (viz_jitter) plots[[i]] <- plots[[i]] + geom_jitter()
				}

				if(!is.factor(dat[,i]) & !is.factor(dat[,j])) {
			    if (viz_color != '') plots[[i]] <- plots[[i]] + aes_string(color=viz_color) + scale_fill_brewer()
			    if (viz_line) plots[[i]] <- plots[[i]] + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75,
			    	linetype = "dashed", colour = 'black')
			    if (viz_loess) plots[[i]] <- plots[[i]] + geom_smooth(span = 1, size = .75, linetype = "dotdash")
			    if (viz_jitter) plots[[i]] <- plots[[i]] + geom_jitter()
			  }
			}
		}
	}

	suppressWarnings(suppressMessages(do.call(grid.arrange, c(plots, list(ncol = min(2,length(plots)))))))
}
