# variable selection in the datatabs views
output$vizvars1 <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()

	selectInput(inputId = "vizvars1", label = "X-variable", choices = as.list(cols), selected = NULL, multiple = FALSE)
})

# variable selection
output$vizvars2 <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	# selectInput(inputId = "vizvars2", label = "Y-variable", choices = as.list(cols[-which(cols == input$vizvars1)]), selected = NULL, multiple = TRUE)
	selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols[-which(cols == input$vizvars1)])), selected = "", multiple = FALSE)
})

output$viz_color <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	selectInput('viz_color', 'Color', c('None'="", as.list(cols)))
})

output$viz_facet_row <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	# isFct <- sapply(getdata(), is.factor || is.integer)
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
	selectInput('viz_facet_row', 'Facet row', c(None='.', as.list(cols)))
})

output$viz_facet_col <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	# isFct <- sapply(getdata(), is.factor || is.integer)
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
	selectInput('viz_facet_col', 'Facet col', c(None='.', as.list(cols)))
})

output$visualize <- renderPlot({
	if(is.null(input$datasets) || is.null(input$vizvars2)) return()
	if(input$datatabs != 'Visualize') return()

		# inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
		dat <- getdata()

		if(input$vizvars2 == "") {
			p <- ggplot(dat, aes_string(x=input$vizvars1)) + geom_histogram(colour = 'black', fill = 'blue') 
			return(print(p))
		} else {
		  p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_point()
		}

    if (input$viz_color != '') {
    	# p <- p + aes_string(color=input$viz_color) + scale_colour_gradient(colors=rainbow(4))
    	p <- p + aes_string(color=input$viz_color) + scale_fill_brewer()
    }

    facets <- paste(input$viz_facet_row, '~', input$viz_facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    if (input$viz_jitter)
      p <- p + geom_jitter()
    if (input$viz_smooth)
      p <- p + geom_smooth(method = "lm", size = .75, linetype = "dotdash")
    
    print(p)

}, width = 700, height = 700)

# will be used in ui.R
output$ui_visualize <- renderUI({
	ui_visualize()
})

ui_visualize <- function() {
	wellPanel(
		uiOutput("vizvars1"),
		uiOutput("vizvars2"),
		conditionalPanel(condition = "input.vizvars2 != ''",
		  uiOutput("viz_color"),
		  uiOutput("viz_facet_row"),
		  uiOutput("viz_facet_col"),
		  checkboxInput('viz_smooth', 'Smooth', value = TRUE),
		  checkboxInput('viz_jitter', 'Jitter', value = FALSE)
		)
	)
}