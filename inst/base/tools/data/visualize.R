#######################################
# Vizualize data
#######################################
output$uiVizvars1 <- renderUI({
  vars <- varnames()
  if(input$viz_multiple == 'multiple') {
    selectInput(inputId = "vizvars1", label = "X-variable", choices = vars,
      selected = state_multvar("vizvars1",vars),
      multiple = TRUE, size = min(10, length(vars)), selectize = FALSE)
  } else {
    selectInput(inputId = "vizvars1", label = "X-variable", choices = vars,
      selected = state_multvar("vizvars1",vars), multiple = FALSE, selectize = TRUE)
  }
})

output$uiVizvars2 <- renderUI({
  vars <- varnames()
  selectizeInput(inputId = "vizvars2", label = "Y-variable",
                 choices = c("None" = "None", vars),
                 selected = state_init_list("vizvars2","None", vars),
                 multiple = FALSE)
})

output$uiViz_color <- renderUI({
  if(is.null(input$vizvars2)) return() 	# can't have an XY plot without an X
  vars <- c("None" = "None", varnames())
  selectizeInput("viz_color", "Color", vars,
                 selected = state_init_list("viz_color","None", vars),
                 multiple = FALSE)
})

output$uiViz_facet_row <- renderUI({
  isFct <- "factor" == getdata_class()
  vars <- c("None" = ".", varnames()[isFct])
  selectizeInput("viz_facet_row", "Facet row", vars,
                 selected = state_init_list("viz_facet_row", ".", vars),
                 multiple = FALSE)
})

output$uiViz_facet_col <- renderUI({
#   if(length(input$vizvars2) > 1) return()
  isFct <- "factor" == getdata_class()
  vars <- c("None" = ".", varnames()[isFct])
  selectizeInput("viz_facet_col", 'Facet column', vars,
                 selected = state_init_list("viz_facet_col", ".", vars),
                 multiple = FALSE)
})

viz_multiple <- c("Single" = "single", "Multiple" = "multiple")
viz_check <- c("Line" = "line", "Loess" = "loess", "Jitter" = "jitter")

output$ui_Visualize <- renderUI({
  list(wellPanel(
    radioButtons("viz_multiple", "Number of plots:", viz_multiple,
                 state_init_list("viz_multiple","single", viz_multiple),
                 inline = TRUE),
    uiOutput("uiVizvars1"),
    uiOutput("uiVizvars2"),
    conditionalPanel(condition = "input.viz_multiple == 'single'",
      uiOutput("uiViz_facet_row"),
      uiOutput("uiViz_facet_col")
    ),
    conditionalPanel(condition = "input.vizvars2 != 'None'",
      uiOutput("uiViz_color"),
      checkboxGroupInput("viz_check", "", viz_check,
        selected = state_init_list("viz_check","", viz_check),
        inline = TRUE)
    ),
    div(class="row",
        div(class="col-xs-6",
            numericInput("viz_plot_height", label = "Plot height:", min = 100, step = 50,
                         value = state_init("viz_plot_height", r_data$plotHeight))),
        div(class="col-xs-6",
            numericInput("viz_plot_width", label = "Plot width:", min = 100, step = 50,
                         value = state_init("viz_plot_width", r_data$plotWidth)))
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
  ifelse(is.null(input$viz_plot_width), return(r_data$plotWidth), return(input$viz_plot_width))
})

viz_plot_height <- reactive({
  # if(is.null(input$viz_plot_height)) return(r_data$plotHeight)
  if(is.null(input$viz_plot_height)) return(r_data$plotHeight)
  if(input$viz_multiple == "multiple") {
    nrPlots <- length(input$vizvars1)
    ifelse(nrPlots > 1,
           return((input$viz_plot_height/2) * ceiling(nrPlots / 2)),
           return(input$viz_plot_height))
  } else {
    return(input$viz_plot_height)
  }
})

output$visualize <- renderPlot({

  if(is.null(input$viz_facet_col)) return()
  if(is.null(input$vizvars1) || input$vizvars1 == "None")
    return(
      plot(x = 1, type = 'n',
           main="Please select variables from the dropdown menus to create a plot.",
           axes = FALSE, xlab = "", ylab = "")
    )

  withProgress(message = 'Making plot', value = 0, {
    plots <- .visualize()
  })

  if(!is.null(plots)) return(plots)

}, width = viz_plot_width, height = viz_plot_height)

.visualize <- reactive({
  # need dependency on ..
  input$viz_plot_height; input$viz_plot_width

  if(input$vizvars1 %>% not_available) return()

  visualize(input$dataset, input$vizvars1, input$vizvars2, input$viz_multiple,
            input$viz_facet_row, input$viz_facet_col, input$viz_color, input$viz_check)
})

observe({
  if(is.null(input$visualizeReport) || input$visualizeReport == 0) return()
  isolate({
    inp <- list(input$dataset, input$vizvars1, input$vizvars2, input$viz_multiple,
                input$viz_facet_row, input$viz_facet_col, input$viz_color, input$viz_check)
    updateReportViz(inp,"visualize", round(7 * viz_plot_width()/650,2),
                    round(7 * viz_plot_height()/650,2))
  })
})

visualize <- function(dataset, vizvars1, vizvars2, viz_multiple, viz_facet_row,
                      viz_facet_col, viz_color, viz_check) {

  # inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
  # dat <- r_data[[dataset]]
  dat <- getdata()
  # if(sum(vizvars1 %in% colnames(dat)) != length(vizvars1)) return()

  plots <- list()
  if(vizvars2 == "None") {

    for(i in vizvars1) {
      # bw <- select_(dat,i) %>% range %>% diff(.)/20
      # plots[[i]] <- ggplot(dat, aes_string(x=i)) + geom_histogram(binwidth = bw)
      plots[[i]] <- ggplot(dat, aes_string(x=i)) + geom_histogram()
      # plots[[i]] <- ggplot(dat, aes_string(x=i)) + geom_density(fill="green", alpha = .3)
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
            plots[[i]] <- ggplot(dat, aes_string(x=i, fill=j)) + geom_bar(position = "fill", alpha=.7) +
              labs(list(y = ""))
          } else {
            plots[[i]] <- ggplot(dat, aes_string(x=i, y=j, fill=i)) + geom_boxplot(alpha = .7)
          }
        } else if(is.factor(dat[,j])) {
          plots[[i]] <- ggplot(dat, aes_string(x=j, y=i, fill=j)) + geom_boxplot(alpha = .7) +
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
          if ("jitter" %in% viz_check) plots[[i]] <- plots[[i]] + geom_jitter()
        }

        if(!is.factor(dat[,i]) & !is.factor(dat[,j])) {
          if (!is.null(viz_color) && viz_color != 'None')
            plots[[i]] <- plots[[i]] + aes_string(color=viz_color) + scale_fill_brewer()
          if ("line" %in% viz_check)
            plots[[i]] <- plots[[i]] + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75, linetype = "dashed", colour = 'black')
          if ("loess" %in% viz_check) plots[[i]] <- plots[[i]] + geom_smooth(span = 1, size = .75, linetype = "dotdash")
          if ("jitter" %in% viz_check) plots[[i]] <- plots[[i]] + geom_jitter()
        }
      }
    }
  }

  sshh(do.call(grid.arrange, c(plots, list(ncol = length(plots) %>% min(2)))))
}
