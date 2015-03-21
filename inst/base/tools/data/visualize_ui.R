viz_type <- c("Histogram" = "hist", "Density" = "density", "Scatter" = "scatter",
              "Line" = "line", "Bar" = "bar", "Box-plot" = "box")
viz_check <- c("Line" = "line", "Loess" = "loess", "Jitter" = "jitter")
viz_axes <-  c("Flip" = "flip", "Log X" = "log_x", "Log Y" = "log_y")

# list of function arguments
viz_args <- as.list(formals(visualize))

# list of function inputs selected by user
viz_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(viz_args))
    viz_args[[i]] <- input[[i]]
  if(!input$show_filter) viz_args$data_filter = ""
  viz_args
})

#######################################
# Vizualize data
#######################################
output$ui_viz_type <- renderUI({
  selectInput(inputId = "viz_type", label = "Plot-type:", choices = viz_type,
    selected = state_multiple("viz_type", viz_type),
    multiple = FALSE)
})

# X - variable
output$ui_viz_xvar <- renderUI({
  if(is.null(input$viz_type)) return()
  vars <- varnames()
  # if(input$viz_type %in% c("density","line")) vars <- vars["factor" != getdata_class()]
  if(input$viz_type %in% c("density")) vars <- vars["factor" != getdata_class()]
  if(input$viz_type %in% c("box", "bar")) vars <- groupable_vars()
  selectInput(inputId = "viz_xvar", label = "X-variable:", choices = vars,
    selected = state_multiple("viz_xvar",vars),
    multiple = TRUE, size = min(5, length(vars)), selectize = FALSE)
})

# Y - variable
output$ui_viz_yvar <- renderUI({
  if(is.null(input$viz_type)) return()
  vars <- varnames()
  if(input$viz_type %in% c("line")) vars <- vars["factor" != getdata_class()]
  selectizeInput(inputId = "viz_yvar", label = "Y-variable:",
                 choices = c("None" = "none", vars),
                 selected = state_single("viz_yvar", vars, "none"),
                 multiple = FALSE)
})

output$ui_viz_facet_row <- renderUI({
  isFct <- "factor" == getdata_class()
  vars <- c("None" = ".", varnames()[isFct])
  selectizeInput("viz_facet_row", "Facet row", vars,
                 selected = state_single("viz_facet_row", vars, "."),
                 multiple = FALSE)
})

output$ui_viz_facet_col <- renderUI({
  isFct <- "factor" == getdata_class()
  vars <- c("None" = ".", varnames()[isFct])
  selectizeInput("viz_facet_col", 'Facet column', vars,
                 selected = state_single("viz_facet_col", vars, "."),
                 multiple = FALSE)
})

output$ui_viz_color <- renderUI({
  if(not_available(input$viz_yvar)) return()  # can't have an XY plot without an X
  vars <- c("None" = "none", varnames())
  sel <- state_single("viz_color", vars, "none")
  selectizeInput("viz_color", "Color", vars,
                 selected = sel,
                 multiple = FALSE)
})

output$ui_viz_axes <- renderUI({
  if(is.null(input$viz_type)) return()
  ind <- 1
  if(input$viz_type %in% c("line","scatter")) ind <- 1:3
  checkboxGroupInput("viz_axes", NULL, viz_axes[ind],
    selected = state_init("viz_axes"),
    inline = TRUE)
})

output$ui_Visualize <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_viz_type"),
      conditionalPanel(condition = "input.viz_type != 'hist' & input.viz_type != 'density'",
        uiOutput("ui_viz_yvar")
      ),
      uiOutput("ui_viz_xvar"),
      uiOutput("ui_viz_facet_row"),
      uiOutput("ui_viz_facet_col"),
      conditionalPanel(condition = "input.viz_type == 'scatter' |
                                    input.viz_type == 'line' |
                                    input.viz_type == 'box'",
        uiOutput("ui_viz_color"),
        checkboxGroupInput("viz_check", NULL, viz_check,
          selected = state_init("viz_check"),
          inline = TRUE)
      ),
      uiOutput("ui_viz_axes"),
      div(class="row",
          div(class="col-xs-6",
              numericInput("viz_plot_height", label = "Plot height:", min = 100, step = 50,
                           value = state_init("viz_plot_height", r_data$plot_height))),
          div(class="col-xs-6",
              numericInput("viz_plot_width", label = "Plot width:", min = 100, step = 50,
                           value = state_init("viz_plot_width", r_data$plot_width)))
      )
    ),
    help_and_report(modal_title = "Visualize",
                fun_name = "visualize",
                help_file = inclRmd("../base/tools/help/visualize.md"))
  )
})

viz_plot_width <- reactive({
  if(is.null(input$viz_plot_width)) r_data$plot_width else input$viz_plot_width
})

viz_plot_height <- reactive({
  if(is.null(input$viz_plot_height)) {
    r_data$plot_height
  } else {
    length(input$viz_xvar) %>%
    { if(. > 1)
        (input$viz_plot_height/2) * ceiling(. / 2)
      else
        input$viz_plot_height
    }
  }
})

output$visualize <- renderPlot({
  if(is_empty(input$viz_xvar, "none"))
    return(
      plot(x = 1, type = 'n',
           main="\nPlease select variables from the dropdown menus to create a plot",
           axes = FALSE, xlab = "", ylab = "")
    )

  withProgress(message = 'Making plot', value = 0, {
    .visualize() %>% { if(is.list(.)) . else return() }
  })
}, width = viz_plot_width, height = viz_plot_height)

.visualize <- reactive({
  # need dependency on ..
  input$viz_plot_height; input$viz_plot_width

  if(not_available(input$viz_xvar)) return()
  if(input$viz_type %in% c("scatter","line", "box", "bar")
     && is_empty(input$viz_yvar, "none")) return()

  do.call(visualize, viz_inputs())
})

observe({
  if(not_pressed(input$visualize_report)) return()
  isolate({
    update_report(inp_main = clean_args(viz_inputs(), viz_args),
                   fun_name = "visualize", outputs = character(0),
                   figs = TRUE,
                   fig.width = round(7 * viz_plot_width()/650,2),
                   fig.height = round(7 * viz_plot_height()/500,2))
  })
})
