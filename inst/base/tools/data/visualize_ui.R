viz_type <- c("Histogram" = "hist", "Density" = "density", "Scatter" = "scatter",
              "Line" = "line", "Bar" = "bar", "Box-plot" = "box")
viz_check <- c("Line" = "line", "Loess" = "loess", "Jitter" = "jitter")
viz_axes <-  c("Flip" = "flip", "Log X" = "log_x", "Log Y" = "log_y",
               "Scale-y" = "scale_y", "Density" = "density", "Sort" = "sort")

## list of function arguments
viz_args <- as.list(formals(visualize))

## list of function inputs selected by user
viz_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  viz_args$data_filter <- if (input$show_filter) input$data_filter else ""
  viz_args$dataset <- input$dataset
  viz_args$shiny <- input$shiny
  for (i in r_drop(names(viz_args)))
    viz_args[[i]] <- input[[paste0("viz_",i)]]
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

## Y - variable
output$ui_viz_yvar <- renderUI({
  if (is.null(input$viz_type)) return()
  vars <- varying_vars()
  if (not_available(vars)) return()
  vars <- vars["date" != .getclass()[vars]]
  if (input$viz_type %in% c("line","bar","scatter")) {
    vars <- vars["factor" != .getclass()[vars]]
    vars <- vars["character" != .getclass()[vars]]
  }

  isolate({
    ## keep the same y-variable 'active' if possible
    sel <-
      if (available(input$viz_yvar) && all(input$viz_yvar %in% vars))
        input$viz_yvar
      else
        state_multiple("viz_yvar", vars)
  })

  selectInput(inputId = "viz_yvar", label = "Y-variable:",
    choices = vars,
    selected = sel,
    multiple = TRUE, size = min(3, length(vars)), selectize = FALSE)
})

## X - variable
output$ui_viz_xvar <- renderUI({
  if (is.null(input$viz_type)) return()
  vars <- varying_vars()
  if (not_available(vars)) return()
  if (input$viz_type == "hist") vars <- vars["date" != .getclass()[vars]]
  if (input$viz_type == "density") vars <- vars["factor" != .getclass()[vars]]
  if (input$viz_type %in% c("box", "bar")) vars <- groupable_vars()

  isolate({
    ## keep the same x-variable 'active' if possible
    sel <-
      if (available(input$viz_xvar) && all(input$viz_xvar %in% vars))
        input$viz_xvar
      else
        state_multiple("viz_xvar", vars)
  })

  selectInput(inputId = "viz_xvar", label = "X-variable:", choices = vars,
    selected = sel,
    multiple = TRUE, size = min(3, length(vars)), selectize = FALSE)
})

output$ui_viz_facet_row <- renderUI({
  vars <- c("None" = ".", groupable_vars())
  selectizeInput("viz_facet_row", "Facet row", vars,
    selected = state_single("viz_facet_row", vars, "."), multiple = FALSE)
})

output$ui_viz_facet_col <- renderUI({
  vars <- c("None" = ".", groupable_vars())
  selectizeInput("viz_facet_col", 'Facet column', vars,
    selected = state_single("viz_facet_col", vars, "."), multiple = FALSE)
})

output$ui_viz_color <- renderUI({
  vars <- c("None" = "none", varnames())
  sel <- state_single("viz_color", vars, "none")
  selectizeInput("viz_color", "Color", vars, selected = sel, multiple = FALSE)
})

output$ui_viz_fill <- renderUI({
  vars <- c("None" = "none", groupable_vars())
  sel <- state_single("viz_fill", vars, "none")
  selectizeInput("viz_fill", "Fill", vars, selected = sel, multiple = FALSE)
})

output$ui_viz_axes <- renderUI({
  if (is.null(input$viz_type)) return()
  ind <- 1
  if (input$viz_type %in% c("line","scatter")) ind <- 1:3
  if (input$viz_type == "hist") ind <- c(ind, 5)
  if (!is_empty(input$viz_facet_row, ".") || !is_empty(input$viz_facet_col, "."))  ind <- c(ind, 4)
  if (input$viz_type == "bar" && input$viz_facet_row == "." && input$viz_facet_col == ".") ind <- c(ind, 6)
  checkboxGroupInput("viz_axes", NULL, viz_axes[ind],
    selected = state_init("viz_axes", ""),
    inline = TRUE)
})

output$ui_viz_check <- renderUI({
  if (is.null(input$viz_type)) return()
  # ind <- if (input$viz_type == "scatter") 1:3 else 1
  ind <- 1:3
  checkboxGroupInput("viz_check", NULL, viz_check[ind],
    selected = state_init("viz_check"),
    inline = TRUE)
})

observeEvent(input$viz_type, {
  isolate({
    if (input$viz_type != "hist")
      updateSliderInput(session, "viz_bins", value = 10)
  })
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
      conditionalPanel(condition = "input.viz_type == 'bar' |
                                    input.viz_type == 'hist' |
                                    input.viz_type == 'density'",
        uiOutput("ui_viz_fill")
      ),
      conditionalPanel(condition = "input.viz_type == 'scatter' |
                                    input.viz_type == 'line' |
                                    input.viz_type == 'box'",
        uiOutput("ui_viz_color"),
        uiOutput("ui_viz_check")
      ),
      conditionalPanel(condition = "input.viz_type == 'hist'",
        sliderInput("viz_bins", label = "Number of bins:",
          min = 1, max = 50, value = state_init("viz_bins",10),
          step = 1)
      ),
      uiOutput("ui_viz_axes"),
      sliderInput("viz_alpha", label = "Opacity:", min = 0, max = 1,
        value = state_init("viz_alpha",.5), step = .01),
      div(class="row",
          div(class="col-xs-6",
              numericInput("viz_plot_height", label = "Plot height:", min = 100,
                           max = 2000, step = 50,
                           value = state_init("viz_plot_height", r_data$plot_height))),
          div(class="col-xs-6",
              numericInput("viz_plot_width", label = "Plot width:", min = 100,
                           max = 2000, step = 50,
                           value = state_init("viz_plot_width", r_data$plot_width)))
      )
    ),
    help_and_report(modal_title = "Visualize",
                    fun_name = "visualize",
                    help_file = inclRmd(file.path(r_path,"base/tools/help/visualize.md")))
  )
})

viz_plot_width <- reactive({
  if (is.null(input$viz_plot_width)) r_data$plot_width else input$viz_plot_width
})

viz_plot_height <- reactive({
  if (is.null(input$viz_plot_height)) {
    r_data$plot_height
  } else {
    lx <- if (not_available(input$viz_xvar)) 1 else length(input$viz_xvar)
    ly <- ifelse (not_available(input$viz_yvar) || input$viz_type %in% c("hist","density"),
                  1, length(input$viz_yvar))
    (lx * ly) %>%
    { if (. > 1)
        (input$viz_plot_height/2) * ceiling(. / 2)
      else
        input$viz_plot_height
    }
  }
})

output$visualize <- renderPlot({
  if (not_available(input$viz_xvar))
    return(
      plot(x = 1, type = 'n',
           main="\nPlease select variables from the dropdown menus to create a plot",
           axes = FALSE, xlab = "", ylab = "")
    )

  withProgress(message = 'Making plot', value = 0, {
    .visualize() %>% { if (grid::is.grob(.)) . else return(invisible()) } %>% print
  })
}, width = viz_plot_width, height = viz_plot_height)

.visualize <- reactive({
  ## need dependency on ..
  input$viz_plot_height; input$viz_plot_width

  if (not_available(input$viz_xvar)) return()
  if (input$viz_type %in% c("scatter","line", "box", "bar") && not_available(input$viz_yvar)) return()
  if (input$viz_type == "box" && !all(input$viz_xvar %in% groupable_vars()))
    return()

  viz_inputs() %>% { .$shiny <- TRUE; . } %>% do.call(visualize, .)
})

observeEvent(input$visualize_report, {
  isolate({
    update_report(inp_main = clean_args(viz_inputs(), viz_args),
                  fun_name = "visualize", outputs = character(0),
                  pre_cmd = "", figs = TRUE,
                  fig.width = round(7 * viz_plot_width()/650,2),
                  fig.height = round(7 * viz_plot_height()/500,2))
  })
})
