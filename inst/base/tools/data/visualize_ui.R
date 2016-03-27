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
  # print(paste0(names(viz_args), viz_args, collapse = ", "))
  # isolate({
  #   cat(paste0(names(viz_args), " ", viz_args, collapse = ", "), file = stderr(), "\n")
  # })
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
  # if (is_empty(input$viz_type)) return()
  req(input$viz_type)
  vars <- varying_vars()
  # if (not_available(vars)) return()
  req(available(vars))
  vars <- vars["date" != .getclass()[vars]]
  if (input$viz_type %in% c("line","bar","scatter","box")) {
    vars <- vars["character" != .getclass()[vars]]
  }
  if (input$viz_type %in% c("line","scatter","box")) {
    ## allow factors in yvars for bar plots
    vars <- vars["factor" != .getclass()[vars]]
  }

  selectInput(inputId = "viz_yvar", label = "Y-variable:",
    choices = vars,
    # selected = use_input("viz_yvar", vars, fun = "state_multiple"),
    selected = state_multiple("viz_yvar", vars),
    multiple = TRUE, size = min(3, length(vars)), selectize = FALSE)
})

## X - variable
output$ui_viz_xvar <- renderUI({
  # if (is_empty(input$viz_type)) return()
  req(input$viz_type)
  vars <- varying_vars()
  # vars <- varnames()
  # if (not_available(vars)) return()
  req(available(vars))
  if (input$viz_type == "hist") vars <- vars["date" != .getclass()[vars]]
  if (input$viz_type == "density") vars <- vars["factor" != .getclass()[vars]]
  if (input$viz_type %in% c("box", "bar")) vars <- groupable_vars_nonum()

  selectInput(inputId = "viz_xvar", label = "X-variable:", choices = vars,
    # selected = use_input("viz_xvar", vars, fun = "state_multiple"),
    selected = state_multiple("viz_xvar", vars),
    multiple = TRUE, size = min(3, length(vars)), selectize = FALSE)
})

# output$ui_viz_comby <- renderUI({
#   req(input$viz_yvar)
#   if (length(input$viz_yvar) < 2) {
#     # isolate({
#     #   r_state[["viz_comby"]] <<- FALSE
#     #   updateCheckboxInput(session, "viz_comby", value = FALSE)
#     # })
#     r_state[["viz_comby"]] <<- FALSE
#     return()
#   }
#   checkboxInput("viz_comby", "Combine Y-variables in one plot",
#     .state_init("viz_comby", FALSE))
# })

# output$ui_viz_combx <- renderUI({
#   req(input$viz_xvar)
#   if (length(input$viz_xvar) < 2) {
#     # isolate({
#       # r_state[["viz_combx"]] <<- FALSE
#       # updateCheckboxInput(session, "viz_combx", value = FALSE)
#     # })
#     r_state[["viz_combx"]] <<- FALSE
#     checkboxInput("viz_combx", "Combine X-variables in one plot", FALSE)
#     # return()
#   } else {
#     checkboxInput("viz_combx", "Combine X-variables in one plot",
#       state_init("viz_combx", FALSE))
#   }
# })

# observeEvent(length(input$viz_yvar) < 2, {
#   r_state[["viz_comby"]] <<- FALSE
#   updateCheckboxInput(session, "viz_comby", value = FALSE)
# })

output$ui_viz_comby <- renderUI({
  # if (length(input$viz_yvar) < 2) return(NULL)
  # checkboxInput("viz_comby", "Combine Y-variables in one plot",
  #   state_init("viz_comby", FALSE))

  if (length(input$viz_yvar) > 1) {
    checkboxInput("viz_comby", "Combine Y-variables in one plot",
      state_init("viz_comby", FALSE))
  } else {
    return()
  }
})

# observeEvent(!is.null(input$viz_xvar) && length(input$viz_xvar) < 2, {
#   r_state[["viz_combx"]] <<- FALSE
#   updateCheckboxInput(session, "viz_combx", value = FALSE)
# })

# observeEvent(length(input$viz_xvar) == 1, {
#   r_state[["viz_combx"]] <<- FALSE
#   updateCheckboxInput(session, "viz_combx", value = FALSE)
# })

# observeEvent(input$viz_xvar, {
#   if (length(input$viz_xvar) < 2) {
#     r_state[["viz_combx"]] <<- FALSE
#     updateCheckboxInput(session, "viz_combx", value = FALSE)
#   }
# })

output$ui_viz_combx <- renderUI({
  # req(length(input$viz_xvar) > 1)
  # if (!is.null(input$viz_xvar) && length(input$viz_xvar) < 2) return(NULL)
  # if (!is.null(input$viz_xvar) && length(input$viz_xvar) < 2) return(NULL)
  if (length(input$viz_xvar) > 1) {
    checkboxInput("viz_combx", "Combine X-variables in one plot",
      state_init("viz_combx", FALSE))
  } else {
    return()
  }
})

# observeEvent(input$viz_xvar < 2, {
#   # if (length(input$viz_xvar) < 2) {
    # r_state[["viz_combx"]] <<- FALSE
    # updateCheckboxInput(session, "viz_combx", value = FALSE)
  # }
# })

# ?shiny::observeEvent

# observeEvent(input$viz_xvar < 2, {
# observe({
#   if (length(input$viz_xvar) < 2)
#     updateCheckboxInput(session, "viz_combx", value = FALSE)
# })

# output$ui_viz_combx <- renderUI({
#   # req(input$viz_xvar > 1) {
#   # req(input$viz_xvar)
#   if (length(input$viz_xvar) > 1) {
#     checkboxInput("viz_combx", "Combine X-variables in one plot", FALSE)
#     # checkboxInput("viz_combx", "Combine X-variables in one plot",
#     #   .state_init("viz_combx", FALSE))
#   } else {
#     # updateCheckboxInput(session, "viz_combx", value = FALSE)
#     return()
#   }

#   # } else {
#   #   r_state[["viz_combx"]] <<- FALSE
#   #   isolate(updateCheckboxInput(session, "viz_combx", value = FALSE))
#   #   return()
#   # }
# })

observeEvent(input$viz_type, {
  if (input$viz_type %in% c("hist", "density")) {
    updateCheckboxInput(session, "viz_comby", value = FALSE)
  } else {
    updateCheckboxInput(session, "viz_combx", value = FALSE)
  }
})

# observeEvent(input$viz_combx, {
#   if (input$viz_combx) {
#   # if (input$viz_combx && length(input$viz_xvar) > 1) {
#     updateCheckboxInput(session, "viz_color", value = "none")
#     updateCheckboxInput(session, "viz_fill", value = "none")
#   }
# })

# observeEvent(input$viz_comby, {
#   if (input$viz_comby) {
#   # if (input$viz_combx && input$viz_yvar > 1) {
#     updateCheckboxInput(session, "viz_color", value = "none")
#     updateCheckboxInput(session, "viz_fill", value = "none")
#   }
# })

observeEvent(input$viz_check, {
  if (!"loess" %in% input$viz_check && input$viz_smooth != 1)
    updateSliderInput(session, "viz_smooth", value = 1)
})

output$ui_viz_facet_row <- renderUI({
  vars <- c("None" = ".", groupable_vars_nonum())
  selectizeInput("viz_facet_row", "Facet row", vars,
    # selected = use_input("viz_facet_row", vars, init = ".")),
    selected = state_single("viz_facet_row", vars, init = "."),
    multiple = FALSE)
})

output$ui_viz_facet_col <- renderUI({
  vars <- c("None" = ".", groupable_vars_nonum())
  selectizeInput("viz_facet_col", 'Facet column', vars,
    selected = state_single("viz_facet_col", vars, init = "."),
    multiple = FALSE)
})

output$ui_viz_color <- renderUI({
  req(input$viz_type)
  if (input$viz_type == "line")
    vars <- c("None" = "none", groupable_vars())
  else
    vars <- c("None" = "none", varnames())

  if (isTRUE(input$viz_comby) && length(input$viz_yvar) > 1) {
    vars <- c("None" = "none")
    selectizeInput("viz_color", "Color", vars, multiple = FALSE, selected = "none")
  } else {
    selectizeInput("viz_color", "Color", vars, multiple = FALSE,
      selected = state_single("viz_color", vars, init = "none"))
  }
})

output$ui_viz_fill <- renderUI({
  # if (isTRUE(input$viz_combx)) {
  if (isTRUE(input$viz_combx) && length(input$viz_xvar) > 1) {
    vars <- c("None" = "none")
    selectizeInput("viz_fill", "Fill", vars, multiple = FALSE, selected = "none")
  } else {
    vars <- c("None" = "none", groupable_vars())
    selectizeInput("viz_fill", "Fill", vars, multiple = FALSE,
      selected = state_single("viz_fill", vars, init = "none"))
  }
})

output$ui_viz_axes <- renderUI({
  if (is_empty(input$viz_type)) return()
  ind <- 1
  if (input$viz_type %in% c("line","scatter")) ind <- 1:3
  if (input$viz_type %in% c("hist","density")) ind <- c(1:2, 5)
  if (input$viz_type %in% c("bar","box")) ind <- c(1, 3)
  if (!is_empty(input$viz_facet_row, ".") || !is_empty(input$viz_facet_col, "."))  ind <- c(ind, 4)
  if (input$viz_type == "bar" && input$viz_facet_row == "." && input$viz_facet_col == ".") ind <- c(ind, 6)

  checkboxGroupInput("viz_axes", NULL, viz_axes[ind],
    selected = state_init("viz_axes", ""),
    inline = TRUE)
})

output$ui_viz_check <- renderUI({
  if (is_empty(input$viz_type)) return()
  if (input$viz_type == "scatter") {
    ind <- 1:3
  } else if (input$viz_type == "box") {
    ind <- 3
  } else {
    ind <- c()
  }

  if (!input$viz_type %in% c("scatter", "box"))
    r_state$viz_check <<- gsub("jitter","",r_state$viz_check)
  if (input$viz_type != "scatter") {
    r_state$viz_check <<- gsub("line","",r_state$viz_check)
    r_state$viz_check <<- gsub("loess","",r_state$viz_check)
  }

  checkboxGroupInput("viz_check", NULL, viz_check[ind],
    selected = state_init("viz_check", ""),
    inline = TRUE)
})

output$ui_Visualize <- renderUI({
  tagList(
    wellPanel(
      checkboxInput("viz_pause", "Pause plotting", state_init("viz_pause", FALSE)),
      uiOutput("ui_viz_type"),
      conditionalPanel(condition = "input.viz_type != 'hist' & input.viz_type != 'density'",
        uiOutput("ui_viz_yvar"),
        uiOutput("ui_viz_comby")
      ),
      uiOutput("ui_viz_xvar"),
      conditionalPanel("input.viz_type == 'hist' | input.viz_type == 'density'",
        uiOutput("ui_viz_combx")
      ),
      uiOutput("ui_viz_facet_row"),
      uiOutput("ui_viz_facet_col"),
      conditionalPanel(condition = "input.viz_type == 'bar' |
                                    input.viz_type == 'hist' |
                                    input.viz_type == 'density'",
        # conditionalPanel("input.viz_combx != true & input.viz_comby != true",
          uiOutput("ui_viz_fill")
        # )
      ),
      conditionalPanel(condition = "input.viz_type == 'scatter' |
                                    input.viz_type == 'line' |
                                    input.viz_type == 'box'",
        # conditionalPanel("input.viz_combx != true & input.viz_comby != true",
          uiOutput("ui_viz_color"),
        # ),
        uiOutput("ui_viz_check")
      ),
      uiOutput("ui_viz_axes"),
      conditionalPanel(condition = "input.viz_type == 'hist'",
        sliderInput("viz_bins", label = "Number of bins:",
          min = 1, max = 50, value = state_init("viz_bins",10),
          step = 1)
      ),
      conditionalPanel("input.viz_type == 'density' |
                       (input.viz_type == 'scatter' & (input.viz_check && input.viz_check.indexOf('loess') >= 0))",
        sliderInput("viz_smooth", label = "Smooth:",
                    value = state_init("viz_smooth", 1),
                    min = 0.1, max = 3, step = .1)
      ),
      sliderInput("viz_alpha", label = "Opacity:", min = 0, max = 1,
        value = state_init("viz_alpha",.5), step = .01),
      tags$table(
        tags$td(numericInput("viz_plot_height", label = "Plot height:", min = 100,
                             max = 2000, step = 50,
                             value = state_init("viz_plot_height", r_data$plot_height),
                             width = "117px")),

        tags$td(numericInput("viz_plot_width", label = "Plot width:", min = 100,
                             max = 2000, step = 50,
                             value = state_init("viz_plot_width", r_data$plot_width),
                             width = "117px"))
      )
    ),
    help_and_report(modal_title = "Visualize",
                    fun_name = "visualize",
                    help_file = inclRmd(file.path(r_path,"base/tools/help/visualize.md")))
  )
})

viz_plot_width <- reactive({
  if (is_empty(input$viz_plot_width)) r_data$plot_width else input$viz_plot_width
})

viz_plot_height <- reactive({
  if (is_empty(input$viz_plot_height)) {
    r_data$plot_height
  } else {
    lx <- ifelse (not_available(input$viz_xvar) || isTRUE(input$viz_combx), 1, length(input$viz_xvar))
    ly <- ifelse (not_available(input$viz_yvar) || input$viz_type %in% c("hist","density") ||
                  isTRUE(input$viz_comby), 1, length(input$viz_yvar))
    nr <- lx * ly
    if (nr > 1)
      (input$viz_plot_height/2) * ceiling(nr / 2)
    else
      input$viz_plot_height
  }
})

output$visualize <- renderPlot({
  if (not_available(input$viz_xvar)) {
    return(
      plot(x = 1, type = 'n',
      main="\nPlease select variables from the dropdown menus to create a plot",
      axes = FALSE, xlab = "", ylab = "")
    )
  }

  .visualize() %>% { if (is.character(.)) {
      plot(x = 1, type = 'n', main = paste0("\n",.), axes = FALSE, xlab = "", ylab = "")
    } else if (is.null(.)) {
      return(invisible())
    } else {
      withProgress(message = 'Making plot', value = 0, print(.))
    }
  }
}, width = viz_plot_width, height = viz_plot_height)

# .visualize <- reactive({

#   req(input$viz_pause == FALSE, cancelOutput = TRUE)
#   .visualize2()
#   # if (is.null(input$viz_pause) || input$viz_pause == TRUE) {
#   #   isolate(.visualize2())
#   # } else {
#   #   .visualize2()
#   # }
# })

# .visualize2 <- reactive({
.visualize <- reactive({

  req(input$viz_pause == FALSE, cancelOutput = TRUE)
  req(input$viz_type)

  ## need dependency on ..
  req(input$viz_plot_height && input$viz_plot_width)

  if (not_available(input$viz_xvar)) return()
  if (input$viz_type %in% c("scatter","line", "box", "bar") && not_available(input$viz_yvar))
    return("No Y-variable provided for a plot that requires one")
  if (input$viz_type == "box" && !all(input$viz_xvar %in% groupable_vars())) return()

  ## waiting for comby and/or combx to be updated
  if (input$viz_type %in% c("hist", "density")) {
    if (isTRUE(input$viz_comby)) return()
    if (length(input$viz_xvar) > 1 && is.null(input$viz_combx)) return()
    # if (isTRUE(input$viz_combx)) {
    #   if (!is_empty(input$viz_color,"none")) return()
    #   if (!is_empty(input$viz_fill,"none")) return()
    # }
  } else {
    if (isTRUE(input$viz_combx)) return()
    if (length(input$viz_yvar) > 1 && is.null(input$viz_comby)) return()
    # if (isTRUE(input$viz_comby)) {
    #   if (!is_empty(input$viz_color,"none")) return()
    #   if (!is_empty(input$viz_fill,"none")) return()
    # }
  }

  # isolate({
  #   print("-----")
  #   print(length(input$viz_xvar))
  #   print(isTRUE(input$viz_combx))
  #   print(input$viz_combx)
  #   print("-----")
  # })

  ## still doesn't stop flickering!
  req(!is.null(input$viz_color) || !is.null(input$viz_fill))

  viz_inputs() %>% { .$shiny <- TRUE; . } %>% do.call(visualize, .)
})

observeEvent(input$visualize_report, {
  ## resetting hidden elements to default values
  vi <- viz_inputs()
  if (input$viz_type != "hist") vi$bins <- viz_args$bins
  if (!input$viz_type %in% c("density","scatter") ||
      !"loess" %in% input$viz_check) vi$smooth <- viz_args$smooth

  if (!input$viz_type %in% c("scatter", "box") &&
      "jitter" %in% input$viz_check) vi$check <- setdiff(vi$check, "jitter")

  if (isTRUE(input$viz_combx) && length(input$viz_xvar) < 2)
      vi$combx <- FALSE

  if (isTRUE(input$viz_comby) && length(input$viz_yvar) < 2)
      vi$comby <- FALSE

  inp_main <- clean_args(vi, viz_args)
  inp_main[["custom"]] <- FALSE
  update_report(inp_main = inp_main,
                fun_name = "visualize", outputs = character(0),
                pre_cmd = "", figs = TRUE,
                fig.width = round(7 * viz_plot_width()/600,2),
                fig.height = round(7 * viz_plot_height()/600,2))
})
