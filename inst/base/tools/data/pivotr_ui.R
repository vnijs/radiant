############################################
## Pivotr - combination of Explore and View
############################################

pvt_normalize <- c("None" = "None", "Row" = "row", "Column" = "column",
                   "Total" = "total")
pvt_format <- c("None" = "none", "Color bar" = "color_bar", "Heat map" = "heat")
pvt_type <- c("Dodge" = "dodge","Fill" = "fill")

## UI-elements for pivotr
output$ui_pvt_cvars <- renderUI({
  vars <- groupable_vars()
  if (not_available(vars)) return()

  isolate({
    if (available(r_state$pvt_cvars) && all(r_state$pvt_cvars %in% vars))
      vars <- unique(c(r_state$pvt_cvars, vars))
  })

  selectizeInput("pvt_cvars", label = "Categorical variables:", choices = vars,
    selected = state_multiple("pvt_cvars",vars, ""),
    multiple = TRUE,
    options = list(placeholder = 'Select categorical variables',
                   plugins = list('remove_button', 'drag_drop'))
  )
})

output$ui_pvt_nvar <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- c("None", varnames()[isNum])

  if (any(vars %in% input$pvt_cvars)) {
    vars <- setdiff(vars, input$pvt_cvars)
  }

  isolate({
    ## keep the same n-variable 'active' if possible
    sel <- use_input("pvt_nvar", vars)
  })

  selectizeInput("pvt_nvar", label = "Numeric variable:", choices = vars,
    # selected = state_single("pvt_nvar",vars), multiple = FALSE,
    selected = sel, multiple = FALSE,
    options = list(placeholder = 'Select numeric variable'))
})

output$ui_pvt_fun <- renderUI({
  selectizeInput("pvt_fun", label = "Apply function:",
                 choices = r_functions,
                 selected = state_single("pvt_fun", r_functions, "mean_rm"),
                 multiple = FALSE)
})

output$ui_pvt_normalize  <- renderUI({
  if (!is.null(input$pvt_cvars) && length(input$pvt_cvars) == 1) pvt_normalize <- pvt_normalize[-(2:3)]

  isolate({
    sel <- if (is_empty(input$pvt_normalize)) state_single("pvt_normalize", pvt_normalize, "None") else input$pvt_normalize
  })

  selectizeInput("pvt_normalize", label = "Normalize by:",
    choices = pvt_normalize,
    selected = sel,
    multiple = FALSE)
})

output$ui_pvt_format  <- renderUI({
  selectizeInput("pvt_format", label = "Conditional formatting:",
    choices = pvt_format,
    selected = state_single("pvt_format", pvt_format, "none"),
    multiple = FALSE)
})

output$ui_Pivotr <- renderUI({
  tagList(
    wellPanel(
      uiOutput("ui_pvt_cvars"),
      uiOutput("ui_pvt_nvar"),
      conditionalPanel("input.pvt_nvar != 'None'", uiOutput("ui_pvt_fun")),
      uiOutput("ui_pvt_normalize"),
      uiOutput("ui_pvt_format"),
      with(tags, table(
        tr(
          td(checkboxInput("pvt_tab", "Show table", value = state_init("pvt_tab", TRUE))),
          td(checkboxInput("pvt_plot", "Show plot", value = state_init("pvt_plot", FALSE)))
        ),
        tr(
          td(checkboxInput("pvt_perc", "Percentage", value = state_init("pvt_perc", FALSE))),
          td(conditionalPanel("input.pvt_nvar == 'None' && input.pvt_normalize == 'None'",
               checkboxInput("pvt_chi2", "Chi-square", value = state_init("pvt_chi2", FALSE))))
      )))
    ),
    conditionalPanel("input.pvt_plot == true",
      wellPanel(
        radioButtons("pvt_type", label = "Plot type:",
          pvt_type,
          selected = state_init("pvt_type", "dodge"),
          inline = TRUE),
        checkboxInput("pvt_flip", "Flip", value = state_init("pvt_flip", FALSE))
      )
    ),
    help_and_report(modal_title = "Pivotr",
                    fun_name = "pivotr",
                    help_file = inclMD(file.path(r_path,"base/tools/help/pivotr.md")))
  )
})

pvt_args <- as.list(formals(pivotr))

observe({
  ## only allow chi2 if frequencies are shown
  if (is_empty(input$pvt_normalize, "None") && is_empty(input$pvt_nvar, "None")) return()
  isolate({
    if (input$pvt_chi2) updateCheckboxInput(session, "pvt_chi2", value = FALSE)
  })
})

## list of function inputs selected by user
pvt_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  pvt_args$data_filter <- if (input$show_filter) input$data_filter else ""
  pvt_args$dataset <- input$dataset
  for (i in r_drop(names(pvt_args)))
    pvt_args[[i]] <- input[[paste0("pvt_",i)]]

  pvt_args
})

pvt_plot_args <- as.list(if (exists("plot.pivotr")) formals(plot.pivotr)
                         else formals(radiant:::plot.pivotr))

## list of function inputs selected by user
pvt_plot_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(pvt_plot_args))
    pvt_plot_args[[i]] <- input[[paste0("pvt_",i)]]
  pvt_plot_args
})

.pivotr <- reactive({
  if (not_available(input$pvt_cvars)) return()
  if (is_empty(input$pvt_fun)) {
    updateSelectInput(session, "pvt_fun", selected = "length")
    return()
  }
  if (is_empty(input$pvt_nvar)) {
    updateSelectInput(session, "pvt_nvar", selected = "None")
    return()
  }

  if (any(input$pvt_nvar %in% input$pvt_cvars)) return()

  withProgress(message = "Calculating", value = 0, {
    sshhr( do.call(pivotr, pvt_inputs()) )
  })
})

observeEvent(input$pivotr_search_columns, {
  isolate({
    r_state$pivotr_search_columns <<- input$pivotr_search_columns
  })
})

observeEvent(input$pivotr_state, {
  isolate({
    r_state$pivotr_state <<-
      if (is.null(input$pivotr_state)) list() else input$pivotr_state
  })
})

output$pivotr <- DT::renderDataTable({
  pvt <- .pivotr()
  if (is.null(pvt)) return()
  pvt$shiny <- TRUE

  if (!identical(r_state$pvt_cvars, input$pvt_cvars)) {
    r_state$pvt_cvars <<- input$pvt_cvars
    r_state$pivotr_state <<- list()
    r_state$pivotr_search_columns <<- rep("", ncol(pvt$tab))
  }

  search <- r_state$pivotr_state$search$search
  if (is.null(search)) search <- ""
  searchCols <- lapply(r_state$pivotr_search_columns, function(x) list(search = x))
  order <- r_state$pivotr_state$order

  make_dt(pvt, format = input$pvt_format, perc = input$pvt_perc,
          search = search, searchCols = searchCols, order = order)

})

output$pivotr_chi2 <- renderPrint({
  if (!input$pvt_chi2) return(invisible())
  .pivotr() %>% {if (is.null(.)) return(invisible())
                 else summary(., chi2 = TRUE, shiny = TRUE)}
})

output$dl_pivot_tab <- downloadHandler(
  filename = function() { paste0("pivot_tab.csv") },
  content = function(file) {
    dat <- .pivotr()
    if (is.null(dat)) {
      write.csv(data_frame("Data" = "[Empty]"),file, row.names = FALSE)
    } else {
      rows <- isolate(r_data$pvt_rows)
      dat$tab %>% {if (is.null(rows)) . else slice(., c(rows,nrow(.)))} %>%
        write.csv(file, row.names = FALSE)
    }
  }
)

pvt_plot_width <- function() 750
pvt_plot_height <- function() {
   pvt <- .pivotr()
   if (is.null(pvt)) return(400)
   pvt %<>% pvt_sorter(rows = r_data$pvt_rows)
   if (length(input$pvt_cvars) > 2) {
       pvt$tab %>% .[[input$pvt_cvars[3]]] %>%
         levels %>%
         length %>% {. * 200}
   } else if (input$pvt_flip) {
      if (length(input$pvt_cvars) == 2)
        max(400, ncol(pvt$tab) * 15)
      else
        max(400, nrow(pvt$tab) * 15)
   } else {
      400
   }
}

pvt_sorter <- function(pvt, rows = NULL) {
  if (is.null(rows)) return(pvt)
  cvars <- pvt$cvars
  tab <- pvt$tab %>% {filter(., .[[1]] != "Total")}

  if (length(cvars) > 1)
    tab %<>% select(-which(colnames(.) == "Total"))

  tab %<>% slice(rows)
  cvars <- if (length(cvars) == 1) cvars else cvars[-1]

  for (i in cvars)
    tab[[i]] %<>% factor(., levels = unique(.))

  pvt$tab <- tab
  pvt
}

observeEvent(input$pivotr_rows_all, {
  isolate({
    dt_rows <- input$pivotr_rows_all
    if (identical(r_data$pvt_rows, dt_rows)) return()
    r_data$pvt_rows <- dt_rows
  })
})

.plot_pivot <- reactive({
  pvt <- .pivotr()

  if (is.null(pvt)) return(invisible())
  if (!is_empty(input$pvt_tab, FALSE))
    pvt <- pvt_sorter(pvt, rows = r_data$pvt_rows)
    pvt_plot_inputs() %>% { .$shiny <- TRUE; . } %>%
      { do.call(plot, c(list(x = pvt), .)) }
})

output$plot_pivot <- renderPlot({
  if (is_empty(input$pvt_plot, FALSE)) return(invisible())
  withProgress(message = 'Making plot', value = 0, {
    sshhr(.plot_pivot()) %>% print
  })
  return(invisible())
}, width = pvt_plot_width, height = pvt_plot_height)

observeEvent(input$pivotr_report, {
  isolate({

    # print("====")
    # print(input$pivotr_search_columns)
    # print(sc %>% {set_names(.,colnames(pvt$tab))})

    inp_out <- list(list(chi2 = input$pvt_chi2),"")
    if (input$pvt_plot == TRUE) {
      inp_out[[2]] <- clean_args(pvt_plot_inputs(), pvt_plot_args[-1])
      outputs <- c("summary","plot")
      figs <- TRUE
    } else {
      outputs <- c("summary")
      figs <- FALSE
    }
    update_report(inp_main = c(clean_args(pvt_inputs(), pvt_args), tabsort = "", tabfilt = ""),
                  fun_name = "pivotr",
                  outputs = outputs,
                  inp_out = inp_out,
                  figs = figs,
                  fig.width = round(7 * pvt_plot_width()/650,2),
                  fig.height = round(7 * pvt_plot_height()/650,2))
  })
})
