############################################
## Pivotr - combination of Explore and View
############################################

## UI-elements for pivotr
output$ui_pvt_cvars <- renderUI({
  vars <- groupable_vars()
  selectizeInput("pvt_cvars", label = "Categorical variables:", choices = vars,
    selected = state_multiple("pvt_cvars",vars, ""), multiple = TRUE,
    options = list(placeholder = 'Select categorical variables',
                   plugins = list('remove_button', 'drag_drop'))
  )
})

output$ui_pvt_nvar <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- c("None", varnames()[isNum])
  # vars <- varnames()[isNum]
  selectizeInput("pvt_nvar", label = "Select variable:", choices = vars,
    selected = state_single("pvt_nvar",vars), multiple = FALSE,
    options = list(placeholder = 'Select numeric variable'))
})

output$ui_pvt_fun <- renderUI({
  if (is_empty(input$pvt_nvar, empty = "None")) return()
  selectizeInput("pvt_fun", label = "Apply function:",
                 choices = expl_functions,
                 selected = state_single("pvt_fun", expl_functions, "mean_rm"),
                 multiple = FALSE)
})

pvt_normalize <- c("None" = "None", "Row" = "row", "Column" = "column",
                   "Total" = "total")

output$ui_pvt_normalize  <- renderUI({
  if(is.null(input$pvt_cvars)) return()
  if(length(input$pvt_cvars) == 1) pvt_normalize <- pvt_normalize[-2]

  sel <- if(is_empty(input$pvt_normalize)) state_single("pvt_normalize", pvt_normalize, "None") else input$pvt_normalize
  selectizeInput("pvt_normalize", label = "Normalize:",
    choices = pvt_normalize,
    # selected = state_single("pvt_normalize", pvt_normalize, "None"),
    selected = sel,
    multiple = FALSE)
})

output$ui_Pivotr <- renderUI({
  list(
    wellPanel(
      uiOutput("ui_pvt_cvars"),
      uiOutput("ui_pvt_nvar"),
      uiOutput("ui_pvt_fun"),
      uiOutput("ui_pvt_normalize"),
      checkboxInput(inputId = "pvt_color_bar", label = "Color bar",
        value = state_init("pvt_color_bar",FALSE))
    ),
    help_and_report(modal_title = "Pivotr",
                    fun_name = "pivotr",
                    help_file = inclMD(file.path(r_path,"base/tools/help/pivotr.md")))
  )
})

pvt_args <- as.list(formals(pivotr))

## list of function inputs selected by user
pvt_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  pvt_args$data_filter <- if (input$show_filter) input$data_filter else ""
  pvt_args$dataset <- input$dataset
  for (i in r_drop(names(pvt_args)))
    pvt_args[[i]] <- input[[paste0("pvt_",i)]]

  pvt_args
})

.pivotr <- reactive({
  if(not_available(input$pvt_cvars)) return()
  withProgress(message = 'Calculating', value = 0, {
    sshhr( do.call(pivotr, pvt_inputs()) )
  })
})

output$pivotr <- DT::renderDataTable({
  pvt <- .pivotr()
  if(is.null(pvt)) return()
  color_bar <- input$pvt_color_bar == TRUE

  perc <- FALSE
  if(!is.null(input$pvt_normalize) && input$pvt_normalize != "None" &&
     input$pvt_nvar == "None") perc <- TRUE

  pvt$shiny <- TRUE
  make_dt(pvt, color_bar = color_bar, perc = perc)
})

observe({
  if (not_pressed(input$pivotr_report)) return()
  isolate({
    update_report(inp_main = clean_args(pvt_inputs(), pvt_args),
                  fun_name = "pivotr", outputs = "summary",
                  figs = FALSE)
  })
})

