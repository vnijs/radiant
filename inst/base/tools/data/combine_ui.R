#######################################
# Combine datasets
#######################################
# list of function arguments
cmb_args <- as.list(formals(combinedata))

# list of function inputs selected by user
cmb_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(cmb_args))
    cmb_args[[i]] <- input[[i]]

  # only need cmb_vars when using a join method
  if (!grepl("_join",cmb_args$cmb_type)) cmb_args$cmb_vars <- ""
  cmb_args
})

output$ui_dataset2 <- renderUI({
  datasetlist <- r_data$datasetlist
  if (length(datasetlist) < 2) return()
  cmb_datasets <- datasetlist[-which(input$dataset == datasetlist)]
  selectInput(inputId = "dataset2", label = "Combine with:",
    choices = cmb_datasets, selected = state_init("dataset2"), multiple = FALSE)
})

output$ui_cmb_vars <- renderUI({
  if (input$dataset2 %>% is.null) return()
  vars1 <- varnames()
  vars2 <- colnames(r_data[[input$dataset2]])
  vars <- intersect(vars1, vars2)
  if (length(vars) == 0) return()
  vars <- vars1[vars1 %in% vars]  # need variable labels from varnames()
  selectInput("cmb_vars", "Select variables:", choices  = vars,
    selected = state_multiple("cmb_vars",vars, vars),
    multiple = TRUE, size = min(5,length(vars)), selectize = FALSE)
})

cmb_type <- c("inner_join","left_join","right_join","full_join","semi_join",
              "anti_join","bind_rows","bind_cols","intersect","union","setdiff")

# post nested renderui issue to github
# isolate(input$cmb_type) return an empty string
# output$ui_cmb_type <- renderUI({
#   selectInput("cmb_type", "Combine type:", choices  = cmb_type,
#     selected = state_single("cmb_type",cmb_type, "inner_join"),
#     multiple = FALSE)
# })

output$ui_Combine <- renderUI({
  list(
    wellPanel(
      uiOutput("ui_dataset2"),
      conditionalPanel(condition = "output.ui_dataset2 == null",
        HTML("<label>Only one dataset available.</label>")
      ),
      uiOutput("ui_cmb_vars"),
      selectInput("cmb_type", "Combine type:", choices  = cmb_type,
                  selected = state_single("cmb_type",cmb_type, "inner_join"),
                  multiple = FALSE),
      textInput("cmb_name", "Data name:",
                state_init("cmb_name",paste0("cmb_",input$dataset))),
      actionButton("cmb_button", "Combine")
    ),
    help_and_report(modal_title = "Combine",
                    fun_name = "combine",
                    help_file = inclMD(file.path(r_path, "base/tools/help/combine.md")))
  )
})

observe({
  # merging data
  if (not_pressed(input$cmb_button)) return()
  isolate({
    dataset1 <- input$dataset
    dataset2 <- input$dataset2
    cmb_result <- try(do.call(combinedata, cmb_inputs()), silent = TRUE)
    if (is(cmb_result, 'try-error')) {
      r_data[[input$cmb_name]] <- attr(cmb_result,"condition")$message
      r_data[["datasetlist"]] %<>% setdiff(input$cmb_name)
    }
    updateSelectInput(session = session, inputId = "dataset", selected = dataset1)
    updateSelectInput(session = session, inputId = "dataset2", selected = dataset2)
  })
})

observe({
  if (not_pressed(input$combine_report)) return()
  isolate({
    update_report(inp_main = clean_args(cmb_inputs(), cmb_args),
                  fun_name = "combinedata", outputs = character(0), pre_cmd = "",
                  figs = FALSE)
  })
})

output$cmb_data1 <- renderText({
  if (is.null(input$dataset2)) return()
  show_data_snippet(title = paste("<h3>Dataset 1:",input$dataset,"</h3>"))
})

output$cmb_data2 <- renderText({
  if (is.null(input$dataset2)) return()
  show_data_snippet(input$dataset2, title = paste("<h3>Dataset 2:",input$dataset2,"</h3>"))
})

output$cmb_possible <- renderText({
  if (is_empty(input$cmb_vars) && !is_empty(input$cmb_type) && grepl("_join",input$cmb_type))
    "<h3>No matching variables selected</h3>"
})

output$cmb_data <- renderText({
  input$cmb_button  # this dependence is needed to update cmb_type when the result is the same
  cmb_name <- if (is_empty(input$cmb_name)) paste0("cmb_",isolate(input$dataset))
              else input$cmb_name
  if (!is.null(r_data[[cmb_name]])) {
    if (is.character(r_data[[cmb_name]]))
      HTML(paste0("</br><h4>The combine type selected is not appropriate for these datasets.</br>The error message was:</br></br>\"", r_data[[cmb_name]],"\"</h4>"))
    else
      show_data_snippet(cmb_name, nshow = 15, title = paste0("<h3>Combined dataset: ",
        cmb_name," [<font color='blue'>", isolate(input$cmb_type),"</font>]</h3>"))
  }
})


