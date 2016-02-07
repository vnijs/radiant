#######################################
# Combine datasets
#######################################
## list of function arguments
cmb_args <- as.list(formals(combinedata))

## list of function inputs selected by user
cmb_inputs <- reactive({

  cmb_args$dataset <- input$dataset
  cmb_args$cmb_dataset <- input$cmb_dataset
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in r_drop(names(cmb_args), drop = c("dataset","cmb_dataset")))
    cmb_args[[i]] <- input[[paste0("cmb_",i)]]

  ## only need cmb_by when using a join method
  if (!grepl("_join",cmb_args$type)) cmb_args$by <- ""
  cmb_args
})

output$ui_cmb_dataset <- renderUI({
  datasetlist <- r_data$datasetlist
  if (length(datasetlist) < 2) return()
  # req(length(datasetlist) > 1)
  cmb_datasets <- datasetlist[-which(input$dataset == datasetlist)]
  selectInput(inputId = "cmb_dataset", label = "Combine with:",
    choices = cmb_datasets, selected = state_init("cmb_dataset"), multiple = FALSE)
})

output$ui_cmb_by <- renderUI({
  # if (is.null(input$cmb_dataset)) return()
  req(input$cmb_dataset)
  vars1 <- varnames()
  vars2 <- colnames(r_data[[input$cmb_dataset]])
  vars <- intersect(vars1, vars2)
  if (length(vars) == 0) return()
  vars <- vars1[vars1 %in% vars]  ## need variable labels from varnames()
  selectInput("cmb_by", "Join by:", choices  = vars,
    selected = state_multiple("cmb_by",vars, vars),
    multiple = TRUE, size = min(5,length(vars)), selectize = FALSE)
})

output$ui_cmb_add <- renderUI({
  # if (is.null(input$cmb_dataset)) return()
  req(input$cmb_dataset)
  vars <- colnames(r_data[[input$cmb_dataset]])
  selectInput("cmb_add", "Variables to add:", choices  = vars,
    selected = state_multiple("cmb_add", vars, vars),
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
  tagList(
    wellPanel(
      uiOutput("ui_cmb_dataset"),
      conditionalPanel(condition = "output.ui_cmb_dataset == null",
        HTML("<label>Only one dataset available.</label>")
      ),
      uiOutput("ui_cmb_by"),
      uiOutput("ui_cmb_add"),
      selectInput("cmb_type", "Combine type:", choices  = cmb_type,
                  selected = state_single("cmb_type",cmb_type,"inner_join"),
                  multiple = FALSE),
      tags$table(
        tags$td(textInput("cmb_name", "Data name:", paste0(input$dataset,"_cmb"))),
        tags$td(actionButton("cmb_store", "Combine"), style="padding-top:30px;")
      )
    ),
    help_and_report(modal_title = "Combine",
                    fun_name = "combine",
                    help_file = inclMD(file.path(r_path, "base/tools/help/combine.md")))
  )
})

observeEvent(input$cmb_store, {
  ## combining datasets
  dataset <- input$dataset
  cmb_dataset <- input$cmb_dataset
  result <- try(do.call(combinedata, cmb_inputs()), silent = TRUE)
  if (is(result, 'try-error')) {
    r_data[[input$cmb_name]] <- attr(result,"condition")$message
    r_data[["datasetlist"]] %<>% setdiff(input$cmb_name)
  }
  updateSelectInput(session = session, inputId = "dataset", selected = dataset)
  updateSelectInput(session = session, inputId = "cmb_dataset", selected = cmb_dataset)
})

observeEvent(input$combine_report, {
  update_report(inp_main = clean_args(cmb_inputs(), cmb_args),
                fun_name = "combinedata", outputs = character(0),
                pre_cmd = "", figs = FALSE)
})

output$cmb_data1 <- renderText({
  req(input$dataset)
  show_data_snippet(title = paste("<h3>Dataset 1:",input$dataset,"</h3>"))
})

output$cmb_data2 <- renderText({
  req(input$cmb_dataset)
  show_data_snippet(input$cmb_dataset, title = paste("<h3>Dataset 2:",input$cmb_dataset,"</h3>"))
})

output$cmb_possible <- renderText({
  if (is_empty(input$cmb_by) && !is_empty(input$cmb_type) && grepl("_join",input$cmb_type))
    "<h3>No matching variables selected</h3>"
})

output$cmb_data <- renderText({
  input$cmb_store  ## dependence is needed to update cmb_type when result doesn't change
  name <- if (is_empty(input$cmb_name)) paste0("cmb_",isolate(input$dataset))
          else input$cmb_name
  if (!is.null(r_data[[name]])) {
    if (is.character(r_data[[name]])) {
      HTML(paste0("</br><h4>The combine type selected is not appropriate for these datasets.</br>The error message was:</br></br>\"", r_data[[name]],"\"</h4>"))
    } else {
      show_data_snippet(name, nshow = 15, title = paste0("<h3>Combined dataset: ",
        name," [<font color='blue'>", isolate(input$cmb_type),"</font>]</h3>"))
    }
  }
})


