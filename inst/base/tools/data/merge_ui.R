#######################################
# Merge/Join datasets
#######################################
# list of function arguments
merge_args <- as.list(formals(merge_data))

# list of function inputs selected by user
merge_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(merge_args))
    merge_args[[i]] <- input[[i]]
  if(!input$show_filter) merge_args$data_filter = ""
  merge_args
})

output$ui_merge_data <- renderUI({
  datasetlist <- r_data$datasetlist
  if(length(datasetlist) < 2) return()
  mdatasets <- datasetlist[-which(input$dataset == datasetlist)]
  selectInput(inputId = "merge_data", label = "Merge with:",
    choices = mdatasets, selected = state_init("merge_data"), multiple = FALSE)
})

output$ui_merge_vars <- renderUI({
  if(input$merge_data %>% is.null) return()
  vars1 <- varnames()
  vars2 <- colnames(r_data[[input$merge_data]])
  vars <- intersect(vars1, vars2)
  if(length(vars) == 0) return()
  vars <- vars1[vars1 %in% vars]  # need variable labels from varnames()
  selectInput("merge_vars", "Select merge-by variables:", choices  = vars,
    selected = state_multiple("merge_vars",vars),
    multiple = TRUE, size = length(vars), selectize = FALSE)
})

merge_type <- c("inner_join","left_join","semi_join","anti_join")

output$ui_merge_type <- renderUI({
  selectInput("merge_type", "Merge type:", choices  = merge_type,
    selected = state_single("merge_type",merge_type, "inner"), multiple = FALSE)
})

output$ui_Merge <- renderUI({
  list(
    wellPanel(
      uiOutput("ui_merge_data"),
      conditionalPanel(condition = "output.ui_merge_data == null",
        HTML("<label>Only one dataset available.</label>")
      ),
      uiOutput("ui_merge_vars"),
      conditionalPanel(condition = "output.ui_merge_vars != null",
        uiOutput("ui_merge_type"),
        textInput("merge_name", "Data name:", state_init("merge_name",paste0("merged_",input$dataset))),
        actionButton("merge_button", "Merge data")
      )
    ),
    help_and_report(modal_title = "Merge",
                    fun_name = "merge",
                    help_file = inclMD("../base/tools/help/merge.md"))
  )
})

observe({
  # merging data
  if(input$merge_button %>% not_pressed) return()
  isolate({
    do.call(merge_data, merge_inputs())
  })
})

observe({
  # if(is.null(input$mergeReport) || input$mergeReport == 0) return()
  if(input$merge_report %>% not_pressed) return()
  isolate({
    outputs <- c()
    # inp <- list(input$dataset, input$merge_data, input$merge_vars, input$merge_type, input$merge_name)
    update_report(inp = merge_inputs(), fun_name = "merge_data", outputs = outputs, pre_cmd = "")
  })
})

output$merge_possible <- renderText({
  if(input$merge_vars %>% is.null)
    "<h4>No matching variables selected</h4>"
  else
    ""
})

output$merge_data1 <- renderText({
  if(input$merge_data %>% is.null) return()
  show_data_snippet(title = paste("<h3>Data:",input$dataset,"</h3>"))
})

output$merge_data2 <- renderText({
  if(input$merge_data %>% is.null) return()
  show_data_snippet(input$merge_data, title = paste("<h3>Data:",input$merge_data,"</h3>"))
})
