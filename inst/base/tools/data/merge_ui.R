#######################################
# Merge/Join datasets
#######################################
# list of function arguments
merge_args <- as.list(formals(mergedata))

# list of function inputs selected by user
merge_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for(i in names(merge_args))
    merge_args[[i]] <- input[[i]]
  # if(!input$show_filter) merge_args$data_filter = ""
  merge_args
})

output$ui_dataset2 <- renderUI({
  datasetlist <- r_data$datasetlist
  if(length(datasetlist) < 2) return()
  mdatasets <- datasetlist[-which(input$dataset == datasetlist)]
  selectInput(inputId = "dataset2", label = "Merge with:",
    choices = mdatasets, selected = state_init("dataset2"), multiple = FALSE)
})

output$ui_merge_vars <- renderUI({
  if(input$dataset2 %>% is.null) return()
  vars1 <- varnames()
  vars2 <- colnames(r_data[[input$dataset2]])
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
      uiOutput("ui_dataset2"),
      conditionalPanel(condition = "output.ui_dataset2 == null",
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
  if(not_pressed(input$merge_button)) return()
  isolate({
    do.call(mergedata, merge_inputs())
  })
})

observe({
  if(not_pressed(input$merge_report)) return()
  isolate({
    update_report(inp_main = clean_args(merge_inputs(), merge_args),
                  fun_name = "mergedata", outputs = character(0), figs = FALSE)
  })
})

output$merge_possible <- renderText({
  if(is.null(input$merge_vars))
    "<h4>No matching variables selected</h4>"
  else
    ""
})

output$mergedata1 <- renderText({
  if(is.null(input$dataset2)) return()
  show_data_snippet(title = paste("<h3>Data:",input$dataset,"</h3>"))
})

output$mergedata2 <- renderText({
  if(is.null(input$dataset2)) return()
  show_data_snippet(input$dataset2, title = paste("<h3>Data:",input$dataset2,"</h3>"))
})
