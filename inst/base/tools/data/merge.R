#######################################
# Merge/Join datasets
#######################################
output$uiMergeDataset <- renderUI({
  datasetlist <- r_data$datasetlist
  if(length(datasetlist) < 2) return()
  mdatasets <- datasetlist[-which(input$dataset == datasetlist)]
  selectInput(inputId = "mergeDataset", label = "Merge with:",
    choices = mdatasets, selected = state_init("mergeDataset"), multiple = FALSE)
})

output$uiMerge_vars <- renderUI({

  if(is.null(input$mergeDataset)) return()
  vars1 <- varnames()
  vars2 <- colnames(r_data[[input$mergeDataset]])
  vars <- intersect(vars1, vars2)
  if(length(vars) == 0) return()
  vars <- vars1[vars1 %in% vars]  # need variable labels from varnames()
  selectInput("merge_vars", "Select merge-by variables:", choices  = vars,
    selected = state_multiple("merge_vars",vars),
    multiple = TRUE, size = length(vars), selectize = FALSE)
})

merge_type <- c('inner_join','left_join','semi_join','anti_join')

output$uiMerge_type <- renderUI({
  selectInput("merge_type", "Merge type:", choices  = merge_type,
    selected = state_single("merge_type",merge_type, "inner"), multiple = FALSE)
})

output$ui_Merge <- renderUI({
  list(
    wellPanel(
      uiOutput("uiMergeDataset"),
      conditionalPanel(condition = "output.uiMergeDataset == null",
        HTML("<label>Only one dataset available.</label>")
      ),
      uiOutput("uiMerge_vars"),
      conditionalPanel(condition = "output.uiMerge_vars != null",
        uiOutput("uiMerge_type"),
        textInput("merge_name", "Data name:", state_init("merge_name",paste0("merged_",input$dataset))),
        actionButton('mergeData', 'Merge data')
      )
    ),
    helpAndReport('Merge','merge',inclMD("../base/tools/help/merge.md"))
  )
})

observe({
  # merging data
  if(is.null(input$mergeData) || input$mergeData == 0) return()
  isolate({
    mergeData(input$dataset, input$mergeDataset, input$merge_vars, input$merge_type,
      input$merge_name)
  })
})

mergeData <- function(dataset, mergeDataset, merge_vars, merge_type, merge_name) {

  # gettin the join-type from the string
  tmpjoin <- get(merge_type)
  r_data[[merge_name]] <- tmpjoin(r_data[[dataset]], r_data[[mergeDataset]], by = merge_vars)
  r_data[['datasetlist']] <- unique(c(merge_name,r_data[['datasetlist']]))
}

observe({
  if(is.null(input$mergeReport) || input$mergeReport == 0) return()
  isolate({
    inp <- list(input$dataset, input$mergeDataset, input$merge_vars, input$merge_type, input$merge_name)
    updateReportMerge(inp,"mergeData")
  })
})

output$mergePossible <- renderText({
  if(is.null(input$merge_vars))
    return("<h4>No matching variables selected</h4>")
  return("")
})


output$mergeData1 <- renderText({
  if(is.null(input$mergeDataset)) return()
  show_data_snippet(title = paste("<h3>Data:",input$dataset,"</h3>"))
})

output$mergeData2 <- renderText({
  if(is.null(input$mergeDataset)) return()
  show_data_snippet(input$mergeDataset, title = paste("<h3>Data:",input$mergeDataset,"</h3>"))
})
