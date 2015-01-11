#######################################
# Merge/Join datasets 
#######################################
output$uiMergeDataset <- renderUI({
  datasetlist <- values$datasetlist
  if(length(datasetlist) < 2) return()
  mdatasets <- datasetlist[-which(input$datasets == datasetlist)]
  selectInput(inputId = "mergeDataset", label = "Merge with:", 
    choices = mdatasets, selected = state_init("mergeDataset"), multiple = FALSE)
})

output$uiMerge_vars <- renderUI({

  if(is.null(input$mergeDataset)) return()
  vars1 <- varnames()
  vars2 <- colnames(values[[input$mergeDataset]])
  vars <- intersect(vars1, vars2)
  if(length(vars) == 0) return()
  vars <- vars1[vars %in% vars1]  # need variable labels from varnames()
  selectInput("merge_vars", "Select merge-by variables:", choices  = vars, 
    selected = state_init_multvar("merge_vars",vars, vars), multiple = TRUE, selectize = FALSE)
})

merge_type <- c('left','right','inner','full')

output$uiMerge_type <- renderUI({
  selectInput("merge_type", "Merge type:", choices  = merge_type, 
    selected = state_init_list("merge_type","left", merge_type), multiple = FALSE)
})

merge_match <- c('all','first')

output$uiMerge_match <- renderUI({
  radioButtons("merge_match", label = "Match:", merge_match, 
   selected = state_init_list("merge_match","all", merge_match))
})

output$ui_Merge <- renderUI({
  list(
    wellPanel(
      uiOutput("uiMergeDataset"),
      conditionalPanel(condition = "output.uiMergeDataset == null",
        HTML("<label>Only one dataset available.</label>")
      ),
      # conditionalPanel(condition = "output.uiMergeDataset != null",
      uiOutput("uiMerge_vars"),
      conditionalPanel(condition = "output.uiMerge_vars != null",
        uiOutput("uiMerge_type"),
        uiOutput("uiMerge_match"),
        textInput("merge_name", "Data name:", state_init("merge_name",paste0("merged_",input$datasets))),
        actionButton('mergeData', 'Merge data')
      )
    ),
    helpAndReport('Merge','merge',inclMD("tools/help/merge.md"))
  )
})

observe({
  # merging data
  if(is.null(input$mergeData) || input$mergeData == 0) return()
  isolate({
    mergeData(input$datasets, input$mergeDataset, input$merge_vars, input$merge_type, input$merge_match, 
      input$merge_name)
  })
})

mergeData <- function(datasets, mergeDataset, merge_vars, merge_type, merge_match, merge_name) {
  mdata <- join(values[[datasets]], values[[mergeDataset]], by = merge_vars, 
    type = merge_type, match = merge_match)

  values[[merge_name]] <- mdata
  values[['datasetlist']] <- unique(c(merge_name,values[['datasetlist']]))
}

observe({
  if(is.null(input$mergeReport) || input$mergeReport == 0) return()
  isolate({
    inp <- list(input$datasets, input$mergeDataset, input$merge_vars, input$merge_type, input$merge_match, 
      input$merge_name)
    updateReportMerge(inp,"mergeData")
  })
})

output$mergePossible <- renderText({

  if(is.null(input$merge_vars)) 
    return("<h4>No matching variables found</h4><br>")
  if(sum(input$merge_vars %in% colnames(values[[input$mergeDataset]])) == 0)
    return("<h4>No matching variables found</h4><br>")

  return("")
})

output$mergeData1 <- renderText({
  # if(isolate(input$datatabs) != 'Merge') return(invisible())
  if(is.null(input$mergeDataset)) return()
  dat <- getdata()

  # Show only the first 5 rows
  nr <- min(5,nrow(dat))
  dat <- data.frame(dat[1:nr,, drop = FALSE])
  dat <- date2character_dat(dat) # dealing with dates
  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- paste0("<h4>Data: ",input$datasets,"</h4>\n",html, "\n<br>") 
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html
})

output$mergeData2 <- renderText({
  # if(isolate(input$nav_radiant) != 'Merge') return(invisible())
  if(is.null(input$mergeDataset)) return()
  dat <- values[[input$mergeDataset]]

  # Show only the first 5 rows
  nr <- min(5,nrow(dat))
  dat <- data.frame(dat[1:nr,, drop = FALSE])
  dat <- date2character_dat(dat) # dealing with dates
  html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- paste0("<h4>Data: ",input$mergeDataset,"</h4>\n",html) 
  html <- sub("<TABLE border=1>","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html
})
