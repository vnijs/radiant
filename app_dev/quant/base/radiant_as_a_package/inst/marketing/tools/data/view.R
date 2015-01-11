#############################################
# View table output of the selected dataset
#############################################
output$uiView_vars <- renderUI({
  vars <- varnames()
  selectInput("view_vars", "Select variables to show:", choices  = vars, 
    selected = state_init_multvar("view_vars",vars, vars), multiple = TRUE, selectize = FALSE)
})

output$ui_View <- renderUI({
  list(
    wellPanel(
      uiOutput("uiView_vars"), 
      returnTextInput("view_select", "Subset (e.g., price > 5000)", state_init("view_select"))
    ),
    helpModal('View','viewHelp',inclMD("tools/help/view.md"))
  )
})

output$dataviewer <- renderDataTable({

  # if(isolate(input$datatabs) != 'View') return(invisible())
  # if(is.null(input$datasets) || is.null(input$view_vars)) return()
  
  if(is.null(input$view_vars)) return()
  # dat <- date2character()
  dat <- getdata()

  if(!all(input$view_vars %in% colnames(dat))) return()
  if(input$view_select != "") {
    selcom <- input$view_select
    selcom <- gsub(" ", "", selcom)

    seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)

    if(!is(seldat, 'try-error')) {
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
      }
    }
  }

  as.data.frame(dat[, input$view_vars, drop = FALSE])

}, options = list(bSortClasses = TRUE, bCaseInsensitive = TRUE, 
  aLengthMenu = c(10, 20, 30, 50), iDisplayLength = 10))
