#############################################
# View table output of the selected dataset
#############################################
output$uiView_vars <- renderUI({
  vars <- varnames()
  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_init_multvar("view_vars",vars, vars), multiple = TRUE, selectize = FALSE)
})

output$uiView_subsbig <- renderUI({
  list(
    tags$textarea(id="view_subsbig_area", rows=3, cols=5, "", state_init("view_subsbig_area",isolate(input$view_select))),
    actionButton("view_subsbig_action", "Submit command")
  )
})

values$error = ""

output$uiView_err <- renderUI({
  if(values$error == "") return()
  helpText(values$error)
})


output$ui_View <- renderUI({
  list(
    wellPanel(
      uiOutput("uiView_vars"),
      checkboxInput('view_subsbig', 'Subset (price > 5000)', FALSE),
      conditionalPanel(condition = "input.view_subsbig == false",
         returnTextInput("view_select", "", state_init("view_select"))
      ),
      conditionalPanel(condition = "input.view_subsbig == true",
        uiOutput("uiView_subsbig")
      ),
      uiOutput("uiView_err")
    ),
    helpModal('View','viewHelp',inclMD("../base/tools/help/view.md"))
  )
})

observe({
#   if(is.null(input$view_subsbig_action) || input$view_subsbig_action == 0) return()
#   isolate({
#     values[[paste0(input$datasets,"_filter")]] <- input$view_subsbig_action
#      updateCheckboxInput(session = session, "man_add_descr","Add/edit data description", FALSE)
#   })
})

output$dataviewer <- renderDataTable({

  # if(isolate(input$datatabs) != 'View') return(invisible())
  # if(is.null(input$datasets) || is.null(input$view_vars)) return()
  if(is.null(input$view_vars)) return()
  # dat <- date2character()
  dat <- getdata()

  if(!all(input$view_vars %in% colnames(dat))) return()

  selcom <- gsub("\\s","",input$view_select)
  if(!(is.null(input$view_subsbig_action) || input$view_subsbig_action == 0)) {
    isolate({

      if(input$view_subsbig) {
        view_subsbig_area <- gsub("\\s", "", input$view_subsbig_area)
        if(view_subsbig_area != "") {
          selcom <- view_subsbig_area
 	        updateTextInput(session = session, "view_select", "", stringr::str_trim(input$view_subsbig_area))
        }
      } else {
        updateTextInput(session = session, "view_subsbig_area", "", input$view_select)
      }
    })
  } else {
    updateTextInput(session = session, "view_subsbig_area", "", input$view_select)
  }

  selcom <- gsub("\\s", "", selcom)
  if(selcom != "") {
    seldat <- try(filter_(dat, selcom), silent = TRUE)
    if(!is(seldat, 'try-error')) {
      values$error <- ""
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
      }
    } else {
      isolate(values$error <- attr(seldat,"condition")$message)
    }
  } else {
    isolate(values$error <- "")
  }

  as.data.frame(dat[, input$view_vars, drop = FALSE])

}, options = list(orderClasses = TRUE, caseInsensitive = TRUE,
  lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
  pageLength = 10, search = list(regex = TRUE)))
