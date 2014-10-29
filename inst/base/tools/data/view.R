#############################################
# View table output of the selected dataset
#############################################
output$uiView_vars <- renderUI({
  vars <- varnames()
  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_init_multvar("view_vars",vars, vars), multiple = TRUE, selectize = FALSE)
})

output$uiView_subsbig <- renderUI({
#   view_select <- ""
#   if(!is.null(input$view_select) && input$view_select != "") {
#     view_select <- stringr::str_trim(input$view_select)
#   }

  list(
    tags$textarea(id="view_subsbig_area", rows=3, cols=5, "", state_init("view_subsbig_area",input$view_select)),
#     tags$textarea(id="view_subsbig_area", class="myclass", rows=3, cols=5, "", state_init("view_subsbig_area",view_select)),
    actionButton("view_subsbig_action", "Submit command")
  )
})

output$ui_View <- renderUI({
  list(
    wellPanel(
      uiOutput("uiView_vars"),
#       actionLink("view_alink", "Subset (e.g., price > 5000)"),
      checkboxInput('view_subsbig', 'Subset (price > 5000)', FALSE),
#       returnTextInput("view_select", "Subset (e.g., price > 5000)", state_init("view_select"))
      conditionalPanel(condition = "input.view_subsbig == false",
         returnTextInput("view_select", "", state_init("view_select"))
      ),
      conditionalPanel(condition = "input.view_subsbig == true",
        uiOutput("uiView_subsbig")
      )
    ),
    helpModal('View','viewHelp',inclMD("../base/tools/help/view.md"))
  )
})



# updating the dataset description
# dataDescriptionOutput
observe({
#   if(is.null(input$updateDescr) || input$updateDescr == 0) return()
#   isolate({
#     values[[paste0(input$datasets,"_descr")]] <- input$man_data_descr
#     updateCheckboxInput(session = session, "man_add_descr","Add/edit data description", FALSE)
#   })

  if(is.null(input$view_subsbig_action) || input$view_subsbig_action == 0) return()
  isolate({
    values[[paste0(input$datasets,"_filter")]] <- input$view_subsbig_action
#     updateCheckboxInput(session = session, "man_add_descr","Add/edit data description", FALSE)
  })
})

output$dataviewer <- renderDataTable({

  # if(isolate(input$datatabs) != 'View') return(invisible())
  # if(is.null(input$datasets) || is.null(input$view_vars)) return()

  if(is.null(input$view_vars)) return()
  # dat <- date2character()
  dat <- getdata()

  if(!all(input$view_vars %in% colnames(dat))) return()

#   input$view_select
#   input$view_subsbig
#
#   selcom <-
#   selcom <- input$view_select
# dataDescriptionOutput
#         selcom <- isolate(values[[paste0(input$datasets,"_filter")]])

    selcom <- input$view_select
    if(!(is.null(input$view_subsbig_action) || input$view_subsbig_action == 0)) {
#       selcom = ""
#     } else {
      isolate({
        if(input$view_subsbig_area != "") selcom <- input$view_subsbig_area
	 	    updateTextInput(session = session, "view_select", "","")
      })
    }
#   selcom = ""
#   if(input$view_select == "") {
#     if(is.null(input$view_subsbig_action) || input$view_subsbig_action == 0) {
#       selcom = ""
#     } else {
#       isolate({
#         if(input$view_subsbig_area != "") selcom <- input$view_subsbig_area
# #         selcom <- isolate(values[[paste0(input$datasets,"_filter")]])
#       })
#     }
#   } else {
#     selcom <- input$view_select
#   }

  if(selcom != "") {
    selcom <- gsub(" ", "", selcom)
#     seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
#     filter_(dat, "cyl == 8")

    seldat <- try(filter_(list(dat,parse(text = selcom))), silent = TRUE)
    if(!is(seldat, 'try-error')) {
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
      }
    }
  }

  as.data.frame(dat[, input$view_vars, drop = FALSE])

}, options = list(orderClasses = TRUE, caseInsensitive = TRUE,
  lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
  pageLength = 10, search = list(regex = TRUE)))

# }, options = list(bSortClasses = TRUE, bCaseInsensitive = TRUE,
#   lengthMenu = list(c(10, 25, 50, -1),c('10','25','50','All')),
#   pageLength = 10, search = list(regex = TRUE)))
