#############################################
# View table output of the selected dataset
#############################################
output$uiView_vars <- renderUI({
  vars <- varnames()
  selectInput("view_vars", "Select variables to show:", choices  = vars,
    selected = state_init_multvar("view_vars",vars, vars),
    multiple = TRUE, selectize = FALSE)
})

output$uiView_subsbig <- renderUI({
  list(
    tags$textarea(id="view_subsbig_area", rows=3, "",
                  state_init("view_subsbig_area",isolate(input$view_select))),
    actionButton("view_subsbig_action", "Submit command")
  )
})

output$uiView_err <- renderUI({
  if(is.null(values$error) || values$error == "") return()
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
#     values[[paste0(input$dataset,"_filter")]] <- input$view_subsbig_action
#      updateCheckboxInput(session = session, "man_add_descr","Add/edit data description", FALSE)
#   })
})

output$dataviewer <- renderDataTable({

  # if(is.null(input$view_vars)) return()
  # if(!all(input$view_vars %in% colnames(dat))) return()
  if(input$view_vars %>% not_available) return()

  dat <- getdata()

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

# This works:
#   filter_(mtcars, "mpg > 24")
# and this:
#   filter_(mtcars, "mpg > 24", "disp > 75")
# Is there a way to make this work as well?
#   filter_(mtcars, "mpg > 24, disp > 75")
# filter_(mtcars,unlist(strsplit("mpg > 24, disp > 75", ",")))
# filter_(mtcars,unlist(strsplit("mpg > 24 | disp > 75", ",")))
# filter_(mtcars,"mpg > 24 | disp > 75")
# filter_(mtcars,c("mpg > 24", "disp > 75"))
# filter_(mtcars,"mpg > 24", "disp > 75")
# filter_(mtcars,c("mpg > 24", "disp > 1000075"))
#
#
#       mpg cyl  disp  hp drat   wt qsec vs am gear carb
#       1 24.4   4 146.7  62 3.69 3.19 20.0  1  0    4    2
#       2 32.4   4  78.7  66 4.08 2.20 19.5  1  1    4    1
#       3 30.4   4  75.7  52 4.93 1.61 18.5  1  1    4    2
#       4 33.9   4  71.1  65 4.22 1.83 19.9  1  1    4    1
#       5 27.3   4  79.0  66 4.08 1.94 18.9  1  1    4    1
#       6 26.0   4 120.3  91 4.43 2.14 16.7  0  1    5    2
#       7 30.4   4  95.1 113 3.77 1.51 16.9  1  1    5    2

# This works as an alternative
#   filter_(mtcars, "mpg > 24 & disp > 75")
# the do.call option works but for something like:
# (price > 4000 , carat > 1) |  clarity == "VS1"
# you run into trouble again
#      seldat <- try(do.call(filter_, c(list(dat),unlist(strsplit(selcom, ",")))))
#     seldat <- try(filter_(mtcars,unlist(strsplit("mpg > 24, disp > 75", ","))), silent = TRUE)
#     seldat <- try(filter_(dat,unlist(strsplit(selcom,","))), silent = TRUE)

  selcom <- gsub("\\s", "", selcom)
  if(selcom != "") {
     seldat <- try(filter_(dat, selcom), silent = TRUE)

    if(!is(seldat, 'try-error')) {
      isolate(values$error <- "")
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

