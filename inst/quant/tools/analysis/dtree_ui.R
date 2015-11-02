#######################################
## Create decision tree
#######################################

dtree_example <-
"name: Jenny Lind
type: decision
Sign with Movie Company:
    type: chance
    Small Box Office:
        p: 0.3
        payoff: 200000
    Medium Box Office:
        p: 0.6
        payoff: 1000000
    Large Box Office:
        p: 0.1
        payoff: 3000000
Sign with TV Network:
    type: chance
    Small Box Office:
        p: 0.3
        payoff: 900000
    Medium Box Office:
        p: 0.6
        payoff: 900000
    Large Box Office:
        p: 0.1
        payoff: 900000
"

observeEvent(input$dtree_vim_keys, {
  isolate({
     if (!is_empty(input$dtree_edit))
       r_state$dtree_edit <<- input$dtree_edit

    r_data$vim_keys %<>% {. == FALSE}
  })
})

output$ui_dtree_vim <- renderUI({
  ## initialize vim_keys to false
  if (is.null(r_data$vim_keys)) r_data$vim_keys <- FALSE
  actionButton("dtree_vim_keys",
    if (r_data$vim_keys) "Vim keys (on)" else "Vim keys (off)")
})

dtree_max_min <- c("Max" = "max", "Min" = "min")

output$ui_dtree_list <- renderUI({
  dtree_list <- r_data$dtree_list
  if (length(dtree_list) == 0) return(invisible())
  selectInput(inputId = "dtree_list", label = NULL,
    choices = dtree_list, selected = state_init("dtree_list", dtree_list[1]),
    multiple = FALSE, width = "100%")
})

output$ui_dtree_name <- renderUI({
  dtree_name <- input$dtree_list
  if (length(dtree_name) == 0) dtree_name <- dtree_name()
  if (is_empty(dtree_name)) dtree_name <- "dtree"
  textInput("dtree_name", NULL, dtree_name, width = "100px")
})

output$dtree <- renderUI({
  tabsetPanel(
    id = "tabs_dtree",
    tabPanel("Model",
    with(tags,
      table(
            td(help_modal('Decision tree','dtree_help', inclRmd(file.path(r_path,"quant/tools/help/dtree.Rmd")))),
            td(HTML("&nbsp;&nbsp;")),
            td(HTML("<i title='Report results' class='fa fa-edit action-button shiny-bound-input' href='' id='dtree_report'></i>")),
            td(HTML("&nbsp;&nbsp;")),
            td(HTML("&nbsp;&nbsp;")),
            td(radioButtons(inputId = "dtree_opt", label = NULL,
               dtree_max_min, selected = state_init("dtree_opt", "max"), inline = TRUE)),
            td(actionButton("dtree_eval", "Calculate"), style="padding-top:5px;"),
            td(uiOutput("ui_dtree_name")),
            # td(textInput("dtree_name", NULL, paste0("dtree"), width = "100px")),
            td(actionButton("dtree_store", "Store"), style= "padding-top:5px;"),
            td(uiOutput("ui_dtree_list"), style="padding-top:14px;"),
            td(uiOutput("ui_dtree_vim"), style="padding-top:5px;"),
            td(downloadButton("dtree_save_yaml", "Save input"), style="padding-top:5px;"),
            td(downloadButton("dtree_save", "Save output"), style="padding-top:5px;"),
            td(HTML("<div class='form-group shiny-input-container'><input id='dtree_load_yaml' name='dtree_load_yaml' type='file' accept='.yaml'/></div>"))
      )
    ),

    shinyAce::aceEditor("dtree_edit", mode = "yaml",
              vimKeyBinding = ifelse (is.null(r_data$vim_keys), FALSE, r_data$vim_keys),
              wordWrap = TRUE,
              height = "auto",
              value = state_init("dtree_edit", dtree_example),
              hotkeys = list(dtree_run = list(win = "CTRL-ENTER", mac = "CMD-ENTER"))),
    verbatimTextOutput("dtree_print")
  ),
    tabPanel("Plot",
      actionLink("dtree_save_plot", "", class = "fa fa-download alignright", onclick = "window.print();"),
      with(tags, table(
        td(radioButtons(inputId = "dtree_plot_init", label = "Plot decision tree:",
          c("Initial" = FALSE, "Final" = TRUE),
          selected = state_init("dtree_plot_init", FALSE), inline = TRUE)),
        td(actionButton("dtree_eval_plot", "Calculate"), style="padding-top:30px;"),
        td(numericInput("dtree_round", "Round", value = state_init("dtree_round", 3),
           min = 0, max = 10, width = "50px")),
        td(textInput("dtree_symbol", "Symbol", state_init("dtree_symbol", "$"), width = "50px"))
      )),
      DiagrammeR::DiagrammeROutput("dtree_plot", height = "600px"))
    # ,tabPanel("Sensitivity", verbatimTextOutput("something")
      # actionLink("dtree_save_splot", "", class = "fa fa-download alignright", onclick = "window.print();"),
#       with(tags, table(
#         td(radioButtons(inputId = "dtree_plot_init", label = "Plot decision tree:",
#           c("Initial" = FALSE, "Final" = TRUE),
#           selected = state_init("dtree_plot_init", FALSE), inline = TRUE)),
#         td(actionButton("dtree_eval_plot", "Calculate"))
#       )),
      # DiagrammeR::DiagrammeROutput("dtree_plot", height = "600px")
    # )
  )
})

vals_dtree <- reactiveValues(dtree_run = 0)

observe({
  input$dtree_run
  input$dtree_eval_plot
  if (!is.null(input$dtree_eval)) isolate(vals_dtree$dtree_run %<>% add(1))
})

dtree_name <- reactive({

    dtree_name <- input$dtree_name
    if (is_empty(dtree_name)) {
      dtree_name <- stringr::str_match(input$dtree_edit, "^\\s*name:\\s*(.*)\\n\\s*type:")[2]
      if (is.na(dtree_name)) {
        dtree_name <- "dtree"
      } else {
        dtree_name %<>% tolower %>% gsub("[^[:alnum:] ]", "", .) %>%
          gsub("\\s+","_",.) %>% gsub("^([0-9]+)",".",.)
      }
    }
    dtree_name
})

observeEvent(input$dtree_store, {
  isolate({

    # dtree_name <- input$dtree_name
    # if (is_empty(dtree_name)) {
    #   dtree_name <- stringr::str_match(input$dtree_edit, "^\\s*name:\\s*(.*)\\n\\s*type:")[2]
    #   if (is.na(dtree_name)) {
    #     dtree_name <- "dtree"
    #   } else {
    #     dtree_name %<>% tolower %>% gsub("[^[:alnum:] ]", "", .) %>%
    #       gsub("\\s+","_",.) %>% gsub("^([0-9]+)",".",.)
    #   }
    # }
    dtree_name <- dtree_name()

    r_data[[dtree_name]] <- input$dtree_edit
    r_data[["dtree_list"]] <- c(dtree_name, r_data[["dtree_list"]]) %>% unique
    updateSelectInput(session = session, inputId = "dtree_list", selected = dtree_name)
  })
})

dtree_eval <- reactive({
  if (vals_dtree$dtree_run == 1) return(invisible())
  isolate({
    if (input$dtree_edit != "") {
      withProgress(message = 'Creating decision tree', value = 0, {
        dtree(input$dtree_edit, opt = input$dtree_opt)
      })
    }
  })
})

output$dtree_print <- renderPrint({
  dtree_eval() %>% {if (is.null(.)) invisible() else summary(.)}
})

output$dtree_plot <- DiagrammeR::renderDiagrammeR({
  if (is_empty(input$dtree_plot_init)) return(invisible())
  dt <- dtree_eval()
  if (is.null(dt)) {
    return(invisible())
  } else {
    DiagrammeR::DiagrammeR(plot(dt, symbol = input$dtree_symbol,
                           dec = input$dtree_round, final = input$dtree_plot_init,
                           shiny = TRUE))
  }
})

.plot_dtree <- reactive({
  if (is_empty(input$dtree_plot_init)) return(invisible())
  dt <- dtree_eval()
  if (is.null(dt)) {
    return(invisible())
  } else {
    # DiagrammeR(plot(dt, final = input$dtree_plot_init, shiny = TRUE))
    plot(dt, symbol = input$dtree_symbol, dec = input$dtree_round,
         final = input$dtree_plot_init, shiny = TRUE)
  }
})

output$dtree_save <- downloadHandler(
  filename = function() {"dtree.txt"},
  content = function(file) {
    isolate({
      capture.output(dtree(input$dtree_edit) %>% summary) %>% cat(.,file=file,sep="\n")
    })
  }
)

output$dtree_save_yaml <- downloadHandler(
  filename = function() {"dtree.yaml"},
  content = function(file) {
    isolate({
      cat(paste0(input$dtree_edit,"\n"), file = file)
    })
  }
)

observe({
  ## loading yaml file from disk
  inFile <- input$dtree_load_yaml
  if (!is.null(inFile) && !is.na(inFile)) {
    isolate({

      yaml_file <- paste0(readLines(inFile$datapath), collapse = "\n")
      # print(inFile$name)
      # dtree_name <- "test.yaml"
      dtree_name <- sub(paste0(".",tools::file_ext(inFile$name)),"",inFile$name)
      r_data[[dtree_name]] <- yaml_file
      r_data[["dtree_list"]] <- c(dtree_name, r_data[["dtree_list"]]) %>% unique
      shinyAce::updateAceEditor(session, "dtree_edit", value = yaml_file)
    })
  }
})

observeEvent(input$dtree_list, {
  shinyAce::updateAceEditor(session, "dtree_edit", value = r_data[[input$dtree_list]])
})

observeEvent(input$dtree_report, {
  isolate({

    # dtree_name <- dtree_name()
    # dtree_name <- stringr::str_match(input$dtree_edit, "^\\s*name:\\s*(.*)\\n\\s*type:")[2]
    # if (is.na(dtree_name)) {
    #   dtree_name <- "dtree"
    # } else {
    #   dtree_name %<>% tolower %>% gsub("[^[:alnum:] ]", "", .) %>%
    #     gsub("\\s+","_",.) %>% gsub("^([0-9]+)",".",.)
    # }

    dtree_name <- input$dtree_list
    if (is_empty(dtree_name)) dtree_name <- input$dtree_name
    if (is_empty(dtree_name)) dtree_name <- dtree_name()

    r_data[[dtree_name]] <- input$dtree_edit
    r_data[["dtree_list"]] <- c(dtree_name, r_data[["dtree_list"]]) %>% unique

    update_report(inp_main = list(yl = dtree_name, opt = input$dtree_opt),
                  fun_name = "dtree",
                  inp_out = list("",""), outputs = "summary",
                  figs = FALSE)
  })
})
