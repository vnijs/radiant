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
            td(actionButton("dtree_eval", "Calculate")),
            td(uiOutput("ui_dtree_vim")),
            td(downloadButton("dtree_save_yaml", "Save input")),
            td(downloadButton("dtree_save", "Save output")),
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

dtree_eval <- reactive({
  if (vals_dtree$dtree_run == 1) return(invisible())
  isolate({
    if (input$dtree_edit != "") {
      withProgress(message = 'Creating decision tree', value = 0, {
        dtree(input$dtree_edit)
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
      shinyAce::updateAceEditor(session, "dtree_edit", value = yaml_file)
    })
  }
})

observe({
  if (not_pressed(input$dtree_report)) return()
  isolate({

# input <- list()
# input$dtree_edit <- " name: Jenny Lind
#  type: chance
#  High:
#      p: 0.3
#      type: decision
#      Accept:
#          payoff: 4000
#      Reject:
#          payoff: 900
#  Low:
#      p: 0.7
#      type: decision
#      Accept:
#          payoff: 1000
#      Reject:
#          payoff: 900"


    dtree_name <- stringr::str_match(input$dtree_edit, "^\\s*name:\\s*(.*)\\n\\s*type:")[2]
    if (is.na(dtree_name)) {
      dtree_name <- "dtree"
    } else {
      dtree_name %<>% tolower %>% gsub("[^[:alnum:] ]", "", .) %>%
        gsub("\\s+","_",.) %>% gsub("^([0-9]+)",".",.)
    }

    # dtree_name <-
    #   sub("^\\s*name:\\s*(.*)\\ntype:.*", "\\1", input$dtree_edit) %>% tolower %>%
    #   gsub("[^[:alnum:] ]", "", .) %>% gsub("\\s+","_",.) %>%
    #   gsub("^([0-9]+)",".",.)

    ## code for plotting inside Radiant - can't export yet
    # DiagrammeR::renderDiagrammeR({
    #   DiagrammeR::DiagrammeR(plot(result, final = TRUE, shiny=TRUE))
    # })

    r_data[[dtree_name]] <- input$dtree_edit
    update_report(inp_main = list(yl = dtree_name),
                  fun_name = "dtree",
                  inp_out = list("",""), outputs = "summary",
                  figs = FALSE)
  })
})
