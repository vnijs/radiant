#######################################
## Create decision tree
#######################################

dtree_example <-
"## Example from https://github.com/gluc/useR15/blob/master/00_data/jennylind.yaml
name: Jenny Lind
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
  isolate(r_data$vim_keys %<>% {. == FALSE})
})

output$ui_dtree_vim <- renderUI({
  ## initialize vim_keys to false
  if(is.null(r_data$vim_keys)) r_data$vim_keys <- FALSE
  actionButton("dtree_vim_keys",
    if(r_data$vim_keys) "Vim keys (on)" else "Vim keys (off)")
})

output$dtree <- renderUI({
  tagList(
    with(tags,
      table(
            td(help_modal('Decision tree','dtree_help',
                       inclMD(file.path(r_path,"quant/tools/help/dtree.md")))),
            td(HTML("&nbsp;&nbsp;")),
            # td(actionLink("dtree_report", "", icon = icon("book"))),
            # td(actionLink("dtree_report", "", class="glyphicon glyphicon-book")),
            td(HTML("<i title='Report results' class='glyphicon glyphicon-book action-button shiny-bound-input' href='' id='dtree_report'></i>")),
            td(HTML("&nbsp;&nbsp;")),
            td(actionButton("eval_dtree", "Calculate")),
            td(uiOutput("ui_dtree_vim")),
            td(downloadButton("save_yaml", "Save input")),
            td(downloadButton("save_dtree", "Save output")),
            td(HTML("<div class='form-group shiny-input-container'>
                <input id='load_yaml' name='load_yaml' type='file' accept='.yaml'/>
              </div>"))
      )
    ),

    shinyAce::aceEditor("dtree_edit", mode = "yaml",
              vimKeyBinding = ifelse(is.null(r_data$vim_keys), FALSE, r_data$vim_keys),
              wordWrap = TRUE,
              height = "auto",
              value = state_init("dtree_edit", dtree_example),
              hotkeys = list(run_dtree = list(win = "CTRL-ENTER", mac = "CMD-ENTER"))),
    verbatimTextOutput("dtree_print")
  )
})

## Knit for report in Radiant
# knitIt2 <- function(text) {
#   # paste(knitr::knit2html(text = text, fragment.only = TRUE, quiet = TRUE, envir = r_env),
#   paste(knitr::knit2html(text = text, fragment.only = TRUE, quiet = TRUE,
#         envir = r_data$r_knitr), stylesheet = "",
#         "<script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>",
#         "<script>MathJax.Hub.Typeset();</script>", sep = '\n') %>% scrub %>% HTML
# }

vals_dtree <- reactiveValues(dtree_run = 0)

observe({
  input$run_dtree
  if (!is.null(input$eval_dtree)) isolate(vals_dtree$dtree_run %<>% add(1))
})

eval_dtree <- reactive({
  if (vals_dtree$dtree_run == 1) return(invisible())
  # if (not_pressed(input$eval_dtree) && not_pressed(input$run_dtree$randNum)) return(invisible())
  isolate({
    if (input$dtree_edit != "") {
      withProgress(message = 'Creating decision tree', value = 0, {
        dtree(input$dtree_edit) %>% summary
      })
    }
  })
})

output$dtree_print <- renderPrint({
  eval_dtree()
})

output$save_dtree <- downloadHandler(
  filename = function() {"dtree.txt"},
  content = function(file) {
    isolate({
      # dtree(input$dtree_edit) %>% summary %>% cat(.,file=file,sep="\n")
      capture.output(dtree(input$dtree_edit) %>% summary) %>% cat(.,file=file,sep="\n")
      # knitIt(input$dtree_edit) %>% cat(.,file=file,sep="\n")
    })
  }
)

output$save_yaml <- downloadHandler(
  filename = function() {"dtree.yaml"},
  content = function(file) {
    isolate({
      cat(input$dtree_edit,file=file,sep="\n")
    })
  }
)

observe({
  ## loading yaml file from disk
  inFile <- input$load_yaml
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
    dtree_name <- paste0("dtree",floor(runif(1, 1000, 9999)))
    r_data[[dtree_name]] <- input$dtree_edit
    update_report(inp_main = list(yl = dtree_name),
                  fun_name = "dtree",
                  inp_out = list("",""), outputs = "summary",
                  figs = FALSE)
  })
})

