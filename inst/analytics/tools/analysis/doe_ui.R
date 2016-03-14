## list of function arguments
doe_args <- as.list(formals(doe))

## list of function inputs selected by user
doe_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  for (i in names(doe_args))
    doe_args[[i]] <- input[[paste0("doe_",i)]]
  doe_args
})

output$ui_doe_int <- renderUI({

  req(!is_empty(input$doe_factors))

# factors <- "Annual   fee;  $0 ;   $20 ;



# Card_type ; MC ; Visa  ; ;




# "
# factors <- "Annual_fee;  $0;  $20
# Card_type ; MC; Visa"

# factors <- "Annual fee;  $0;  $20

# Card type ; MC; Visa

# test  ; lkjlkads

# "
#   gsub("$","\n", factors) %>%
#   gsub("[ ]*;[^\n]*[\n]+","\n",.) %>%
#   %>%
#   gsub("\n[\\s]*","\n",.)

#   %>%
#   sub("\n$","",.)

#   %>%
#   gsub("[\n[ ]*\n",";",.)

#   %>%


# vars <-
#   # gsub("$","\n", input$doe_factors) %>%
#   gsub("$","\n", factors) %>%
#   gsub("[ ]*;[^\n]*[\n]+","\n",.) %>%
#   gsub("\n[ ]+\n",";",.) %>%
#   sub("\n$","",.)

#   %>%
#   strsplit(.,"\n") %>% .[[1]] %>% unique

  vars <-
    gsub("[ ]{2,}"," ",input$doe_factors) %>%
    gsub("/","",.) %>%
    gsub("[ ]*;[ ]*",";",.) %>%
    gsub(";{2,}",";",.) %>%
    gsub("[;]+[ ]{0,}\n","\n",.) %>%
    gsub("[ ]{1,}\n","\n",.) %>%
    gsub("\n[ ]+","\n",.) %>%
    gsub("[\n]{2,}","\n",.) %>%
    gsub("[ ]+","_",.) %>%
    strsplit(.,"\n") %>% .[[1]] %>%
    strsplit(";") %>%
    sapply(function(x) x[1]) %>%
    unique

  if (length(vars) < 2)
    choices <- c()
  else
    choices <- iterms(vars, 2)

  selectInput("doe_int", label = "Interactions:", choices = choices,
    selected = state_init("doe_int"),
    multiple = TRUE, size = min(3,length(choices)), selectize = FALSE)
})

output$ui_doe_levels <- renderUI({
  req(input$doe_max > 2)
  make_level <- function(nr) {
    textInput(paste0("doe_level",nr), paste0("Level ",nr, ":"),
              value = state_init(paste0("doe_level", nr)))
  }

  lapply(3:input$doe_max, make_level)
})


output$ui_doe <- renderUI({
  tagList(
    wellPanel(
      tags$table(
        tags$td(numericInput("doe_max", label = "Max levels:", min = 2, max = 10,
                   value = state_init("doe_max", init = 2), width = "77px")),
        tags$td(numericInput("doe_trials", label = "# trials:", min = 0,
                   value = state_init("doe_trials", init = NA), width = "77px")),
        tags$td(numericInput("doe_seed", label = "Rnd. seed:", min = 0,
                   value = state_init("doe_seed", init = 172110), width = "77px"))
      ),
      HTML("<label>Variable name: <i id='doe_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
            <i id='doe_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
      textInput("doe_name", NULL, value = state_init("doe_name", "")),
      textInput("doe_level1", "Level 1:", value = state_init("doe_level1")),
      textInput("doe_level2", "Level 2:", value = state_init("doe_level2")),
      uiOutput("ui_doe_levels"),
      uiOutput("ui_doe_int")
    ),
    wellPanel(
      actionButton("doe_run", "Create"),
      downloadButton('doe_download_part', 'Partial'),
      downloadButton('doe_download_full', 'Full')
    ),
    wellPanel(

      HTML("<label>Upload factors:</label><div class='form-group shiny-input-container'>
              <input id='doe_upload' name='doe_upload' type='file' accept='.txt'/>
            </div><br><label>Download factors:</label><br>"),
      downloadButton('doe_download', 'Download')
    ),
    help_and_report(modal_title = "Design of Experiments", fun_name = "doe",
                    help_file = inclMD(file.path(r_path,"analytics/tools/help/doe.md")))
  )
})

observeEvent(input$doe_add, {
  req(input$doe_max)

  dup <- input$doe_name
  for (i in 1:input$doe_max) {
    dtmp <- input[[paste0("doe_level",i)]]
    if (!is_empty(dtmp)) dup <- c(dup, dtmp)
  }
  dup <- paste(dup, collapse = "; ")

  if (is_empty(input[["doe_factors"]]))
    val <- dup
  else
    val <- paste0(dup, "\n", input[["doe_factors"]])

  updateTextInput(session = session, "doe_factors", value = val)
})

observeEvent(input$doe_del, {
  input[["doe_factors"]] %>% strsplit("\n") %>% unlist %>% .[-1] %>% paste0(collapse = "\n") %>%
    updateTextInput(session = session, "doe_factors", value = .)
})

doe_maker <- function(id = "factors", rows = "5", pre = "doe_") {
  id <- paste0(pre, id)
  tags$textarea(state_init(id), id = id, type = "text", rows = rows, class = "form-control")
}

## output is called from the main radiant ui.R
output$doe <- renderUI({

    register_print_output("summary_doe", ".summary_doe")

    ## single tab with components stacked
    doe_output_panels <- tagList(
      tabPanel("Summary",
        HTML("<label>Design factors:</label>"),
        doe_maker("factors", rows = "5"),
        HTML("<br><label>Generated experimental design:</label>"),
        verbatimTextOutput("summary_doe")
      )
    )

    stat_tab_panel(menu = "Model",
                  tool = "DOE",
                  data = NULL,
                  tool_ui = "ui_doe",
                  output_panels = doe_output_panels)
})

.doe <- eventReactive(input$doe_run, {
  req(!is_empty(input$doe_factors))

  int <- ""
  if (length(input$doe_int) > 0)
    int <- input$doe_int

  withProgress(message = 'Generating design', value = 0, {
    do.call(doe, doe_inputs())
  })
})

.summary_doe <- reactive({
  summary(.doe(), eff = TRUE, part = TRUE, full = TRUE)
})

output$doe_download_part <- downloadHandler(
  filename = function() { 'part_factorial.csv' },
  content = function(file) {
    .doe() %>%
    # { if (class(.)[1] == "character") . else sample_n(.$part, size = nrow(.$part)) } %>%
    { if (class(.)[1] == "character") . else .$part } %>%
    write.csv(file)
  }
)

output$doe_download_full <- downloadHandler(
  filename = function() { 'full_factorial.csv' },
  content = function(file) {
    .doe() %>%
    # { if (class(.)[1] == "character") . else sample_n(.$full, size = nrow(.$full)) } %>%
    { if (class(.)[1] == "character") . else .$full } %>%
    write.csv(file)
  }
)

output$doe_download <- downloadHandler(
  filename = function() { 'design_factors.txt' },
  content = function(file) {
    cat(paste0(input$doe_factors,"\n"), file  = file)
  }
)

observeEvent(input$doe_upload, {
    paste0(readLines(input$doe_upload$datapath), collapse = "\n") %>%
    updateTextInput(session = session, "doe_factors", value = .)

    ## cleaning out previous settings
    updateNumericInput(session = session, "doe_max", value = 2)
    updateNumericInput(session = session, "doe_trials", value = NA)
    updateTextInput(session = session, "doe_name", value = "")
    for (i in 1:9) {
      r_state[[paste0("doe_level",i)]] <<- NULL
      updateTextInput(session = session, paste0("doe_level",i), value = "")
    }
})

observeEvent(input$doe_report, {
  xcmd <- "# write.csv(result$part, file = '~/part_factorial.csv')"
  inp_out = list(list(eff = TRUE, part = TRUE, full = TRUE))
  update_report(inp_main = clean_args(doe_inputs(), doe_args),
                fun_name = "doe",
                outputs = "summary",
                inp_out = inp_out,
                figs = FALSE,
                xcmd = xcmd)
})
