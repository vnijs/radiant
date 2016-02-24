output$ui_doe_int <- renderUI({

  req(!is_empty(input$doe_profiles))

  # dp <- "free_ship; $300; $200\ndiscount; 15%; 20%\ncoup_entry; manual; automatic"
  # dp_list <-
  vars <-
    gsub("[\n]{2,}$","",input$doe_profiles) %>%
    strsplit(.,"\n") %>% .[[1]] %>%
    strsplit(";") %>%
    sapply(function(x) x[1]) %>%
    unique

  if (length(vars) < 2)
    choices <- c()
  else
    choices <- iterms(vars, 2)

  selectInput("doe_int", label = "Interactions:", choices = choices,
    selected = use_input_nonvar("doe_int", choices),
    multiple = TRUE, size = min(3,length(choices)), selectize = FALSE)
})

output$ui_doe_levels <- renderUI({

  req(input$doe_max > 2)

  make_level <- function(nr) {
    textInput(paste0("doe_level",nr), paste0("Level ",nr, ":"),
              value = use_input_nonvar(paste0("doe_level", nr)))
  }

  lapply(3:input$doe_max, make_level)
})


output$ui_doe <- renderUI({
  tagList(
    wellPanel(
      tags$table(

        tags$td(numericInput("doe_max", label = "Max levels:", min = 2, max = 10,
                   value = use_input_nonvar("doe_max", init = 2), width = "115px")),
        tags$td(numericInput("doe_trials", label = "# trials:", min = 0,
                   value = use_input_nonvar("doe_trials", init = NA), width = "115px"))
      ),
      HTML("<label>Variable name: <i id='doe_add' title='Add variable' href='#' class='action-button fa fa-plus-circle'></i>
            <i id='doe_del' title='Remove variable' href='#' class='action-button fa fa-minus-circle'></i></label>"),
      textInput("doe_name", NULL, value = state_init("doe_name", "")),
      textInput("doe_level1", "Level 1:", value = use_input_nonvar("doe_level1")),
      textInput("doe_level2", "Level 2:", value = use_input_nonvar("doe_level2")),
      uiOutput("ui_doe_levels"),
      uiOutput("ui_doe_int")
    ),
    wellPanel(
      actionButton("doe_run", "Create"),
      downloadButton('doe_download', 'Partial'),
      downloadButton('doe_download_full', 'Full')
    ),
    wellPanel(

      HTML("<label>Upload profiles:</label><div class='form-group shiny-input-container'>
              <input id='doe_upload' name='doe_upload' type='file' accept='.txt'/>
            </div><br><label>Save profiles:</label><br>"),
      # fileInput('upload_dp', 'Upload profiles', multiple=FALSE, accept = ".txt"),
      downloadButton('doe_save', 'Save')
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

  if (is_empty(input[["doe_profiles"]]))
    val <- dup
  else
    val <- paste0(dup, "\n", input[["doe_profiles"]])

  updateTextInput(session = session, "doe_profiles", value = val)
})

observeEvent(input$doe_del, {
  input[["doe_profiles"]] %>% strsplit("\n") %>% unlist %>% .[-1] %>% paste0(collapse = "\n") %>%
    updateTextInput(session = session, "doe_profiles", value = .)
})

doe_maker <- function(id = "profiles", rows = "5", pre = "doe_") {
  id <- paste0(pre, id)
  tags$textarea(state_init(id), id = id, type = "text", rows = rows, class = "form-control")
}

## output is called from the main radiant ui.R
output$doe <- renderUI({

    register_print_output("summary_doe", ".summary_doe")

    ## single tab with components stacked
    doe_output_panels <- tagList(
      tabPanel("Summary",
        HTML("<label>Design profiles:</label>"),
        doe_maker("profiles", rows = "5"),
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
  req(!is_empty(input$doe_profiles))

  int <- ""
  if (length(input$doe_int) > 0)
    int <- input$doe_int

  withProgress(message = 'Generating design', value = 0, {
    doe(input$doe_profiles, int, trials = input$doe_trials)
  })
})

doe <- function(dp, int = "", trials = NA) {
  # dp <- "free_ship; $300; $200\ndiscount; 15%; 20%\ncoup_entry; manual; automatic"
  dp_list <- gsub("[\n]{2,}$","",dp) %>% strsplit(.,"\n") %>% .[[1]] %>% strsplit(";")
  dp_names <- c()
  for (i in seq_len(length(dp_list))) {
    dt <- dp_list[[i]] %>% gsub("^\\s+|\\s+$", "", .)
    dp_names <- c(dp_names, dt[1])
    dp_list[[i]] <- dt[-1]
  }
  names(dp_list) <- dp_names
  model <- paste0("~ ", paste0(dp_names, collapse = " + "))
  nInt <- 0
  if (!is_empty(int)) {
    model <- paste0(model, " + ", paste0(int, collapse = " + "))
    nInt <- length(int)
  }
  part_frac(dp_list, model = as.formula(model), int = nInt, trials = trials)
}

part_frac <- function(dp, model = ~ ., int = 0, trials = NA, rseed = 172110) {

  full <- expand.grid(dp)

  ###############################################
  # eliminate combinations from full
  # by removing then from the variable _experiment_
  # http://stackoverflow.com/questions/18459311/creating-a-fractional-factorial-design-in-r-without-prohibited-pairs?rq=1
  ###############################################

  nr_levels <- sapply(dp, length) %>% sum
  min_profiles <- nr_levels - length(dp) + 1
  max_profiles <- nrow(full)

  if (!is_not(trials)) max_profiles <- min_profiles <- trials

  for (i in min_profiles:max_profiles) {
    set.seed(rseed)     # needs to be in the loop
    design <- try(optFederov(model, data = full, nRepeat = 50,
                         nTrials = i, maxIteration=1000,
                         approximate = FALSE), silent = TRUE)
    if (is(design, 'try-error')) next
    cor_mat <- cor(data.matrix(design$design))
    # print(as.dist(cor_mat), digits = 1)
    # cat('\nD-efficiency:',design$Dea,'\n')
    if (design$Dea == 1) break
    # if (det(cor_mat)==1) break
  }

  list(dp = dp, cor_mat = cor_mat, Dea = design$Dea, part = design$design, full = full)
}

.summary_doe <- reactive({
  summary.doe(.doe())
})

summary.doe <- function(object, ...) {

  cat("Experimental design\n")
  cat("# trials for partial factorial:", nrow(object$part),"\n")
  cat("# trials for full factorial   :", nrow(object$full),"\n")
  cat("D-efficiency                  :", object$Dea)

  cat("\n\nAttributes and levels:\n")
  # print(as.data.frame(object$dp))
  # apply(object$dp, 1, function(x) {cat(x); cat("\n")})
  nl <- names(object$dp)
  for (i in nl) {
    cat(paste0(i, ":"), paste0(object$dp[[i]], collapse = ", "), "\n")

  }

  # sapply(object$dp, function(x) {cat(names(x)); cat(x); cat("\n")})
  # print(object$dp)

  cat("\nPartial factorial design correlations:\n")
  print(object$cor_mat, row.names = FALSE)

  cat("\nPartial factorial design:\n")
  print(object$part, row.names = FALSE)

  cat("\nFull factorial design:\n")
  print(object$full, row.names = FALSE)
}

output$doe_download <- downloadHandler(
  filename = function() { 'part_factorial.csv' },
  content = function(file) {
    .doe() %>%
    { if (class(.)[1] == "character") . else sample_n(.$part, size = nrow(.$part)) } %>%
    write.csv(file, row.names = FALSE)
  }
)

output$doe_download_full <- downloadHandler(
  filename = function() { 'full_factorial.csv' },
  content = function(file) {
    .doe() %>%
    { if (class(.)[1] == "character") . else sample_n(.$full, size = nrow(.$full)) } %>%
    write.csv(file, row.names = FALSE)
  }
)

output$doe_save <- downloadHandler(
  filename = function() { 'profiles.txt' },
  content = function(file) {
    # write.csv(file, row.names = FALSE)
    cat(paste0(input$doe_profiles,"\n"), file  = file)
  }
)

observeEvent(input$doe_upload, {
    paste0(readLines(input$doe_upload$datapath), collapse = "\n") %>%
    updateTextInput(session = session, "doe_profiles", value = .)

    ## cleaning out previous settings
    updateNumericInput(session = session, "doe_max", value = 2)
    updateNumericInput(session = session, "doe_trials", value = NA)
    updateTextInput(session = session, "doe_name", value = "")
    for (i in 1:9) {
      r_state[[paste0("doe_level",i)]] <<- NULL
      updateTextInput(session = session, paste0("doe_level",i), value = "")
    }
})


# report_cleaner <- function(x) x %>% gsub("\n",";",.) %>% gsub("[;]{2,}",";",.)

# observeEvent(input$simulater_report, {
#   sim_dec <- input$sim_dec %>% {ifelse(is.na(.), 3, .)}
#   update_report(inp_main = clean_args(sim_inputs(), sim_args) %>% lapply(report_cleaner),
#                 fun_name = "simulater", inp_out = list(list(dec = sim_dec),""),
#                 outputs = c("summary","plot"), figs = TRUE,
#                 fig.width = round(7 * sim_plot_width()/650,2),
#                 fig.height = round(7 * (sim_plot_height()/650),2))
# })

# observeEvent(input$repeater_report, {
#   outputs <- c("summary", "plot")
#   inp_out <- list("","")
#   inp_out[[1]] <- clean_args(rep_sum_inputs(), rep_sum_args[-1]) %>% lapply(report_cleaner)
#   inp_out[[2]] <- clean_args(rep_plot_inputs(), rep_plot_args[-1]) %>% lapply(report_cleaner)

#   update_report(inp_main = clean_args(rep_inputs(), rep_args) %>% lapply(report_cleaner),
#                 fun_name = "repeater", inp_out = inp_out,
#                 outputs = outputs, figs = TRUE,
#                 fig.width = round(7 * rep_plot_width()/650,2),
#                 fig.height = round(7 * (rep_plot_height()/650),2))
# })
