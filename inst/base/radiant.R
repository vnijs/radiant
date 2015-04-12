observe({
  # reset r_state on dataset change ... when you are not on the
  # Manage > Data tab
  if(is.null(r_state$dataset) || is.null(input$dataset)) return()
  if(input$datatabs != "Manage" || input$nav_radiant != "Data")
    if(r_state$dataset != input$dataset) r_state <<- list()
})

################################################################################
# function to save app state on refresh or crash
################################################################################
saveStateOnRefresh <- function(session = session) {
  session$onSessionEnded(function() {
    isolate({
      if(not_pressed(input$resetState) && not_pressed(input$quitApp) &&
         is.null(input$uploadState)) {
        r_sessions[[r_ssuid]] <- list(
          r_data = reactiveValuesToList(r_data),
          r_state = reactiveValuesToList(input),
          timestamp = Sys.time()
        )
        if(r_local) rm(r_env, envir = .GlobalEnv)
      } else {
        if(is.null(input$uploadState))
          try(r_sessions[[r_ssuid]] <- NULL, silent = TRUE)
      }
    })
  })
}

################################################################
# functions used across tools in radiant
################################################################
.changedata <- function(new_col, new_col_name = "", dataset = input$dataset) {
	if(nrow(r_data[[dataset]]) == new_col %>% nrow &&
     new_col_name[1] != "")
    r_data[[dataset]][,new_col_name] <- new_col
}

.getdata <- reactive({

  if(is_empty(input$data_filter) | input$show_filter == FALSE) return(r_data[[input$dataset]])
  selcom <- gsub("\\s","", input$data_filter)
  if(selcom != "") {
    seldat <- try(filter_(r_data[[input$dataset]], selcom), silent = TRUE)

    if(is(seldat, 'try-error')) {
      isolate(r_data$filter_error <- attr(seldat,"condition")$message)
    } else {
      isolate(r_data$filter_error <- "")
      return(seldat)
    }
  } else {
    isolate(r_data$filter_error <- "")
  }

  r_data[[input$dataset]]
})

.getclass <- reactive({
  r_data[[input$dataset]] %>% head %>% getclass
})

groupable_vars <- reactive({
  .getdata() %>%
    summarise_each(funs(n_distinct)) %>%
    { . < 10 } %>%
    which(.) %>%
    varnames()[.]
})

two_level_vars <- reactive({
  .getdata() %>%
    summarise_each(funs(n_distinct)) %>%
    { . == 2 } %>%
    which(.) %>%
    varnames()[.]
})

varnames <- reactive({
  .getclass() %>% names %>%
    set_names(., paste0(., " {", .getclass(), "}"))
})

# cleaning up the arguments for data_filter and defaults passed to report
clean_args <- function(rep_args, rep_default = list()) {
  if(!is.null(rep_args$data_filter)) {
    if(rep_args$data_filter == "")
      rep_args$data_filter  <- NULL
    else
      rep_args$data_filter %<>% gsub("\\n","", .) %>% gsub("\"","\'",.)
  }

  if(length(rep_default) == 0) rep_default[names(rep_args)] <- ""

  # removing default arguments before sending to report feature
  for(i in names(rep_args))
    if(all(rep_args[[i]] == rep_default[[i]])) rep_args[[i]] <- NULL
  rep_args
}

# check if a variable is null or not in the selected data.frame
not_available <- function(x)
  if(any(is.null(x)) || (sum(x %in% varnames()) < length(x))) TRUE else FALSE

# check if a button was NOT pressed
not_pressed <- function(x) if(is.null(x) || x == 0) TRUE else FALSE

# check for duplicate entries
has_duplicates <- function(x)
  if(length(unique(x)) < length(x)) TRUE else FALSE

# is x some type of date variable
is_date <- function(x) is.Date(x) | is.POSIXct(x) | is.POSIXt(x)

# convert a date variable to character for printing
d2c <- function(x) if(is_date(x)) as.character(x) else x

# truncate character fields for show_data_snippet
trunc_char <- function(x) if(is.character(x)) strtrim(x,10) else x

# show a few rows of a dataframe
show_data_snippet <- function(dat = input$dataset, nshow = 5, title = "") {
  { if(is.character(dat) && length(dat) == 1) r_data[[dat]] else dat } %>%
    slice(1:min(nshow,nrow(.))) %>%
    mutate_each(funs(d2c)) %>%
    mutate_each(funs(trunc_char)) %>%
    xtable::xtable(.) %>%
    print(type='html',  print.results = FALSE, include.rownames = FALSE) %>%
    paste0(title, .) %>%
    sub("<table border=1>","<table class='table table-condensed table-hover'>", .) %>%
    paste0(.,'<label>',nshow,' (max) rows shown. See View-tab for details.</label>') %>%
    enc2utf8
}

suggest_data <- function(text = "", dat = "diamonds")
  paste0(text, "For an example dataset go to Data > Manage, select the 'examples' radio button,\nand press the 'Load examples' button. Then select the \'", dat, "\' dataset")

# function written by @wch https://github.com/rstudio/shiny/issues/781#issuecomment-87135411
capture_plot <- function(expr, env = parent.frame()) {
  structure(
    list(expr = substitute(expr), env = env),
    class = "capture_plot"
  )
}

# function written by @wch https://github.com/rstudio/shiny/issues/781#issuecomment-87135411
print.capture_plot <- function(x, ...) {
  eval(x$expr, x$env)
}

################################################################
# functions used to create Shiny in and outputs
################################################################

returnTextAreaInput <- function(inputId, label = NULL, value = "") {
  tagList(
    tags$label(label, `for` = inputId),br(),
    tags$textarea(id=inputId, type = "text", rows="2",
                  class="returnTextArea form-control", value)
  )
}

plot_width <- function()
  if(is.null(input$viz_plot_width)) r_data$plot_width else input$viz_plot_width

plot_height <- function()
  if(is.null(input$viz_plot_height)) r_data$plot_height else input$viz_plot_height

# fun_name is a string of the main function name
# rfun_name is a string of the reactive wrapper that calls the main function
# out_name is the name of the output, set to fun_name by default
register_print_output <- function(fun_name, rfun_name,
                                  out_name = fun_name) {

  # Generate output for the summary tab
  output[[out_name]] <- renderPrint({
    # when no analysis was conducted (e.g., no variables selected)
    get(rfun_name)() %>%
    { if(is.character(.)) cat(.,"\n") else . } %>% rm

  })
}

# fun_name is a string of the main function name
# rfun_name is a string of the reactive wrapper that calls the main function
# out_name is the name of the output, set to fun_name by default
register_plot_output <- function(fun_name, rfun_name,
                                 out_name = fun_name,
                                 width_fun = "plot_width",
                                 height_fun = "plot_height") {

  # Generate output for the plots tab
  output[[out_name]] <- renderPlot({

    # when no analysis was conducted (e.g., no variables selected)
    get(rfun_name)() %>% { if(is.null(.)) " " else . } %>%
    { if(is.character(.)) {
        plot(x = 1, type = 'n', main= . , axes = FALSE, xlab = "", ylab = "")
      } else {
        withProgress(message = 'Making plot', value = 0, print(.))
      }
    }

    return(invisible())

  }, width=get(width_fun), height=get(height_fun))
}

stat_tab_panel <- function(menu, tool, tool_ui, output_panels,
                           data = input$dataset) {
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML(paste("<label><strong>Menu:",menu,"</strong></label><br>")),
        HTML(paste("<label><strong>Tool:",tool,"</strong></label><br>")),
        if(!is.null(data))
          HTML(paste("<label><strong>Data:",data,"</strong></label>"))
      ),
      uiOutput(tool_ui)
    ),
    mainPanel(
      output_panels
    )
  )
}

################################################################
# functions used for app help
################################################################
help_modal <- function(modal_title, link, help_file) {
  sprintf("<div class='modal fade' id='%s' tabindex='-1' role='dialog' aria-labelledby='%s_label' aria-hidden='true'>
            <div class='modal-dialog'>
              <div class='modal-content'>
                <div class='modal-header'>
                  <button type='button' class='close' data-dismiss='modal' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
                  <h4 class='modal-title' id='%s_label'>%s</h4>
                  </div>
                <div class='modal-body'>%s<br>
                  &copy; Vincent Nijs (2015) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/80x15.png' /></a>
                </div>
              </div>
            </div>
           </div>
           <i title='Help' class='glyphicon glyphicon-question-sign' data-toggle='modal' data-target='#%s'></i>",
           link, link, link, modal_title, help_file, link) %>%
  enc2utf8 %>% HTML
}

help_and_report <- function(modal_title, fun_name, help_file) {
  sprintf("<div class='modal fade' id='%s_help' tabindex='-1' role='dialog' aria-labelledby='%s_help_label' aria-hidden='true'>
            <div class='modal-dialog'>
              <div class='modal-content'>
                <div class='modal-header'>
                  <button type='button' class='close' data-dismiss='modal' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
                  <h4 class='modal-title' id='%s_help_label'>%s</h4>
                  </div>
                <div class='modal-body'>%s<br>
                  &copy; Vincent Nijs (2015) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/80x15.png' /></a>
                </div>
              </div>
            </div>
           </div>
           <i title='Help' class='glyphicon glyphicon-question-sign alignleft' data-toggle='modal' data-target='#%s_help'></i>
           <i title='Report results' class='glyphicon glyphicon-book action-button shiny-bound-input alignright' href='#%s_report' id='%s_report'></i>
           <div style='clear: both;'></div>",
          fun_name, fun_name, fun_name, modal_title, help_file, fun_name, fun_name, fun_name) %>%
  enc2utf8 %>% HTML %>% withMathJax()
}

# function to render .md files to html
inclMD <- function(path) {
  markdown::markdownToHTML(path, fragment.only = TRUE, options = c(""),
                           stylesheet=file.path(r_path,"/base/www/empty.css"))
}

# function to render .Rmd files to html - does not embed image or add css
inclRmd <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = '\n') %>%
  knitr::knit2html(text = ., fragment.only = TRUE, options = "",
                   stylesheet=file.path(r_path,"/base/www/empty.css"))
}
