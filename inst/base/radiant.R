# observe({
#   ## reset r_state on dataset change ... when you are not on the
#   ## Manage > Data tab
#   if (is.null(r_state$dataset) || is.null(input$dataset)) return()
#   if (input$tabs_data != "Manage" || input$nav_radiant != "Data") {
#     if (r_state$dataset != input$dataset) {
#       r_state <<- list()
#       updateTextInput(session = session, inputId = "data_filter", value = "")
#       updateCheckboxInput(session = session, inputId = "show_filter", value = FALSE)
#     }
#   }
# })

################################################################################
## function to save app state on refresh or crash
################################################################################

saveSession <- function(session = session) {
  if (!exists("r_sessions")) return()
  if (exists("r_state") && !is_empty(r_state)) {
    rs <- r_state
    rs_input <- reactiveValuesToList(input)
    rs[names(rs_input)] <- rs_input
  } else {
    rs <- reactiveValuesToList(input)
  }

  r_sessions[[r_ssuid]] <- list(
    r_data    = reactiveValuesToList(r_data),
    # r_state = reactiveValuesToList(input),
    r_state = rs,
    timestamp = Sys.time()
  )

  ## saving session information to file
  fn <- paste0(normalizePath("~/r_sessions"),"/r_", r_ssuid, ".rds")
  saveRDS(r_sessions[[r_ssuid]], file = fn)

  # if (!r_local)
  # saveRDS(r_sessions[[r_ssuid]], file = paste0("~/r_sessions/r_", r_ssuid, ".rds"))
}

observeEvent(input$refresh_radiant, {
  if (r_local) {
    fn <- normalizePath("~/r_sessions")
    file.remove(list.files(fn, full.names = TRUE))
  } else {
    fn <- paste0(normalizePath("~/r_sessions"),"/r_", r_ssuid, ".rds")
    if (file.exists(fn)) unlink(fn, force = TRUE)
  }

  try(r_ssuid <- NULL, silent = TRUE)
})

saveStateOnRefresh <- function(session = session) {
  session$onSessionEnded(function() {
    isolate({
      if (not_pressed(input$refresh_radiant) && not_pressed(input$stop_radiant) &&
          is.null(input$uploadState)) {
        saveSession(session)
        # if (r_local) sshh( rm(r_env, envir = .GlobalEnv) )
      } else {
        if (is.null(input$uploadState)) {
          if (exists("r_sessions")) {
            sshhr(try(r_sessions[[r_ssuid]] <- NULL, silent = TRUE))
            sshhr(try(rm(r_ssuid), silent = TRUE))
          }
        }
      }
    })
  })
}

################################################################
## functions used across tools in radiant
################################################################

## get active dataset and apply data-filter if available
.getdata <- reactive({

  if (is.null(input$dataset)) return()

  # selcom <- input$data_filter %>% gsub("\\s","", .) %>% gsub("\"","\'",.)
  selcom <- input$data_filter %>% gsub("\\n","", .) %>% gsub("\"","\'",.)
  if (is_empty(selcom) || input$show_filter == FALSE) {
    isolate(r_data$filter_error <- "")
  } else if (grepl("([^=!<>])=([^=])",selcom)) {
    isolate(r_data$filter_error <- "Invalid filter: never use = in a filter but == (e.g., year == 2014). Update or remove the expression")
  } else {
    seldat <- try(filter_(r_data[[input$dataset]], selcom), silent = TRUE)
    if (is(seldat, 'try-error')) {
      isolate(r_data$filter_error <- paste0("Invalid filter: \"", attr(seldat,"condition")$message,"\". Update or remove the expression"))
    } else {
      isolate(r_data$filter_error <- "")
      return(seldat)
    }
  }

  r_data[[input$dataset]]
})

.getclass <- reactive({
  head(r_data[[input$dataset]]) %>% getclass
})

## used for group_by and facet row/column
# groupable_vars <- reactive({
#   .getdata() %>%
#     summarise_each(funs(is.factor(.) || lubridate::is.Date(.) || (n_distinct(., na_rm = TRUE)/n()) < .25)) %>%
#     {which(. == TRUE)} %>%
#     varnames()[.]
# })

groupable_vars <- reactive({
  .getdata() %>%
    summarise_each(funs(is.factor(.) || lubridate::is.Date(.) || is.integer(.) ||
                        ((n_distinct(., na_rm = TRUE)/n()) < .30))) %>%
                        # ((n_distinct(., na_rm = TRUE)/n()) < .30 && !is.numeric(.)))) %>%
    {which(. == TRUE)} %>%
    varnames()[.]
})

## used in compare proportions
two_level_vars <- reactive({
  .getdata() %>%
    summarise_each(funs(n_distinct(., na_rm = TRUE))) %>%
    { . == 2 } %>%
    which(.) %>%
    varnames()[.]
})

## used in visualize - don't plot Y-variables that don't vary
varying_vars <- reactive({
  .getdata() %>%
    summarise_each(funs(does_vary(.))) %>%
    as.logical %>%
    which %>%
    varnames()[.]
})

## getting variable names in active dataset and their class
varnames <- reactive({
  .getclass() %>% names %>%
    set_names(., paste0(., " {", .getclass(), "}"))
})

## cleaning up the arguments for data_filter and defaults passed to report
clean_args <- function(rep_args, rep_default = list()) {
  if (!is.null(rep_args$data_filter)) {
    if (rep_args$data_filter == "")
      rep_args$data_filter  <- NULL
    else
      rep_args$data_filter %<>% gsub("\\n","", .) %>% gsub("\"","\'",.)
  }

  if (length(rep_default) == 0) rep_default[names(rep_args)] <- ""

  ## removing default arguments before sending to report feature
  for (i in names(rep_args)) {
    # if (is.na(rep_args[[i]])) || all(rep_args[[i]] == rep_default[[i]])) rep_args[[i]] <- NULL
    if (all(is.na(rep_args[[i]]))) {rep_args[[i]] <- NULL; next}
    # if (rep_default[[i]] == Inf || rep_default[[i]] == -Inf) next
    # if (is.symbol(rep_default[[i]])) next
    if (!all(is.symbol(rep_default[[i]])) && all(is_not(rep_default[[i]]))) next
    # print(rep_default[[i]])
    # if (rep_default[[i]] == NA) next
    if (all(rep_args[[i]] == rep_default[[i]])) rep_args[[i]] <- NULL
  }

  rep_args
}

is.symbol

## check if a variable is null or not in the selected data.frame
not_available <- function(x)
  if (any(is.null(x)) || (sum(x %in% varnames()) < length(x))) TRUE else FALSE

## check if a variable is null or not in the selected data.frame
available <- function(x) not_available(x) == FALSE

is_not <- function(x) is.null(x) || is.na(x)

## check if a button was NOT pressed
not_pressed <- function(x) if (is.null(x) || x == 0) TRUE else FALSE

## check if a button WAS pressed
# was_pressed <- function(x) if (is.null(x) || x == 0) FALSE else TRUE
# was_pressed <- function(x) not_pressed == FALSE

## check for duplicate entries
has_duplicates <- function(x)
  if (length(unique(x)) < length(x)) TRUE else FALSE

## is x some type of date variable
is_date <- function(x) inherits(x, c('Date', 'POSIXlt', 'POSIXct'))

## drop elements from .._args variables obtained using formals
r_drop <- function(x, drop = c("dataset","data_filter")) x[-which(x %in% drop)]

## convert a date variable to character for printing
d2c <- function(x) if (is_date(x)) as.character(x) else x

## truncate character fields for show_data_snippet
trunc_char <- function(x) if (is.character(x)) strtrim(x,40) else x

## show a few rows of a dataframe
show_data_snippet <- function(dat = input$dataset, nshow = 7, title = "") {

  n <- 0
  {if (is.character(dat) && length(dat) == 1) r_data[[dat]] else dat} %>%
    { n <<- nrow(.); . } %>%
    slice(1:min(nshow,n)) %>%
    mutate_each(funs(d2c)) %>%
    mutate_each(funs(trunc_char)) %>%
    xtable::xtable(.) %>%
    print(type = 'html',  print.results = FALSE, include.rownames = FALSE,
          sanitize.text.function = identity,
          html.table.attributes = "class='table table-condensed table-hover'") %>%
    paste0(title, .) %>%
    {if (n <= nshow) . else paste0(.,'\n<label>',nshow,' of ', n, ' rows shown. See View-tab for details.</label>')} %>%
    enc2utf8
}

suggest_data <- function(text = "", dat = "diamonds")
  paste0(text, "For an example dataset go to Data > Manage, select 'examples' from the\n'Load data of type' dropdown, and press the 'Load examples' button. Then\nselect the \'", dat, "\' dataset.")

## function written by @wch https://github.com/rstudio/shiny/issues/781#issuecomment-87135411
capture_plot <- function(expr, env = parent.frame()) {
  structure(
    list(expr = substitute(expr), env = env),
    class = "capture_plot"
  )
}

## function written by @wch https://github.com/rstudio/shiny/issues/781#issuecomment-87135411
print.capture_plot <- function(x, ...) {
  eval(x$expr, x$env)
}

################################################################
## functions used to create Shiny in and outputs
################################################################

## textarea where the return key submits the content
returnTextAreaInput <- function(inputId, label = NULL, value = "") {
  tagList(
    tags$label(label, `for` = inputId),br(),
    tags$textarea(value, id=inputId, type = "text", rows="2",
                  class="returnTextArea form-control")
  )
}

returnTextInput <- function(inputId, label = NULL, value = "") {
  tagList(
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value,
               class = "returnTextInput form-control")
  )
}

plot_width <- function()
  if (is.null(input$viz_plot_width)) r_data$plot_width else input$viz_plot_width

plot_height <- function()
  if (is.null(input$viz_plot_height)) r_data$plot_height else input$viz_plot_height

## fun_name is a string of the main function name
## rfun_name is a string of the reactive wrapper that calls the main function
## out_name is the name of the output, set to fun_name by default
register_print_output <- function(fun_name, rfun_name,
                                  out_name = fun_name) {

  ## Generate output for the summary tab
  output[[out_name]] <- renderPrint({
    ## when no analysis was conducted (e.g., no variables selected)
    get(rfun_name)() %>%
    { if (is.character(.)) cat(.,"\n") else . } %>% rm
  })
}

# fun_name is a string of the main function name
# rfun_name is a string of the reactive wrapper that calls the main function
# out_name is the name of the output, set to fun_name by default
register_plot_output <- function(fun_name, rfun_name,
                                 out_name = fun_name,
                                 width_fun = "plot_width",
                                 height_fun = "plot_height") {

  ## Generate output for the plots tab
  output[[out_name]] <- renderPlot({

    ## when no analysis was conducted (e.g., no variables selected)
    get(rfun_name)() %>% { if (is.null(.)) " " else . } %>%
    { if (is.character(.)) {
        plot(x = 1, type = 'n', main = paste0("\n\n\n\n\n\n\n\n",.) ,
             axes = FALSE, xlab = "", ylab = "")
      } else {
        withProgress(message = 'Making plot', value = 0, print(.))
      }
    }
  }, width=get(width_fun), height=get(height_fun))

  return(invisible())
}

plot_downloader <- function(plot_name, width = plot_width(),
                            height = plot_height(), pre = ".plot_", po = "dl_") {

  ## link and output name
  lnm <- paste0(po, plot_name)

  # psize <- . %>% {7 * ./650} %>% round(2)
  # fext <- . %>% tools::file_ext(.) %>% tolower

  ## create an output
  output[[lnm]] <- downloadHandler(
    filename = function() { paste0(plot_name, ".png") },
    content = function(file) {
        # if (fext(file) == "svg") svg(file=file, width = psize(width), height = psize(height))
        # if (fext(file) == "pdf") pdf(file=file, width = psize(width), height = psize(height))

        ## needed to get the image quality at the same level as shiny
        pr <- session$clientData$pixelratio
        if (is.null(pr) || pr < 1) pr <- 1
        png(file=file, width = width*pr, height = height*pr, res=72*pr)
          print(get(paste0(pre, plot_name))())
        dev.off()
    }
  )
  downloadLink(lnm, "", class = "fa fa-download alignright")
}

stat_tab_panel <- function(menu, tool, tool_ui, output_panels,
                           data = input$dataset) {
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML(paste("<label><strong>Menu:", menu, "</strong></label><br>")),
        HTML(paste("<label><strong>Tool:", tool, "</strong></label><br>")),
        if (!is.null(data))
          HTML(paste("<label><strong>Data:", data, "</strong></label>"))
      ),
      uiOutput(tool_ui)
    ),
    mainPanel(
      output_panels
    )
  )
}

################################################################
## functions used for app help
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
           <i title='Help' class='fa fa-question' data-toggle='modal' data-target='#%s'></i>",
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
           <i title='Help' class='fa fa-question alignleft' data-toggle='modal' data-target='#%s_help'></i>
           <i title='Report results' class='fa fa-edit action-button shiny-bound-input alignright' href='#%s_report' id='%s_report'></i>
           <div style='clear: both;'></div>",
          fun_name, fun_name, fun_name, modal_title, help_file, fun_name, fun_name, fun_name) %>%
  enc2utf8 %>% HTML %>% withMathJax
}

## function to render .md files to html
inclMD <- function(path) {
  markdown::markdownToHTML(path, fragment.only = TRUE, options = "",
                           stylesheet = "")
}

inclRmd <- function(path, r_env = parent.frame()) {
  paste(readLines(path, warn = FALSE), collapse = '\n') %>%
  knitr::knit2html(text = ., fragment.only = TRUE, quiet = TRUE,
    envir = r_env, options = "", stylesheet = "") %>%
    # gsub("&lt;!--/html_preserve--&gt;","",.) %>%  ## knitr adds this
    # gsub("&lt;!--html_preserve--&gt;","",.) %>%   ## knitr adds this
    HTML %>% withMathJax
}

## used by View - remove or use more broadly
find_env <- function(dataset) {
  if (exists("r_env")) {
    r_env
  } else if (exists("r_data") && !is.null(r_data[[dataset]])) {
    pryr::where("r_data")
  } else if (exists(dataset)) {
    pryr::where(dataset)
  }
}

## used by View - remove or use more broadly
save2env <- function(dat, dataset,
                     dat_name = dataset,
                     mess = "") {

  env <- find_env(dataset)
  env$r_data[[dat_name]] <- dat
  if (dataset != dat_name) {
    cat(paste0("Dataset r_data$", dat_name, " created in ", environmentName(env), " environment\n"))
    env$r_data[['datasetlist']] <- c(dat_name, env$r_data[['datasetlist']]) %>% unique
  } else {
    cat(paste0("Dataset r_data$", dataset, " changed in ", environmentName(env), " environment\n"))
  }

  ## set to previous description
  env$r_data[[paste0(dat_name,"_descr")]] <- env$r_data[[paste0(dataset,"_descr")]]

  if (mess != "")
    env$r_data[[paste0(dat_name,"_descr")]] %<>% paste0("\n\n",mess)
}

## cat to file
## use with tail -f ~/r_cat.txt in a terminal
cf <- function(...) {
  cat(paste0("\n--- called from: ", environmentName(parent.frame()), " (", lubridate::now(), ")\n"), file = "~/r_cat.txt", append = TRUE)
  out <- paste0(capture.output(...), collapse = "\n")
  cat("--\n", out, "\n--", sep = "\n", file = "~/r_cat.txt", append = TRUE)
}
