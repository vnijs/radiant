################################################################################
# functions to set initial values and take information from r_state
# when available
#
# Note: putting functions in R/radiant.R produces
# Error in eval(expr, envir, enclos) : object 'r_state' not found
# because exported functions cannot access variables in the environment
# created by shinyServer
################################################################################

observe({
  # reset r_state on dataset change ... when you are not on the
  # Manage > Data tab
  if(is.null(r_state$dataset) || is.null(input$dataset)) return()
  if(input$datatabs != "Manage" || input$nav_radiant != "Data")
    if(r_state$dataset != input$dataset) r_state <<- list()
})

## Can't export the state_... function through R/radiant.R
## Error in checkboxGroupInput("help_data", NULL, help_data, selected = state_init_list("help_data",  :
##  could not find function "state_init"

# Set initial value for shiny input (e.g., radio button or checkbox)
state_init <- function(inputvar, init = "")
  if(is.null(r_state[[inputvar]])) init else r_state[[inputvar]]

# library(dplyr)
# r_state <- list()
# state_init("test")
# state_init("test",0)
# r_state$test <- c("a","b")
# state_init("test",0)

# Set initial value for shiny input from a list of values
state_single <- function(inputvar, vals, init = character(0))
  if(is.null(r_state[[inputvar]])) init else vals[vals == r_state[[inputvar]]]

# library(dplyr)
# r_state <- list()
# state_single("test",1,1:10)
# r_state$test <- 8
# state_single("test",1,1:10)
# state_single("test",1,1:5)

# Set initial values for variable selection (e.g., selection used in another analysis)
state_multiple <- function(inputvar, vals, init = character(0)) {
  if(is.null(r_state[[inputvar]]))
    # "a" %in% character(0) --> FALSE, letters[FALSE] --> character(0)
    vals[vals %in% init]
  else
    vals[vals %in% r_state[[inputvar]]]
}

################################################################################
# function to save app state on refresh or crash
################################################################################
saveStateOnRefresh <- function(session = session) {
  session$onSessionEnded(function() {
    isolate({
      if(not_pressed(input$resetState) &&
         not_pressed(input$quitApp) &&
         is.null(input$uploadState)) {

        assign(ip_inputs, reactiveValuesToList(input), envir = .GlobalEnv)
        assign(ip_data, reactiveValuesToList(r_data), envir = .GlobalEnv)
        assign(ip_dump, now(), envir = .GlobalEnv)
        if(running_local) rm(r_env, envir = .GlobalEnv)
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

# .changedata <- changedata

# changedata_names <- function(oldnames, newnames)
#   r_data[[input$dataset]] %<>% rename_(.dots = setNames(oldnames, newnames))

.getdata <- reactive({

  if(input$data_filter %>% is_empty | input$show_filter == FALSE) return(r_data[[input$dataset]])
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

# .getdata <- getdata

getdata_class <- reactive({
  # r_data[[input$dataset]][1,,drop = FALSE] %>% getdata_class_fun
  r_data[[input$dataset]] %>% getdata_class_fun
})

getdata_class_fun <- function(dat) {
  sapply(dat, function(x) class(x)[1]) %>%
	  gsub("ordered","factor", .) %>%
	  gsub("POSIXct","date", .) %>%
    gsub("POSIXct","date", .) %>%
	  gsub("Date","date", .)
}

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
  getdata_class() %>% names %>%
    set_names(., paste0(., " {", getdata_class(), "}"))
})

# cleaning up the arguments for data_filter passed to report
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
    if(rep_args[[i]][1] == rep_default[[i]]) rep_args[[i]] <- NULL
  rep_args
}

# check if a variable is null or not in the selected data.frame
not_available <- function(x)
  if(any(is.null(x)) || (sum(x %in% varnames()) < length(x))) TRUE else FALSE

# check if a button was NOT pressed
not_pressed <- function(x) if(is.null(x) || x == 0) TRUE else FALSE

# check if string variable is defined
is_empty <- function(x, empty = "") if(is.null(x) || x == empty) TRUE else FALSE

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

################################################################
# functions used to create Shiny in and outputs
################################################################
plot_width <- function()
  if(is.null(input$viz_plot_width)) r_data$plotWidth else input$viz_plot_width

# if(input$viz_plot_width %>% not_available) r_data$plotWidth else input$viz_plot_width

plotWidth <- plot_width

plot_height <- function()
  if(is.null(input$viz_plot_height)) r_data$plotHeight else input$viz_plot_height

# if(input$viz_plot_height %>% not_available) r_data$plotHeight else input$viz_plot_height

plotHeight <- plot_height

twoPanels <- function(fun_name, rfun_label, fun_label, widthFun, heightFun) {
  #
  # to be deprecated if register_..._output functions work as expected
  #
  if(isolate(input$nav_radiant) != fun_name) return()

  sum_name <- paste0("summary_", fun_label)
  plot_name <- paste0("plots_", fun_label)

  # Generate output for the summary tab
  output[[sum_name]] <- renderPrint({

    result <- get(rfun_label)()
    # when no analysis was conducted (e.g., no variables selected)
    if(is.character(result)) return(cat(result,"\n"))
    get(sum_name)()
  })

  # Generate output for the plots tab
  output[[plot_name]] <- renderPlot({
    result <- get(rfun_label)()
    # when no analysis was conducted (e.g., no variables selected)
    if(is.character(result))
      return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))

    withProgress(message = 'Making plot', value = 0, {
      get(plot_name)()
    })

  }, width=get(widthFun), height=get(heightFun))

  return(tabsetPanel(
    id = paste0("tabs_",fun_label),
    tabPanel("Summary", verbatimTextOutput(sum_name)),
    tabPanel("Plots", plotOutput(plot_name, height = "100%"))
  ))
}

# fun_name is a string of the main function name
# rfun_name is a string of the reactive wrapper that calls the main function
# out_name is the name of the output, set to fun_name by default
register_print_output <- function(fun_name, rfun_name, out_name = fun_name) {

  # Generate output for the summary tab
  output[[out_name]] <- renderPrint({

    # needs to be inside output
    result <- get(rfun_name)()

    # when no analysis was conducted (e.g., no variables selected)
    if(is.character(result)) return(cat(result,"\n"))
    # get(fun_name)(result)
    summary(result)
  })
}

register_print_output2 <- function(fun_name, rfun_name, out_name = fun_name) {

  # Generate output for the summary tab
  output[[out_name]] <- renderPrint({
    # when no analysis was conducted (e.g., no variables selected)
    get(rfun_name)() %>%
    { if(is.character(.)) cat(.,"\n") else . } %>% rm

  })
}

register_plot_output2 <- function(fun_name, rfun_name,
                                 out_name = fun_name,
                                 width_fun = "plot_width",
                                 height_fun = "plot_height") {

  # Generate output for the plots tab
  output[[out_name]] <- renderPlot({

    # when no analysis was conducted (e.g., no variables selected)
    get(rfun_name)() %>%
    { if(is.character(.)) {
        plot(x = 1, type = 'n', main= . , axes = FALSE, xlab = "", ylab = "")
      } else {
        withProgress(message = 'Making plot', value = 0, { . })
      }
    }
  }, width=get(width_fun), height=get(height_fun))
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

    # needs to be inside output
    result <- get(rfun_name)()

    # when no analysis was conducted (e.g., no variables selected)
    if(is.character(result))
      return(plot(x = 1, type = 'n', main=result,
             axes = FALSE, xlab = "", ylab = ""))

    withProgress(message = 'Making plot', value = 0, {
      # get(fun_name)(result)
      plot(result)
    })
  }, width=get(width_fun), height=get(height_fun))

}

# the 1/2015 version
statTabPanel2 <- function(menu, tool, tool_ui, output_panels,
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

# menu_name - radiant menu from navbar
# fun_name - radiant menu from navbar
statTabPanel <- function(menu_name, fun_name, rfun_label, fun_label,
                         widthFun = "plotWidth", heightFun = "plotHeight",
                         mpan = "twoPanels") {

  #
  # to be deprecated in favor of statTabPanel2
  #

  tool <- isolate(input$nav_radiant)
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML(paste("<label><strong>Menu:",menu_name,"</strong></label><br>")),
        HTML(paste("<label><strong>Tool:",tool,"</strong></label><br>")),
        if(!tool %in% c("Central Limit Theorem", "Sample size", "Create profiles"))
	        HTML(paste("<label><strong>Data:",input$dataset,"</strong></label>"))
      ),
      uiOutput(paste0("ui_",fun_label))
    ),
    mainPanel(
			# statPanel(fun_name, rfun_label, fun_label, widthFun, heightFun)
			# get("statPanel")(fun_name, rfun_label, fun_label, widthFun, heightFun)
			get(mpan)(fun_name, rfun_label, fun_label, widthFun, heightFun)
    )
  )
}

################################################################
# various other functions
################################################################

# binding for a text input that updates when the return key is pressed
returnTextInput <- function(inputId, label, value = "") {
  tagList(
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value,
               class = "returnTextInput form-control")
  )
}

returnTextAreaInput <- function(inputId, label = NULL, value = "") {
  tagList(
    tags$label(label, `for` = inputId),br(),
    tags$textarea(id=inputId, type = "text", rows="2",
                  class="returnTextArea form-control", value)
  )
}

# create html for sortable list of variables or factor levels
# html_list <- function(vars, id) {
#   hl <- paste0("<ul id=\'",id,"\' class='stab'>")
#   for(i in vars) hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
#   paste0(hl, "</ul>")
# }

# binding for a sortable list of variables or factor levels
# returnOrder <- function(inputId, vars) {
#   tagList(
#     includeScript("../base/www/js/sort.js"),
#     includeCSS("../base/www/sort.css"),
#     HTML(html_list(vars, inputId)),
#     tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
#   )
# }

# binding to a bootstrap popover, function by Joe Cheng https://gist.github.com/jcheng5/5913297
# helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'),
#   trigger=c('click', 'hover', 'focus', 'manual')) {

#   tagList(
#     singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
#     tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
#       `data-placement` = match.arg(placement, several.ok=TRUE)[1],
#       `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="glyphicon-question-sign"))
#   )
# }

helpModal <- function(modal_title, link, help_file) {
  #
  # to be deprecated in favor of ... help_and_report
  #
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

helpAndReport <- function(title, link, content) {
  #
  # to be deprecated in favor of ... help_and_report
  #
  sprintf("<div class='modal fade' id='%sHelp' tabindex='-1' role='dialog' aria-labelledby='%sHelp_label' aria-hidden='true'>
            <div class='modal-dialog'>
              <div class='modal-content'>
                <div class='modal-header'>
                  <button type='button' class='close' data-dismiss='modal' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
                  <h4 class='modal-title' id='%sHelp_label'>%s</h4>
                </div>
                <div class='modal-body'>%s<br>
                  &copy; Vincent Nijs (2015) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/80x15.png' /></a>
                </div>
              </div>
            </div>
           </div>
           <i title='Help' class='glyphicon glyphicon-question-sign alignleft' data-toggle='modal' data-target='#%sHelp'></i>
           <i title='Report results' class='glyphicon glyphicon-book action-button shiny-bound-input alignright' href='#%sReport' id='%sReport'></i>
           <div style='clear: both;'></div>",
          link, link, link, title, content, link, link, link) %>%
  enc2utf8 %>% HTML %>% withMathJax()
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
#
# to be deprecated when all help files are in html
#
inclMD <- function(path) {
  markdown::markdownToHTML(path, fragment.only = TRUE, options = c(""),
                           stylesheet="../base/www/empty.css")
}

# function to render .Rmd files to html - does not embed image or add css
inclRmd <- function(path) {
#
# to be deprecated when all help files are in html
#
  paste(readLines(path, warn = FALSE), collapse = '\n') %>%
  knitr::knit2html(text = ., fragment.only = TRUE, options = "",
                   stylesheet = "../base/www/empty.css")
}
