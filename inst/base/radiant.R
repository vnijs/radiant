################################################################################
# functions to set initial values and take information from r_state
# when available
#
# Note: puting functions in R/radiant.R produces
# Error in eval(expr, envir, enclos) : object 'r_state' not found
# because exported functions cannot access variables in the environment
# created by shinyServer
################################################################################

#' Set initial selection for shiny input (e.g., selectInput for multiple = FALSE)
state_singlevar <- function(inputvar, vars)
  vars[vars == r_state[[inputvar]]]
  # ifelse(exists("r_state"), vars[vars == r_state[[inputvar]]], c())

#' Set initial selection for shiny input (e.g., selectInput for multiple = TRUE)
state_multvar <- function(inputvar, vars)
  vars[vars %in% r_state[[inputvar]]]

  # adding the following condition causes an error in Visualize and others
  # ifelse(exists("r_state"), vars[vars %in% r_state[[inputvar]]], c())
  # Error in ifelse(exists("r_state"), vars[vars %in% r_state[[inputvar]]],  :
  # replacement has length zero

#' Set initial value for shiny input
state_init <- function(inputvar, init = "") {
  # if(!exists("r_state")) return(init)
  ifelse(r_state[[inputvar]] %>% is.null, return(init),
         return(r_state[[inputvar]]))
}


# r_state <- list()
# state_init("test")
# state_init("test",0)
# r_state$test <- 8
# state_init("test",0)

#' Set initial value for shiny input from a list of values
state_init_list <- function(inputvar, init, vals) {
  ifelse(r_state[[inputvar]] %>% is.null, return(init),
         return(state_singlevar(inputvar, vals)))
}

# r_state <- list()
# state_init_list("test",1,1:10)
# r_state$test <- 8
# state_init_list("test",1,1:10)

#' Set initial values for variable selection from a prior analysis
state_init_multvar <- function(inputvar, pre_inputvar, vals) {
  # Data > View does not select all variables unless you use return
  # inside ifelse
  ifelse(r_state[[inputvar]] %>% is.null, return(vals[vals %in% pre_inputvar]),
         return(state_multvar(inputvar, vals)))

}

# r_state <- list()
# state_init_multvar("test",letters[1:5],letters)
# r_state$test <- letters[6:10]
# state_init_multvar("test",letters[1:5],letters)

################################################################################
# function to save app state on refresh or crash
################################################################################
saveStateOnRefresh <- function(session = session) {
  session$onSessionEnded(function() {
    # print("Session ended")
    isolate({
      # if(input$resetState %>% not_pressed &&
      #    input$quitApp %>% not_pressed &&
      if(not_pressed(input$resetState) &&
         not_pressed(input$quitApp) &&
         is.null(input$uploadState)) {

        # RadiantInputs <- r_state
        # LiveInputs <- reactiveValuesToList(input)
        # RadiantInputs[names(LiveInputs)] <- LiveInputs

        # assign(paste0("RadiantInputs",ip), reactiveValuesToList(input),
        assign(ip_inputs, reactiveValuesToList(input),
               envir = .GlobalEnv)
        # assign(paste0("RadiantValues",ip), reactiveValuesToList(r_data),
        assign(ip_data, reactiveValuesToList(r_data),
               envir = .GlobalEnv)
        assign(ip_dump, now(), envir = .GlobalEnv)
      }
    })
  })
}

################################################################
# functions used across tools in radiant
################################################################
changedata <- function(new_col, new_col_name = "", dataset = input$dataset) {
	if(r_data[[dataset]] %>% nrow == new_col %>% nrow &&
     new_col_name[1] != "")
    r_data[[dataset]][,new_col_name] <- new_col
}

changedata_names <- function(oldnames, newnames)
  r_data[[input$dataset]] %<>% rename_(.dots = setNames(oldnames, newnames))

getdata <- reactive({

  if(input$data_filter %>% is_empty) return(r_data[[input$dataset]])

  selcom <- gsub("\\s","", input$data_filter)
  if(selcom != "") {
    seldat <- try(filter_(r_data[[input$dataset]], selcom), silent = TRUE)

    if(is(seldat, 'try-error')) {
      isolate(r_data$error <- attr(seldat,"condition")$message)
    } else {
      isolate(r_data$error <- "")
      return(seldat)
    }
  } else {
    isolate(r_data$error <- "")
  }

  r_data[[input$dataset]]
})

getdata_class <- reactive({
  # r_data[[input$dataset]][1,,drop = FALSE] %>% getdata_class_fun
  r_data[[input$dataset]] %>% getdata_class_fun
})

getdata_class_fun <- function(dat) {
  sapply(dat, function(x) class(x)[1]) %>%
	  gsub("ordered","factor", .) %>%
	  gsub("POSIXct","date", .) %>%
	  gsub("POSIXt","date", .)
}

groupable_vars <- reactive({
  # isGroupable <- getdata_class() %in% c("factor","integer","character")
    # select(which(isGroupable)) %>%
  getdata() %>%
    summarise_each(funs(n_distinct)) %>%
    is_less_than(10) %>%
    which  %>%
    varnames()[.]
})

varnames <- reactive({
  getdata_class() %>% names %>%
    set_names(., paste0(., " {", getdata_class(), "}"))
})

# check if a variable is null or not in the data
not_available <- function(x)
  ifelse(any(is.null(x)) || (sum(x %in% varnames()) < length(x)), TRUE, FALSE)

# check if a button was NOT pressed
not_pressed <- function(x) ifelse(is.null(x) || x == 0, TRUE, FALSE)

# check if string variable is defined
is_empty <- function(x, empty = "") ifelse(is.null(x) || x == "", TRUE, FALSE)

# check for duplicate entries
has_duplicates <- function(x)
  ifelse(length(x %>% unique) < length(x), TRUE, FALSE)

# is x some type of date variable
is_date <- function(x) is.Date(x) | is.POSIXct(x) | is.POSIXt(x)

# convert a date variable to character for printing
d2c <- function(x) if(x %>% is_date) { x %>% as.character } else { x }

# show a few rows of a dataframe
show_data_snippet <- function(dat = input$dataset, nshow = 5, title = "") {

  if(is.character(dat) && length(dat) == 1) dat <- r_data[[dat]]
  dat %>%
    slice(1:min(nshow,nrow(.))) %>%
    mutate_each(funs(d2c)) %>%
    xtable::xtable(.) %>%
    print(type='html',  print.results = FALSE) %>%
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
plotWidth <- function() {
	ifelse(is.null(input$viz_plot_width), return(r_data$plotWidth),
		return(input$viz_plot_width))
}

plotHeight <- function() {
	ifelse(is.null(input$viz_plot_height), return(r_data$plotHeight),
		return(input$viz_plot_height))
}

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

onePanel <- function(fun_name, rfun_label, fun_label, widthFun, heightFun) {

  #
  # to be deprecated if register_..._output functions work as expected
  #

	if(isolate(input$nav_radiant) != fun_name) return()

	main_name <- paste0("main_", fun_label)

	# Generate output for the summary tab
	output[[main_name]] <- renderPrint({

		result <- get(rfun_label)()
		# when no analysis was conducted (e.g., no variables selected)
		if(is.character(result)) return(cat(result,"\n"))

		get(main_name)()
	})

  return(tabsetPanel(
    id = paste0("tabs_",fun_label),
    tabPanel("Main", verbatimTextOutput(main_name))
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

# fun_name is a string of the main function name
# rfun_name is a string of the reactive wrapper that calls the main function
# out_name is the name of the output, set to fun_name by default
register_plot_output <- function(fun_name, rfun_name,
                                 out_name = fun_name,
                                 width_fun = "plotWidth",
                                 height_fun = "plotHeight") {

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

returnTextAreaInput <- function(inputId, label, value = "") {
  tagList(
    tags$label(label, `for` = inputId),br(),
    tags$textarea(id=inputId, type = "text", value = value, rows="3",
                  class="returnTextArea form-control")
  )
}

# create html for sortable list of variables or factor levels
html_list <- function(vars, id) {
  hl <- paste0("<ul id=\'",id,"\' class='stab'>")
  for(i in vars) hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
  paste0(hl, "</ul>")
}

# binding for a sortable list of variables or factor levels
returnOrder <- function(inputId, vars) {
  tagList(
    includeScript("../base/www/js/sort.js"),
    includeCSS("../base/www/sort.css"),
    HTML(html_list(vars, inputId)),
    tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
  )
}

# binding to a bootstrap popover, function by Joe Cheng https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'),
  trigger=c('click', 'hover', 'focus', 'manual')) {

  tagList(
    singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
    tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="glyphicon-question-sign"))
  )
}

helpModal <- function(modal_title, link, help_file) {
  sprintf("<div class='modal fade' id='%s' tabindex='-1' role='dialog' aria-labelledby='%s_label' aria-hidden='true'>
            <div class='modal-dialog'>
              <div class='modal-content'>
                <div class='modal-header'>
                  <button type='button' class='close' data-dismiss='modal' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
                  <h4 class='modal-title' id='%s_label'>%s</h4>
                  </div>
                <div class='modal-body'>%s</div>
              </div>
            </div>
           </div>
           <i title='Help' class='glyphicon glyphicon-question-sign' data-toggle='modal' data-target='#%s'></i>",
           link, link, link, modal_title, help_file, link) %>%
  enc2utf8 %>% HTML
}

helpAndReport <- function(title, link, content) {
  sprintf("<div class='modal fade' id='%sHelp' tabindex='-1' role='dialog' aria-labelledby='%sHelp_label' aria-hidden='true'>
            <div class='modal-dialog'>
              <div class='modal-content'>
                <div class='modal-header'>
                  <button type='button' class='close' data-dismiss='modal' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
                  <h4 class='modal-title' id='%sHelp_label'>%s</h4>
                  </div>
                <div class='modal-body'>%s</div>
              </div>
            </div>
           </div>
           <i title='Help' class='glyphicon glyphicon-question-sign alignleft' data-toggle='modal' data-target='#%sHelp'></i>
           <i title='Report results' class='glyphicon glyphicon-book action-button shiny-bound-input alignright' href='#%sReport' id='%sReport'></i>
           <div style='clear: both;'></div>
           <script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>",
          link, link, link, title, content, link, link, link) %>%
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
                <div class='modal-body'>%s</div>
              </div>
            </div>
           </div>
           <i title='Help' class='glyphicon glyphicon-question-sign alignleft' data-toggle='modal' data-target='#%s_help'></i>
           <i title='Report results' class='glyphicon glyphicon-book action-button shiny-bound-input alignright' href='#%s_report' id='%s_report'></i>
           <div style='clear: both;'></div>
           <script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>",
          fun_name, fun_name, fun_name, modal_title, help_file, fun_name, fun_name, fun_name) %>%
  enc2utf8 %>% HTML
}

# function to render .md files to html
inclMD <- function(path) {
  markdown::markdownToHTML(path, fragment.only = TRUE, options = c(""),
                           stylesheet="../base/www/empty.css")
}

# function to render .Rmd files to html
# does not embed image or add css
inclRmd <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = '\n') %>%
  knitr::knit2html(text = ., fragment.only = TRUE, options = "",
                   stylesheet = "../base/www/empty.css")
}
