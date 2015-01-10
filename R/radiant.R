################################################################################
# functions to set initial values and take information from state_list
# when available
################################################################################

state_singlevar <- function(inputvar, vars) vars[vars == state_list[[inputvar]]]
state_multvar <- function(inputvar, vars) vars[vars %in% state_list[[inputvar]]]

state_init <- function(inputvar, init = "") {
  ifelse(is.null(state_list[[inputvar]]), return(init), return(state_list[[inputvar]]))
}

state_init_list <- function(inputvar, init = "", vals) {
  ifelse(is.null(state_list[[inputvar]]), return(init), return(state_singlevar(inputvar, vals)))
}

state_init_multvar <- function(inputvar, pre_inputvar, vals) {
  # for factor and cluster use variable selection from the pre-analysis
  ifelse(is.null(state_list[[inputvar]]), return(vals[vals %in% pre_inputvar]),
    return(state_multvar(inputvar, vals)))
}

################################################################################
# function to save app state on refresh or crash
################################################################################
saveState <- function(filename) {
  isolate({
    RadiantInputs <- state_list
    LiveInputs <- reactiveValuesToList(input)
    RadiantInputs[names(LiveInputs)] <- LiveInputs
    RadiantValues <- reactiveValuesToList(values)
    save(RadiantInputs, RadiantValues , file = filename)
  })
}

saveStateOnCrash <- function(session = session)
  session$onSessionEnded(function() {
    observe({
      pth <- normalizePath("~/radiant_temp/state",winslash="/")
      cdir <- ""
      if(!file.exists(pth))
        cdir <- try(dir.create(pth), silent = TRUE)
      if(!is(cdir, 'try-error')) try(saveState(paste0(pth,"/RadiantState-",Sys.Date(),".rsf")), silent = TRUE)
   })
})


################################################################
# functions used across tools in radiant
################################################################
changedata <- function(addCol, addColName = "") {
	if(nrow(getdata()) == nrow(addCol) && addColName[1] != "") {
  	return(values[[input$dataset]][,addColName] <- addCol)
	}
}

changedata_names <- function(oldnames, newnames) {
	upnames <- colnames(values[[input$dataset]])
	upnames[which(upnames %in% oldnames)] <- newnames
	return(colnames(values[[input$dataset]]) <- upnames)
}

getdata <- reactive({
	values[[input$dataset]]
})

getdata_class <- reactive({
	# don't use isolate here or values won't change when the dataset is changed
  getdata() %>% getdata_class_fun
})

getdata_class_fun <- function(dat = getdata()) {
  sapply(dat, function(x) class(x)[1]) %>%
	  gsub("ordered","factor", .) %>%
	  gsub("POSIXct","date", .) %>%
	  gsub("POSIXt","date", .)
}

varnames <- reactive({
	dat <- getdata_class()
	vars <- names(dat)
	names(vars) <- paste(vars, " {", dat, "}", sep = "")
	vars
})

# are all variables in the currently selected dataset
# deprecate in favor of not_available
inChecker <- function(x)
  ifelse(sum(x %in% varnames()) < length(x), NULL, return('OK'))

# check if a variable is null or not in the data
not_available <- function(x)
  ifelse(is.null(x) || (sum(x %in% varnames()) < length(x)), TRUE, FALSE)

# check if a button was NOT pressed
not_pressed <- function(x) ifelse(is.null(x) || x == 0, TRUE, FALSE)

# is x some type of date variable
isSomeDate <- function(x) is.Date(x) | is.POSIXct(x) | is.POSIXt(x)

# convert a date variable to character for printing
d2c <- function(x) ifelse(isSomeDate(x),return(as.character(x)),return(x))

# show a few rows of a dataframe
show_data_snippet <- function(dat = input$dataset, nshow = 5, title = "") {

  # not sure what happend to this next line
  # leaving line for now (1/7/2015). remove if no issues pop up
  # if(title != "") p

  if(is.character(dat) && length(dat) == 1) dat <- values[[dat]]
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

################################################################
# functions used to create Shiny in and outputs
################################################################
plotWidth <- function() {
	ifelse(is.null(input$viz_plot_width), return(values$plotWidth),
		return(input$viz_plot_width))
}

plotHeight <- function() {
	ifelse(is.null(input$viz_plot_height), return(values$plotHeight),
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

    # not passing in result because this will be picked up by default
    get(fun_name)()
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
      # not passing in result because this should be picked up by default
      get(fun_name)()
    })
  }, width=get(width_fun), height=get(height_fun))

}

# the 1/2015 version
statTabPanel2 <- function(menu, tool, tool_ui, output_panels,
                          data = input$dataset) {
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML(paste("<label><strong>Menu:",menu,"</strong></label>")),
        HTML(paste("<label><strong>Tool:",tool,"</strong></label>")),
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
        HTML(paste("<label><strong>Menu:",menu_name,"</strong></label>")),
        HTML(paste("<label><strong>Tool:",tool,"</strong></label>")),
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
    includeScript("../base/www/js/returnTextInputBinding.js"),
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class = "returnTextInput")
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
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="icon-question-sign"))
  )
}

# binding to a bootstrap modal
helpModal <- function(modal_title, link, help_file) {
  sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
          <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
          <h3>%s</h3>
          </div>
          <div class='modal-body'>%s</div></div>
          <a title='Help' data-toggle='modal' href='#%s' class='icon-question-sign'></a>",
          link, modal_title, help_file, link) %>%
  enc2utf8 %>% HTML %>% withMathJax
}

helpAndReport <- function(title, link, content) {
  # deprecate in favor of help_and_report when possible
  sprintf("<div id='%sHelp' class='modal hide fade in' style='display: none; '>
          <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
          <h3>%s</h3></div>
          <div class='modal-body'>%s</div></div>
          <div><a title='Help' data-toggle='modal' href='#%sHelp' class='icon-question-sign alignleft'></a>
          <a title='Report results' class='icon-book action-button shiny-bound-input alignright' href='#%sReport' id='%sReport'></a>
          </div><div style='clear: both;'></div>",
          link, title, content, link, link, link) %>%
  enc2utf8 %>% HTML %>% withMathJax
}

help_and_report <- function(modal_title, fun_name, help_file) {
  sprintf("<div id='%s_help' class='modal hide fade in' style='display: none; '>
          <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
          <h3>%s</h3></div>
          <div class='modal-body'>%s</div></div>
          <div><a title='Help' data-toggle='modal' href='#%s_help' class='icon-question-sign alignleft'></a>
          <a title='Report results' class='icon-book action-button shiny-bound-input alignright' href='#%s_report' id='%s_report'></a>
          </div><div style='clear: both;'></div>",
          fun_name, modal_title, help_file, fun_name, fun_name, fun_name) %>%
  enc2utf8 %>% HTML %>% withMathJax
}


# function to render .md files to html
inclMD <- function(path)
  markdown::markdownToHTML(path, options = c(""),
                           stylesheet="../base/www/empty.css")

# function to render .Rmd files to html
# does not embed image or add css
inclRmd <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = '\n') %>%
  knitr::knit2html(text = ., fragment.only = TRUE, options = "",
                   stylesheet = "../base/www/empty.css")
}
