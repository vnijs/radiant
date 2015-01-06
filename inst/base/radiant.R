################################################################################
# functions to set initial values and take information from state_list
# when available
################################################################################

# using a function here so it can also be called from quit.R to reset the app
setInitValues <- function() {
  # initialize state list and reactive values
  pth <- "~/radiant_temp/state"
  filename = paste0(pth,"/RadiantState-",Sys.Date(),".rsf")
  if(file.exists(filename)) {
    load(filename)
    if(exists("RadiantValues")) values <<- do.call(reactiveValues, RadiantValues)
    if(exists("RadiantInputs")) state_list <<- RadiantInputs
    backup_loaded <<- TRUE
  } else {

    backup_loaded <<- FALSE
    state_list <<- list()
    values <<- reactiveValues()

    # initial plot height and width
    values$plotHeight <- 650
    values$plotWidth <- 650

    # Datasets can change over time (i.e. the changedata function). Therefore,
    # the data need to be a reactive value so the other reactive functions
    # and outputs that depend on these datasets will know when they are changed.
    robj <- load("../base/data/data_init/diamonds.rda")
    df <- get(robj)
    values[["diamonds"]] <- df
    values[["diamonds_descr"]] <- attr(df,'description')
    values$datasetlist <- c("diamonds")
  }
}

# setup reactiveValues object to store data
setInitValues()

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
  	return(values[[input$datasets]][,addColName] <- addCol)
	}
}

changedata_names <- function(oldnames, newnames) {
	upnames <- colnames(values[[input$datasets]])
	upnames[which(upnames %in% oldnames)] <- newnames
	return(colnames(values[[input$datasets]]) <- upnames)
}

inChecker <- function(tocheck) {
	ifelse(sum(tocheck %in% varnames()) < length(tocheck), return(NULL), return('OK'))
}

getdata <- reactive({
	values[[input$datasets]]
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

isSomeDate <- function(x) is.Date(x) | is.POSIXct(x) | is.POSIXt(x)
d2c <- function(x) ifelse(isSomeDate(x),return(as.character(x)),return(x))

show_data_snippet <- function(dat = input$datasets, nshow = 5, title = "") {

  if(title != "") p
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

		get(plot_name)()
	}, width=get(widthFun), height=get(heightFun))

  return(tabsetPanel(
    id = paste0("tabs_",fun_label),
    tabPanel("Summary", verbatimTextOutput(sum_name)),
    tabPanel("Plots", plotOutput(plot_name, height = "100%"))
  ))
}

onePanel <- function(fun_name, rfun_label, fun_label, widthFun, heightFun) {

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
	        HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
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
helpModal <- function(title, link, content) {
  sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
          <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
          <h3>%s</h3>
          </div>
          <div class='modal-body'>%s</div></div>
          <a title='Help' data-toggle='modal' href='#%s' class='icon-question-sign'></a>",
          link, title, content, link) %>%
  enc2utf8 %>% HTML %>% withMathJax
}

helpAndReport <- function(title, link, content) {
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

inclMD <- function(path)
  return(markdown::markdownToHTML(path, options = c(""), stylesheet="../base/www/empty.css"))

inclRmd <- function(path) {
  # function to render .Rmd files to html on-the-fly
  paste(readLines(path, warn = FALSE), collapse = '\n') %>%
  # do not embed image or add css
  knitr::knit2html(text = ., fragment.only = TRUE, options = "", stylesheet = "../base/www/empty.css")
}

