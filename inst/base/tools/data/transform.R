# UI-elements for transform
output$ui_tr_vars <- renderUI({
  vars <- varnames()
  selectInput("tr_vars", "Select variable(s):", choices  = vars,
    selected = state_multiple("tr_vars", vars),
    multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
})

output$ui_tr_reorg_vars <- renderUI({
  vars <- varnames()
  selectizeInput("tr_reorg_vars", "Reorder/remove variables:", choices  = vars,
    selected = vars, multiple = TRUE,
    options = list(placeholder = 'Select variable(s)',
                   plugins = list('remove_button', 'drag_drop')))
})

output$ui_tr_normalizer <- renderUI({
  isNum <- "numeric" == .getclass() | "integer" == .getclass()
  vars <- varnames()[isNum]
  if (length(vars) == 0) return(NULL)
  selectInput("tr_normalizer", "Normalizing variable:", c("None" = "none", vars),
              selected = "none")
})

output$ui_tr_reorg_levs <- renderUI({
	if (input$tr_vars %>% not_available) return()
  fctCol <- input$tr_vars[1]
	isFct <- "factor" == .getclass()[fctCol]
  if (!isFct) return()
	.getdata()[,fctCol] %>% levels -> levs
  selectizeInput("tr_reorg_levs", "Reorder/remove levels:", choices  = levs,
    selected = levs, multiple = TRUE,
    options = list(placeholder = 'Select level(s)',
                   plugins = list('remove_button', 'drag_drop')))
})

# center variable
cent <- function(x)
	if (is.numeric(x)) { x - mean(x, na.rm = TRUE) } else x

# standardize variable
st <- function(x)
	if (is.numeric(x)) { cent(x) / sd_rm(x) } else x

# if (is.numeric(x)) scale(x) else x

# median split
msp <- function(x) cut(x, breaks = quantile(x,c(0,.5,1)),
                       include.lowest = TRUE,
                       labels = c("Below", "Above"))
# decile split
dec <- function(x) cut(x, breaks = quantile(x, seq(0,1,.1)),
                       include.lowest = TRUE,
                       labels = seq(1,10,1))

sq <- function(x) x^2
inv <- function(x) 1/x
normalize <- function(x,y) x/y

# use as.character in case x is a factor
d_mdy <- . %>% { if (is.factor(.)) as.character(.) else . } %>%
           lubridate::mdy(.) %>% as.Date
d_dmy <- . %>% { if (is.factor(.)) as.character(.) else . } %>%
           lubridate::dmy(.) %>% as.Date
d_ymd <- . %>% { if (is.factor(.)) as.character(.) else . } %>%
           lubridate::ymd(.) %>% as.Date

# test
# dat <- read.table(header = TRUE, text = "date	days
# 1/1/10	1
# 1/2/10	2
# 1/3/10	3
# 1/4/10	4
# 1/5/10	5
# 1/6/10	6
# 1/7/10	7
# 1/8/10	8
# 1/9/10	9
# 1/10/10	10")
# sapply(dat,class)
# library(lubridate)
# dat$date %>% d_mdy %T>% print %>% class
# dat$date %>% as.character
# dat$date %>% d_mdy %T>% print %>% class
# dat$date %>% as.factor
# dat$date %>% d_dmy %T>% print %>% class
# dat$date %>% as.character
# dat$date %>% d_dmy %T>% print %>% class

# http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html
d_ymd_hms <- . %>% { if (is.factor(.)) as.character(.) else . } %>%
               lubridate::ymd_hms(.)

as_int <- function(x) {
	if (is.factor(x)) {
		levels(x) %>% .[x] %>% as.integer
	} else {
		as.integer(x)
	}
}

as_num <- function(x) {
	if (is.factor(x)) {
		levels(x) %>% .[x] %>% as.numeric
	} else {
    as.numeric(x)
	}
}

# test
# library(magrittr)
# library(dplyr)
# x <- as.factor(rep(c('2','3'), 8))
# as.numeric(x)
# as.integer(x)
# as_num(x)
# as_int(x)
# end test

trans_options <- list("None" = "none", "Log" = "log", "Exp" = "exp",
                      "Square" = "sq", "Square-root" = "sqrt",
                      "Center" = "cent", "Standardize" = "st", "Invert" = "inv",
                      "Median split" = "msp", "Deciles" = "dec")

type_options <- list("None" = "none", "As factor" = "as.factor",
                     "As number" = "as_num", "As integer" = "as_int",
                     "As character" = "as.character", "As date (mdy)" = "d_mdy",
                     "As date (dmy)" = "d_dmy", "As date (ymd)" = "d_ymd",
                     "As date/time (ymd_hms)" = "d_ymd_hms")

trans_types <- list("None" = "none", "Type" = "type", "Change" = "change",
                    "Normalize" = "normalize", "Create" = "create",
                    "Clipboard" = "clip", "Recode" = "recode",
                    "Rename" = "rename", "Reorder/remove variables" = "reorg_vars",
                    "Reorder/remove levels" = "reorg_levs",
                    "Remove missing" = "remove_na",
                    "Save filtered data" = "save_filtered")

output$ui_Transform <- renderUI({
	# Inspired by Ian Fellow's transform ui in JGR/Deducer
  list(wellPanel(
    uiOutput("ui_tr_vars"),
    selectInput("tr_change_type", "Transformation type:", trans_types, selected = "none"),
    conditionalPanel(condition = "input.tr_change_type == 'type'",
	    selectInput("tr_typefunction", "Change variable type:", type_options, selected = "none")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'change'",
	    selectInput("tr_transfunction", "Apply function:", trans_options)
    ),
    conditionalPanel(condition = "input.tr_change_type == 'normalize'",
      uiOutput("ui_tr_normalizer")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'create'",
	    returnTextAreaInput("tr_transform", "Create (e.g., x = y - z):", '')
    ),
    conditionalPanel(condition = "input.tr_change_type == 'clip'",
    	HTML("<label>Paste from Excel:</label>"),
    	tags$textarea(class="form-control", id="tr_paste", rows=3, "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'recode'",
	    returnTextAreaInput("tr_recode", "Recode (e.g., lo:20 = 1):", '')
    ),
    conditionalPanel(condition = "input.tr_change_type == 'rename'",
      returnTextAreaInput("tr_rename", "Rename (separate by , ):", '')
    ),
    conditionalPanel(condition = "input.tr_change_type != ''",
      actionButton("tr_save_changes", "Save changes")
	  ),
    conditionalPanel(condition = "input.tr_change_type == 'reorg_vars'",
      br(), uiOutput("ui_tr_reorg_vars")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'reorg_levs'",
      br(), uiOutput("ui_tr_reorg_levs")
    ),
	  textInput("tr_dataset", "Save changes to:", input$dataset)),

    # help_and_report(modal_title = "Transform",
    #                 fun_name = "transform",
    #                 help_file = inclMD(file.path(r_path, "base/tools/help/transform.md")))
		help_modal("Transform", "transform_help",
		  inclMD(file.path(r_path,"base/tools/help/transform.md")))

	)
})

find_env <- function(dataset) {
	if (exists("r_env")) {
	  r_env
	} else if (exists("r_data") && !is.null(r_data[[dataset]])) {
		pryr::where("r_data")
	} else if (exists(dataset)) {
		pryr::where(dataset)
	}
}

save2env <- function(dat, dataset,
                     tr_dataset = dataset,
                     tr_message = "") {

	env <- find_env(dataset)
	env$r_data[[tr_dataset]] <- dat
  if(dataset != tr_dataset) {
  	cat(paste0("Dataset r_data$", tr_dataset, " created in ", environmentName(env), " environment\n"))
  	env$r_data[['datasetlist']] <- c(tr_dataset, env$r_data[['datasetlist']]) %>% unique
 	} else {
  	cat(paste0("Dataset r_data$", dataset, " change in ", environmentName(env), " environment\n"))
 	}

  if(tr_message != "")
  	env$r_data[[paste0(tr_dataset,"_descr")]] %<>% paste0("\n",tr_message)
}

reorg_vars <- function(dataset,
                       tr_reorg_vars = "",
                       tr_dataset = dataset,
                       data_filter = "",
                       tr_save = TRUE) {

	if (!tr_save || !is.character(tr_dataset))
		return(getdata(dataset, tr_reorg_vars, filt = data_filter, na.rm = FALSE))

	tr_message <-  if (tr_reorg_vars[1] != "") ""
	   						 else paste0("\nReorganized variables: ", paste0(tr_reorg_vars, collapse=", "), " (", lubridate::now(), ")")

	getdata(dataset, tr_reorg_vars, filt = data_filter, na.rm = FALSE) %>%
	  save2env(dataset, tr_dataset, tr_message)
}

# mtcars %>% reorg_vars(c("cyl","mpg"))
# dat <- mtcars
# reorg_vars(dat,c("cyl","mpg"))
# reorg_vars("dat",c("cyl","mpg"))
# reorg_vars("dat",c("cyl","mpg"), tr_save = FALSE)

remove_na <- function(dataset,
                      tr_vars = "",
                      tr_dataset = dataset,
                      data_filter = "",
                      tr_save = TRUE) {

	# remove missing values based on selected variables
	cc <- getdata(dataset, tr_vars, filt = data_filter, na.rm = FALSE) %>% complete.cases

  tr_message <- if (tr_vars[1] == "")
  						    paste0("\nRemoved missing values on ", lubridate::now())
  							else
  							  paste0("\nRemoved missing values using: ", paste0(tr_vars, collapse=", "), " (", lubridate::now(), ")")

	getdata(dataset, filt = data_filter, na.rm = FALSE) %>% filter(cc) %>%
	  {if (!tr_save) . else save2env(., dataset, tr_dataset, tr_message)}
}

save_filtered <- function(dataset,
                          tr_dataset = dataset,
                          data_filter = "",
                          tr_save = TRUE) {

	if (!tr_save)
		return(getdata(dataset, filt = data_filter, na.rm = FALSE))

	tr_message <- if (data_filter == "") ""
								else paste0("\nSaved filtered data: ", data_filter, " (", lubridate::now(), ")")

	getdata(dataset, filt = data_filter, na.rm = FALSE) %>%
	  save2env(dataset, tr_dataset, tr_message)
}

# list of function arguments
tr_args_fun <- function(fun) as.list(formals(fun))

# list of function inputs selected by user
tr_inputs <- reactive({
  # if (input$tr_change_type != "reorg_vars") return("")
  # loop needed because reactive values don't allow single bracket indexing
  tr_args <- tr_args_fun(input$tr_change_type)
  for (i in names(tr_args))
    tr_args[[i]] <- input[[i]]
  if (!input$show_filter) tr_args$data_filter = ""
  # if (input$dataset == input$tr_dataset) tr_args$tr_dataset = ""
  tr_args
})

# tr_args_fun("reorg_vars")

inp_vars <- function(inp, rval = "")
	if (is_empty(input[[inp]])) rval else input[[inp]]

transform_main <- reactive({

	if (is.null(input$tr_change_type)) return()
	dat <- .getdata()

	if (input$tr_change_type == "none") {
	  if (not_available(input$tr_vars)) return(dat)
 		dat <- select_(dat, .dots = input$tr_vars)
	}

	if (input$tr_change_type == "reorg_vars")
 	  return(reorg_vars(dat, inp_vars("tr_reorg_vars"), tr_save = FALSE))

	if (input$tr_change_type == "remove_na")
		return(remove_na(dat, inp_vars("tr_vars"), tr_save = FALSE))

	if (input$tr_change_type == 'save_filtered') {
		if (input$show_filter == FALSE)
			updateCheckboxInput(session = session, inputId = "show_filter", value = TRUE)

		return(save_filtered(dat, tr_save = FALSE))
	}

	# stopped here

	if (!is.null(input$tr_vars)) {
		if (!all(input$tr_vars %in% colnames(dat))) return()
		dat <- select_(dat, .dots = input$tr_vars)
    vars <- colnames(dat)

		if (input$tr_transfunction != 'none') {
      fun <- get(input$tr_transfunction)
      dat_tr <- dat %>% mutate_each_(funs(fun), vars)
  		cn <- c(vars,paste(input$tr_transfunction,vars, sep="_"))
			dat <- cbind(dat,dat_tr)
			colnames(dat) <- cn
		}
		if (input$tr_typefunction != 'none') {
      fun <- get(input$tr_typefunction)
      dat <- mutate_each_(dat,funs(fun), vars)
		}
    if (!is.null(input$tr_normalizer) && input$tr_normalizer != 'none') {

      dc <- getclass(dat)
      isNum <- "numeric" == dc | "integer" == dc
      if (sum(isNum) == 0) return("Please select numerical variables to normalize")
      dat_tr <- dplyr::select(dat,which(isNum)) / .getdata()[,input$tr_normalizer]
      # dat_tr <- try(dplyr::select(dat,which(isNum)) / .getdata()[,input$tr_normalizer], silent = TRUE)
      # if (is(dat_tr, 'try-error'))
      # 	return(paste0("The normalization failed. The error message was:\n\n", attr(dat_tr,"condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
     	cn <- c(vars,paste(vars[isNum],input$tr_normalizer, sep="_"))
			dat <- cbind(dat,dat_tr)
			colnames(dat) <- cn
 		}
	} else {
		if (!input$tr_change_type %in% c("", "save_filtered", "create", "clip")) return()
	}

	if (!is.null(input$tr_vars) & input$tr_change_type == 'reorg_levs') {
    if (!is.null(input$tr_reorg_levs)) {
    	isFct <- "factor" == .getclass()[input$tr_vars[1]]
		  if (isFct) dat[,input$tr_vars[1]] <-
		  						factor(dat[,input$tr_vars[1]], levels = input$tr_reorg_levs)
    }
  }

	if (input$tr_change_type ==  'recode') {
		if (input$tr_recode != '') {

			recom <- input$tr_recode
			recom <- gsub("\"","\'", recom)

			newvar <- try(do.call(car::recode, list(dat[,input$tr_vars[1]],recom)), silent = TRUE)
			if (!is(newvar, 'try-error')) {
				cn <- c(colnames(dat),paste("rc",input$tr_vars[1], sep="_"))
				dat <- cbind(dat,newvar)
				colnames(dat) <- cn
				return(dat)
			} else {
      	return(paste0("The recode command was not valid. The error message was:\n", attr(newvar,"condition")$message, "\nPlease try again. Examples are shown in the help file."))
			}
		}
	}

	if (input$tr_change_type == 'clip') {
		if (input$tr_paste != '') {
			cpdat <- read.table(header=T, text=input$tr_paste)
			cpname <- names(cpdat)
			if (sum(cpname %in% colnames(dat)) > 0) names(cpdat) <- paste('cp',cpname,sep = '_')
			if (is.null(input$tr_vars)) return(cpdat)
			if (nrow(cpdat) == nrow(dat)) dat <- cbind(dat,cpdat)
		}
	}

	if (input$tr_change_type == 'rename') {
		if (!is.null(input$tr_vars) && input$tr_rename != '') {
			rcom <- unlist(strsplit(gsub(" ","",input$tr_rename), ","))
			rcom <- rcom[1:min(length(rcom),length(input$tr_vars))]
			names(dat)[1:length(rcom)] <- rcom
      # rename_(dat, .dots = setNames(l2,l1))   # dplyr alternative has the same dplyr::changes result
		}
	}

	if (input$tr_change_type == 'create') {
		if (input$tr_transform != '') {
			recom <- input$tr_transform
			recom <- gsub("\"","\'", recom)

			fullDat <- .getdata()
			newvar <- try(do.call(within, list(fullDat,parse(text = recom))), silent = TRUE)
			if (!is(newvar, 'try-error')) {
				nfull <- ncol(fullDat)
				nnew <- ncol(newvar)

				# this won't work properly if the transform command creates a new variable
				# and also overwrites an existing one
				if (nfull < nnew) newvar <- newvar[,(nfull+1):nnew, drop = FALSE]
				if (is.null(input$tr_vars)) return(newvar)
				cn <- c(colnames(dat),colnames(newvar))
				dat <- cbind(dat,newvar)
				colnames(dat) <- cn
				head(dat)
			} else {
      	return(paste0("The create command was not valid. The command entered was:\n\n", recom, "\n\nThe error message was:\n\n", attr(newvar,"condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
			}
		}
	}

	dat
})

output$transform_data <- reactive({
  dat <- transform_main()
  if (is.null(dat) || is.character(dat)) return(invisible())
  show_data_snippet(dat)
})

output$transform_summary <- renderPrint({
	dat <- transform_main()
	if (is.null(dat)) return(invisible())
	if (is.character(dat)) cat(dat) else getsummary(dat)
})

observe({
	if (is.null(input$tr_save_changes) || input$tr_save_changes == 0) return()
	isolate({
		dat <- transform_main()
		if (dat %>% is.null) return()
		if (dat %>% is.character) return(dat)

		# saving to a new dataset if specified
		dataset <- input$tr_dataset
		if (r_data[[dataset]] %>% is.null) {
			r_data[[dataset]] <- .getdata()
			r_data[[paste0(dataset,"_descr")]] <- r_data[[paste0(input$dataset,"_descr")]]
			r_data[['datasetlist']] %<>%
				c(dataset,.) %>%
				unique
		}

	  if (input$tr_change_type == 'type') {
	  	r_data[[dataset]][,colnames(dat)] <- dat
		} else if (input$tr_change_type == 'remove_na') {
	  	r_data[[dataset]] <- dat
		} else if (input$tr_change_type == 'save_filtered') {
	  	r_data[[dataset]] <- dat
	    r_data[[paste0(dataset,"_descr")]] %<>%
	    	paste0(., "\n\n### Subset\n\nCommand used: `", input$data_filter,
	    	       		"` to filter from dataset: ", input$dataset)
		} else if (input$tr_change_type == 'rename') {
  		r_data[[dataset]] %<>%
  			rename_(.dots = setNames(input$tr_vars, colnames(dat)))
		} else if (input$tr_change_type == 'reorg_vars') {
	  	# r_data[[dataset]] %<>% .[,input$tr_reorg_vars]
	  	r_data[[dataset]] %<>% select_(.dots = input$tr_reorg_vars)
	  } else {
			.changedata(dat, colnames(dat), dataset = dataset)
		}

		# reset input values once the changes have been applied
		updateSelectInput(session = session, inputId = "tr_change_type", selected = "none")

    if (dataset != input$dataset)
			updateSelectInput(session = session, inputId = "dataset", select = dataset)

  })
})

observe({
	# reset all settings when tr_change_type is changed
	input$tr_change_type
	isolate({
		updateTextInput(session = session, inputId = "tr_transform", value = "")
	 	updateTextInput(session = session, inputId = "tr_recode", value = "")
	 	updateTextInput(session = session, inputId = "tr_rename", value = "")
	 	updateTextInput(session = session, inputId = "tr_paste", value = "")
		updateSelectInput(session = session, inputId = "tr_typefunction", selected = "none")
		updateSelectInput(session = session, inputId = "tr_transfunction", selected = "none")
	  updateSelectInput(session = session, inputId = "tr_normalizer", selected = "none")
	 	# updateTextInput(session = session, inputId = "tr_create", value = "")
	 	# updateTextInput(session = session, inputId = "tr_subset", value =  "")
	})
})


observe({
  if (not_pressed(input$transform_report)) return()
  fun <- isolate(input$tr_change_type)
  isolate({
    update_report(inp_main = clean_args(tr_inputs(), tr_args_fun(fun)),
                  fun_name = fun, outputs = character(0), pre_cmd = "",
                  figs = FALSE)
  })
})
