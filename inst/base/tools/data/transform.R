# UI-elements for transform
output$ui_tr_vars <- renderUI({
  vars <- varnames()
  if (not_available(vars)) return()
  selectInput("tr_vars", "Select variable(s):", choices  = vars,
    selected = state_multiple("tr_vars", vars),
    multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
})

output$ui_tr_replace <- renderUI({
  vars <- varnames()
  selectInput("tr_replace", "Select replacement variables:", choices  = vars,
    selected = state_multiple("tr_replace", vars),
    multiple = TRUE, size = min(2, length(vars)), selectize = FALSE)
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

## decile split
dec <- function(x) {
  ## avoid non-unique breaks
  df <- data.frame(breaks = quantile(x, seq(0,1,.1))) %>% set_rownames(0:10) %>% unique
  cut(x, df$breaks, rownames(df)[-1], include.lowest = TRUE)
}

sq <- function(x) x^2
inv <- function(x) 1/x
normalize <- function(x,y) x/y

## use as.character in case x is a factor
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

## time in hours:minutes and seconds
# t_hms <- . %>% { if (is.factor(.)) as.character(.) else . } %>% lubridate::hms(.)
# t_hm <- . %>% { if (is.factor(.)) as.character(.) else . } %>% lubridate::hm(.)

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

make_train <- function(n = .7, name = "training", nr = 100) {
  if (n < 1) n <- round(n * nr) %>% max(1)
  ind <- seq_len(nr)
  training <- rep_len(0L,nr)
  training[sample(ind,n)] <- 1L
  training
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
                     "As numeric" = "as_num", "As integer" = "as_int",
                     "As character" = "as.character", "As date (mdy)" = "d_mdy",
                     "As date (dmy)" = "d_dmy", "As date (ymd)" = "d_ymd",
                     "As date/time (ymd_hms)" = "d_ymd_hms")
                     # "As time (hm)" = "t_hm", "As time (hms)" = "t_hms",

# trans_types <- list("None" = "none", "Type" = "type", "Change" = "change",
trans_types <- list("None" = "none", "Type" = "type", "Transform" = "transform",
                    "Normalize" = "normalize", "Create" = "create",
                    "Training sample" = "training",
                    "Clipboard" = "clip", "Recode" = "recode",
                    "Rename" = "rename", "Replace" = "replace",
                    "Reorder/remove variables" = "reorg_vars",
                    "Reorder/remove levels" = "reorg_levs",
                    "Remove missing" = "remove_na")
                    # "Store filtered data" = "store_filtered")

output$ui_Transform <- renderUI({
	## Inspired by Ian Fellow's transform ui in JGR/Deducer
  tagList(wellPanel(
    uiOutput("ui_tr_vars"),
    selectInput("tr_change_type", "Transformation type:", trans_types, selected = "none"),
    conditionalPanel(condition = "input.tr_change_type == 'type'",
	    selectInput("tr_typefunction", "Change variable type:", type_options, selected = "none")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'transform'",
	    selectInput("tr_transfunction", "Apply function:", trans_options)
    ),
    conditionalPanel(condition = "input.tr_change_type == 'normalize'",
      uiOutput("ui_tr_normalizer")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'create'",
	    returnTextAreaInput("tr_create", "Create (e.g., x = y - z):", "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'training'",
      tags$table(
        tags$td(numericInput("tr_training_n", label = "Size:", min = 0,
                value = state_init("tr_training_n", .9))),
        tags$td(textInput("tr_training", "Variable name:",
                state_init("tr_training","training")))
      )
    ),
    conditionalPanel(condition = "input.tr_change_type == 'clip'",
    	HTML("<label>Paste from spreadsheet:</label>"),
    	tags$textarea(class="form-control", id="tr_paste", rows=3, "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'recode'",
	    returnTextAreaInput("tr_recode", "Recode (e.g., lo:20 = 1):", "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'rename'",
      returnTextAreaInput("tr_rename", "Rename (separate by , ):", "")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'replace'",
      uiOutput("ui_tr_replace")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'reorg_vars'",
      uiOutput("ui_tr_reorg_vars")
    ),
    conditionalPanel(condition = "input.tr_change_type == 'reorg_levs'",
      uiOutput("ui_tr_reorg_levs")
    ),
    conditionalPanel(condition = "input.tr_change_type != 'none'",
	    tags$table(
	      tags$td(textInput("tr_dataset", "Store changes in:", input$dataset)),
	      tags$td(actionButton("tr_store", "Store"), style="padding-top:30px;")
	    )
	  ),
    help_and_report(modal_title = "Transform",
                    fun_name = "transform",
                    help_file = inclMD(file.path(r_path, "base/tools/help/transform.md")))
		# help_modal("Transform", "transform_help",
		#   inclMD(file.path(r_path,"base/tools/help/transform.md")))

	))
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

# store_filtered <- function(dataset,
#                           tr_dataset = dataset,
#                           data_filter = "",
#                           tr_save = TRUE) {

# 	if (!tr_save)
# 		return(getdata(dataset, filt = data_filter, na.rm = FALSE))

# 	tr_message <- if (data_filter == "") ""
# 								else paste0("\nSaved filtered data: ", data_filter, " (", lubridate::now(), ")")

# 	getdata(dataset, filt = data_filter, na.rm = FALSE) %>%
# 	  save2env(dataset, tr_dataset, tr_message)
# }

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

	## don't transform when a filter is active
  selcom <- input$data_filter %>% gsub("\\s","", .) %>% gsub("\"","\'",.)
  if (!is_empty(selcom) && input$show_filter == TRUE)
  	return("A filter is active. Either uncheck the filter checkbox, remove the filter statement,\nor store the filtered data through the Data > View tab")

	dat <- .getdata()

  if (input$tr_change_type == 'training') {
    tname <- if (input$tr_training == "") "training" else input$tr_training
    tnr <- nrow(dat)
    tn <- input$tr_training_n %>% {ifelse (. < 0 || is.na(.) || . > tnr, .7, .)}
    return(data.frame(make_train(tn, tname, tnr)) %>% setNames(tname))
  }

	if (input$tr_change_type == "none") {
	  if (not_available(input$tr_vars)) return(dat)
 		dat <- select_(dat, .dots = input$tr_vars)
	}

	if (input$tr_change_type == "reorg_vars")
 	  return(reorg_vars(dat, inp_vars("tr_reorg_vars"), tr_save = FALSE))

	if (input$tr_change_type == "remove_na")
		return(remove_na(dat, inp_vars("tr_vars"), tr_save = FALSE))

	# stopped here

	if (!is.null(input$tr_vars)) {
		if (!all(input$tr_vars %in% colnames(dat))) return()
		dat <- select_(dat, .dots = input$tr_vars)
    vars <- colnames(dat)

		if (input$tr_transfunction != 'none') {
      fun <- get(input$tr_transfunction)
      dat <- dat %>% mutate_each_(funs(fun), vars) %>%
        bind_cols(dat, .) %>%
        set_colnames(c(vars, paste(vars, input$tr_transfunction, sep="_")))

      # dat <- cbind(dat,dat_tr)
      # cn <- c(vars,paste(vars, input$tr_transfunction, sep="_"))
   #    dat_tr <- dat %>% mutate_each_(funs(fun), vars)
  	# 	cn <- c(vars,paste(vars, input$tr_transfunction, sep="_"))
			# dat <- cbind(dat,dat_tr)
			# dat <- bind_cols(dat,dat_tr)
			# colnames(dat) <- cn
		}
		if (input$tr_typefunction != 'none') {
      ## change in type is always done in-place
      fun <- get(input$tr_typefunction)
      dat <- mutate_each_(dat,funs(fun), vars)
		}
    if (!is.null(input$tr_normalizer) && input$tr_normalizer != 'none') {

      dc <- getclass(dat)
      isNum <- "numeric" == dc | "integer" == dc
      if (sum(isNum) == 0) return("Please select numerical variables to normalize")
      dat_tr <- dplyr::select(dat,which(isNum)) / .getdata()[,input$tr_normalizer]
     	cn <- c(vars,paste(vars[isNum],input$tr_normalizer, sep="_"))
			dat <- cbind(dat,dat_tr)
			colnames(dat) <- cn
 		}
	} else {
		if (!input$tr_change_type %in% c("", "store_filtered", "create", "clip")) return()
	}

	if (!is.null(input$tr_vars) & input$tr_change_type == 'reorg_levs') {
    if (!is.null(input$tr_reorg_levs)) {
    	isFct <- "factor" == .getclass()[input$tr_vars[1]]
		  if (isFct) dat[,input$tr_vars[1]] <-
		  						factor(dat[,input$tr_vars[1]], levels = input$tr_reorg_levs)
    }
  }

	if (input$tr_change_type ==  'recode') {
    recom <- input$tr_recode %>% gsub("\\s","", .) %>% gsub("\"","\'",.) %>% gsub(",",";", .)
		if (recom != "") {
			nvar <- try(do.call(car::recode, list(dat[[input$tr_vars[1]]],recom)), silent = TRUE)
			if (!is(nvar, 'try-error')) {
        return(data.frame(nvar) %>% setNames(paste(input$tr_vars[1],"rc", sep="_")))
			} else {
      	return(paste0("The recode command was not valid. The error message was:\n", attr(nvar,"condition")$message, "\nPlease try again. Examples are shown in the help file (click the ? icon)."))
			}
		}
	}

	if (input$tr_change_type == 'clip') {
		if (input$tr_paste != "") {

      cpdat <- try(read.table(header = TRUE, comment.char = "", fill = TRUE, sep = "\t", as.is = TRUE, text = input$tr_paste), silent = TRUE)
      if (is(cpdat, 'try-error')) {
        return("The copy-and-pasted data was not well formated. Please make\nsure the number of rows in the data in Radiant and in the\nspreadsheet are the same and try again.")
      }

      if (is.null(input$tr_vars)) {
        if (nrow(.getdata()) == nrow(cpdat)) return(cpdat)
        else return("The copy-and-pasted data does not have the correct number of rows. Please make\nsure the number of rows in the data in Radiant and in the spreadsheet are the\nsame and try again.")
      }

			cpname <- names(cpdat)
			if (sum(cpname %in% colnames(dat)) > 0) names(cpdat) <- paste(cpname, "cp", sep = '_')
      if (nrow(cpdat) == nrow(dat)) dat <- bind_cols(dat,cpdat)
      else return("The copy-and-pasted data does not have the correct number of rows. Please make\nsure the number of rows in the data in Radiant and in the spreadsheet are the\nsame and try again.")
		}
	}

	if (input$tr_change_type == 'rename') {
    tr_rename <- input$tr_rename %>% gsub("\\s","", .)
		if (!is.null(input$tr_vars) && tr_rename != "") {
			rcom <- unlist(strsplit(tr_rename, ",")) %>%
        .[1:min(length(.),length(input$tr_vars))]
			names(dat)[1:length(rcom)] <- rcom
      ## dplyr alternative has the same dplyr::changes result
      # rename_(dat, .dots = setNames(l2,l1))
		}
	}

	if (input$tr_change_type == 'replace') {
		vars <- input$tr_vars
		replace <- input$tr_replace
		if (available(input$tr_vars) && available(input$tr_replace)) {
		  if (length(vars) != length(replace))
        return(paste0("The number of replacement variables (", length(replace), ") is not equal to the number of variables to replace (", length(vars),")"))
			dat[,vars] <- .getdata()[,replace]
		}
	}

	if (input$tr_change_type == 'create') {
    ccom <- input$tr_create %>% gsub("\\s","", .) %>% gsub("\"","\'",.)
		if (ccom != "") {
			fdat <- .getdata()
			ndat <- try(do.call(within, list(fdat, parse(text = ccom))), silent = TRUE)
			if (is(ndat, 'try-error')) {
      	return(paste0("The create command was not valid. The command entered was:\n\n", ccom, "\n\nThe error message was:\n\n", attr(ndat,"condition")$message, "\n\nPlease try again. Examples are shown in the help file."))
      } else {
			  nvars <- strsplit(ccom, ";")[[1]] %>% strsplit(.,"=") %>% sapply("[", 1)
				dat <- select_(ndat, .dots = nvars)
			}
		}
	}

	dat
})

output$transform_data <- reactive({
  dat <- transform_main()
  if (is.null(dat) || is.character(dat) || ncol(dat) == 0) return(invisible())
  show_data_snippet(dat)
})

output$transform_summary <- renderPrint({
	dat <- transform_main()
	if (is.null(dat)) return(invisible())
	if (is.character(dat)) cat(dat) else getsummary(dat)
})

observeEvent(input$tr_store, {
	isolate({
		dat <- transform_main()
		if (is.null(dat)) return()
		if (is.character(dat)) return(dat)

		## saving to a new dataset if specified
		dataset <- input$tr_dataset
		if (r_data[[dataset]] %>% is.null) {
			r_data[[dataset]] <- .getdata()
			r_data[[paste0(dataset,"_descr")]] <- r_data[[paste0(input$dataset,"_descr")]]
			r_data[['datasetlist']] %<>% c(dataset,.) %>% unique
		}

	  if (input$tr_change_type == 'type') {
	  	r_data[[dataset]][,colnames(dat)] <- dat
		} else if (input$tr_change_type == 'remove_na') {
	  	r_data[[dataset]] <- dat
		} else if (input$tr_change_type == 'rename') {
  		r_data[[dataset]] %<>%
  			rename_(.dots = setNames(input$tr_vars, colnames(dat)))
		} else if (input$tr_change_type == 'reorg_vars') {
	  	r_data[[dataset]] %<>% select_(.dots = input$tr_reorg_vars)
		} else if (input$tr_change_type == 'replace') {
	  	r_data[[dataset]][,colnames(dat)] <- dat
	  	r_data[[dataset]][, input$tr_replace] <- list(NULL)
	  } else {
			.changedata(dat, colnames(dat), dataset = dataset)
		}

		## reset input values once the changes have been applied
		updateSelectInput(session = session, inputId = "tr_change_type", selected = "none")

    # if (dataset != input$dataset)
		updateSelectInput(session = session, inputId = "dataset", select = dataset)
  })
})

observeEvent(input$tr_change_type, {
	## reset all values when tr_change_type is changed
	isolate({
		updateTextInput(session = session, inputId = "tr_create", value = "")
	 	updateTextInput(session = session, inputId = "tr_recode", value = "")
	 	updateTextInput(session = session, inputId = "tr_rename", value = "")
	 	updateTextInput(session = session, inputId = "tr_paste", value = "")
		updateSelectInput(session = session, inputId = "tr_typefunction", selected = "none")
		updateSelectInput(session = session, inputId = "tr_transfunction", selected = "none")
	  updateSelectInput(session = session, inputId = "tr_replace", selected = "None")
	  updateSelectInput(session = session, inputId = "tr_normalizer", selected = "none")
	})
})

observeEvent(input$transform_report, {
  fun <- isolate(input$tr_change_type)

  ## not doing anything yet ...
  # if (fun == "none") return()
  return()

  isolate({
    update_report(inp_main = clean_args(tr_inputs(), tr_args_fun(fun)),
                  fun_name = fun, outputs = character(0), pre_cmd = "",
                  figs = FALSE)
  })
})
