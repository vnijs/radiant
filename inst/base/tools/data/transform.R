# UI-elements for transform
output$uiTr_columns <- renderUI({
  vars <- varnames()
  selectInput("tr_columns", "Select variable(s):", choices  = vars,
    selected = state_multiple("tr_columns", vars),
    multiple = TRUE, size = min(8, length(vars)), selectize = FALSE)
})

output$uiTr_reorg_cols <- renderUI({
  vars <- varnames()
  selectizeInput("tr_reorg_cols", "Reorder/remove variables", choices  = vars,
    selected = vars, multiple = TRUE,
    options = list(placeholder = 'Select variable(s)',
                   plugins = list('remove_button', 'drag_drop')))
})

output$uiTr_normalizer <- renderUI({
  isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  if(length(vars) == 0) return(NULL)
  selectInput("tr_normalizer", "Normalizing variable:", c("None" = "none", vars),
              selected = "none")
})

output$uiTr_reorg_levs <- renderUI({
	if(input$tr_columns %>% not_available) return()
  fctCol <- input$tr_columns[1]
	isFct <- "factor" == getdata_class()[fctCol]
  if(!isFct) return()
	.getdata()[,fctCol] %>% levels -> levs
  selectizeInput("tr_reorg_levs", "Reorder/remove levels", choices  = levs,
    selected = levs, multiple = TRUE,
    options = list(placeholder = 'Select level(s)',
                   plugins = list('remove_button', 'drag_drop')))
})

# standardize variable
st <- function(x)
	if(is.numeric(x)) scale(x) else x

# center variable
cent <- function(x)
	if(is.numeric(x)) { x - mean(x, na.rm = TRUE) } else x

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
d_mdy <- function(x) { if(is.factor(x)) as.character(x) else x } %>% mdy %>% as.Date
d_dmy <- function(x) { if(is.factor(x)) as.character(x) else x } %>% dmy %>% as.Date
d_ymd <- function(x) { if(is.factor(x)) as.character(x) else x } %>% ymd %>% as.Date

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
d_ymd_hms <- function(x) { if(x %>% is.factor) as.character(x) else x } %>% ymd_hms

as_int <- function(x) {
	if(is.factor(x)) {
		levels(x) %>% .[x] %>% as.integer
	} else {
		as.integer(x)
	}
}

as_num <- function(x) {
	if(is.factor(x)) {
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
                    "Rename" = "rename", "Reorder/remove variables" = "reorg_cols",
                    "Reorder/remove levels" = "reorg_levs",
                    "Remove missing" = "na.remove",
                    "Filter" = "sub_filter")

output$ui_Transform <- renderUI({
	# Inspired by Ian Fellow's transform ui in JGR/Deducer
  list(wellPanel(
    uiOutput("uiTr_columns"),
    selectInput("tr_changeType", "Transformation type:", trans_types, selected = "none"),
    conditionalPanel(condition = "input.tr_changeType == 'type'",
	    selectInput("tr_typefunction", "Change variable type:", type_options, selected = "none")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'change'",
	    selectInput("tr_transfunction", "Apply function:", trans_options)
    ),
    conditionalPanel(condition = "input.tr_changeType == 'normalize'",
      uiOutput("uiTr_normalizer")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'create'",
	    returnTextAreaInput("tr_transform", "Create (e.g., x = y - z):", '')
    ),
    conditionalPanel(condition = "input.tr_changeType == 'clip'",
    	HTML("<label>Paste from Excel:</label>"),
    	tags$textarea(class="form-control",
    	              id="tr_copyAndPaste", rows=3, "")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'recode'",
	    returnTextAreaInput("tr_recode", "Recode (e.g., lo:20 = 1):", '')
    ),
    conditionalPanel(condition = "input.tr_changeType == 'rename'",
	    returnTextAreaInput("tr_rename", "Rename (separate by , ):", '')
    ),
    conditionalPanel(condition = "input.tr_changeType != ''",
	    actionButton("tr_show_changes", "Show"),
	    actionButton("tr_save_changes", "Save changes")
	  ),
    conditionalPanel(condition = "input.tr_changeType == 'reorg_cols'",
    	br(),
	    uiOutput("uiTr_reorg_cols")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'reorg_levs'", br(),
	    uiOutput("uiTr_reorg_levs")
    ),
	  textInput("tr_dataset", "Save changes to:", input$dataset)
  	),
		help_modal('Transform','transformHelp',inclMD("../base/tools/help/transform.md"))

	)
})

transform_main <- reactive({

	if(is.null(input$tr_changeType)) return()

	dat <- .getdata()

  ##### Fix - show data snippet if changeType == 'none' and no columns selected #####
	if(input$tr_changeType == "none") {
	  if(input$tr_columns %>% not_available) return()
 		dat <- select_(dat, .dots = input$tr_columns)
	}

	if(input$tr_changeType == 'reorg_cols') {
    if(is.null(input$tr_reorg_cols)) {
      ordVars <- colnames(dat)
 	  } else {
   	  ordVars <- input$tr_reorg_cols
    }
 	  return(dat[,ordVars, drop = FALSE])
  }

	if(input$tr_changeType == 'na.remove') {
		if(!is.null(input$tr_columns)) {
      # removing rows based on NAs in specific columns
			return(dat[complete.cases(dat[,input$tr_columns]),])
		} else {
      # removing all rows with NAs in any column
	 	  return(na.omit( dat ))
		}
  }

	if(input$tr_changeType == 'sub_filter') {
		if(input$show_filter == FALSE)
			updateCheckboxInput(session = session, inputId = "show_filter", value = TRUE)
	}

	if(!is.null(input$tr_columns)) {
		if(!all(input$tr_columns %in% colnames(dat))) return()
		dat <- select_(dat, .dots = input$tr_columns)
    vars <- colnames(dat)

		if(input$tr_transfunction != 'none') {
			# dat_tr <- try(dat %>% mutate_each_(funs(input$tr_transfunction), vars), silent = TRUE)
      # if(is(dat_tr, 'try-error')) dat_tr <- dat * NA
			# if(is(dat_tr, 'try-error')) return(attr(dat_tr,"condition")$message)

      fun <- get(input$tr_transfunction)
      dat_tr <- dat %>% mutate_each_(funs(fun), vars)
			# dat_tr <- dat %>% mutate_each_(input$tr_transfunction, vars)

  		cn <- c(vars,paste(input$tr_transfunction,vars, sep="_"))
			dat <- cbind(dat,dat_tr)
			colnames(dat) <- cn
		}
		if(input$tr_typefunction != 'none') {
      fun <- get(input$tr_typefunction)
			# dat <- mutate_each_(dat,input$tr_typefunction, vars)
      dat <- mutate_each_(dat,funs(fun), vars)
		}
    if(!is.null(input$tr_normalizer) && input$tr_normalizer != 'none') {

      dat_class <- getdata_class_fun(dat)
      isNum <- "numeric" == dat_class | "integer" == dat_class
      if(length(isNum) == 0) return("Please select numerical variables to normalize")
      dat_tr <- try(dplyr::select(dat,which(isNum)) / .getdata()[,input$tr_normalizer], silent = TRUE)
      if(is(dat_tr, 'try-error')) return(attr(dat_tr,"condition")$message)
     	cn <- c(vars,paste(vars[isNum],input$tr_normalizer, sep="_"))
			dat <- cbind(dat,dat_tr)
			colnames(dat) <- cn
 		}
	} else {
		if(!input$tr_changeType %in% c("", "sub_filter", "create", "clip")) return()
	}

	if(!is.null(input$tr_columns) & input$tr_changeType == 'reorg_levs') {
    if(!is.null(input$tr_reorg_levs)) {
    	isFct <- "factor" == getdata_class()[input$tr_columns[1]]
		  if(isFct) dat[,input$tr_columns[1]] <-
		  						factor(dat[,input$tr_columns[1]], levels = input$tr_reorg_levs)
    }
  }

	if(input$tr_changeType ==  'recode') {
		if(input$tr_recode != '') {

			recom <- input$tr_recode
			recom <- gsub("\"","\'", recom)

			newvar <- try(do.call(car::recode, list(dat[,input$tr_columns[1]],recom)), silent = TRUE)
			if(!is(newvar, 'try-error')) {

				cn <- c(colnames(dat),paste("rc",input$tr_columns[1], sep="_"))
				dat <- cbind(dat,newvar)
				colnames(dat) <- cn
				return(dat)
			}
		}
	}

	if(input$tr_changeType == 'clip') {
		if(input$tr_copyAndPaste != '') {
			cpdat <- read.table(header=T, text=input$tr_copyAndPaste)
			cpname <- names(cpdat)
			if(sum(cpname %in% colnames(dat)) > 0) names(cpdat) <- paste('cp',cpname,sep = '_')
			if(is.null(input$tr_columns)) return(cpdat)
			if(nrow(cpdat) == nrow(dat)) dat <- cbind(dat,cpdat)
		}
	}

	if(input$tr_changeType == 'rename') {
		if(!is.null(input$tr_columns) && input$tr_rename != '') {
			rcom <- unlist(strsplit(gsub(" ","",input$tr_rename), ","))
			rcom <- rcom[1:min(length(rcom),length(input$tr_columns))]
			names(dat)[1:length(rcom)] <- rcom
      # rename_(dat, .dots = setNames(l2,l1))   # dplyr alternative has the same dplyr::changes result
		}
	}

	if(input$tr_changeType == 'create') {
		if(input$tr_transform != '') {
			recom <- input$tr_transform
			recom <- gsub("\"","\'", recom)

			fullDat <- .getdata()
			newvar <- try(do.call(within, list(fullDat,parse(text = recom))), silent = TRUE)

			if(!is(newvar, 'try-error')) {
				nfull <- ncol(fullDat)
				nnew <- ncol(newvar)

				# this won't work properly if the transform command creates a new variable
				# and also overwrites an existing one
				if(nfull < nnew) newvar <- newvar[,(nfull+1):nnew, drop = FALSE]
				if(is.null(input$tr_columns)) return(newvar)
				cn <- c(colnames(dat),colnames(newvar))
				dat <- cbind(dat,newvar)
				colnames(dat) <- cn

				head(dat)
			# } else if(is.null(input$tr_columns)) {
			} else {
				# the 'create' command did not compile so if there were
				# no variables selected show ... nothing
				# print(paste0("Create command:", recom, "did not create a new variable. Please try again."))
			 	# updateTextInput(session = session, inputId = "tr_transform", label = "Create (e.g., y = x - z):", '')
				return(paste0("Create command: ", recom, " did not create a new variable. Please try again."))
				# return()
			}
		}
	}

	dat
})

output$transform_data <- reactive({

  dat <- transform_main()
  if(is.null(dat)) return(invisible())
  if(is.character(dat)) return(dat)
  show_data_snippet(dat)
})

output$transform_summary <- renderPrint({

	dat <- transform_main()
	if(is.null(dat)) return(invisible())
	if(is.character(dat)) return(dat)

  dat_class <- getdata_class_fun(dat)

	isFct <- "factor" == dat_class
	isNum <- "numeric" == dat_class | "integer" == dat_class
	isDate <- "date" == dat_class | "Date" == dat_class
	isChar <- "character" == dat_class
	isLogic <- "logical" == dat_class

	if(sum(isNum) > 0) {

		cn <- names(dat_class)[isNum]

		cat("Summarize numeric variables:\n")
    select(dat, which(isNum)) %>%
      tidyr::gather_("variable", "values", cn) %>%
      group_by(variable) %>%
      summarise_each(funs(n = length, missing = nmissing, mean(.,na.rm=TRUE),
                     median(.,na.rm=TRUE), min(.,na.rm=TRUE), max(.,na.rm=TRUE),
                     `25%` = p25, `75%` = p75, sd(.,na.rm=TRUE), se = serr,
                     cv = sd/mean)) %>%
      as.data.frame -> num_dat
      num_dat[,-1] %<>% round(3)
      colnames(num_dat)[1] <- ""
      num_dat %>% print(row.names = FALSE)
		cat("\n")
	}
	if(sum(isFct) > 0) {
		cat("Summarize factors:\n")
    select(dat, which(isFct)) %>% summary %>% print
		cat("\n")
	}
	if(sum(isDate) > 0) {
		cat("Earliest dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(min)) %>% print
		cat("\nFinal dates:\n")
    select(dat, which(isDate)) %>% summarise_each(funs(max)) %>% print
		cat("\n")
	}
	if(sum(isChar) > 0) {
		cat("Summarize character variables:\n")
    select(dat, which(isChar)) %>% table %>% print
		cat("\n")
	}
	if(sum(isLogic) > 0) {
		cat("Summarize logical variables:\n")
    select(dat, which(isLogic)) %>% table %>% print
		cat("\n")
	}
})

observe({
	if(is.null(input$tr_save_changes) || input$tr_save_changes == 0) return()
	isolate({
		dat <- transform_main()
		if(dat %>% is.null) return()
		if(dat %>% is.character) return(dat)

		# saving to a new dataset if specified
		dataset <- input$tr_dataset
		if(r_data[[dataset]] %>% is.null) {
			r_data[[dataset]] <- .getdata()
			r_data[[paste0(dataset,"_descr")]] <- r_data[[paste0(input$dataset,"_descr")]]
			r_data[['datasetlist']] %<>%
				c(dataset,.) %>%
				unique
		}

	  if(input$tr_changeType == 'type') {
	  	r_data[[dataset]][,colnames(dat)] <- dat
		} else if(input$tr_changeType == 'na.remove') {
	  	r_data[[dataset]] <- dat
		} else if(input$tr_changeType == 'sub_filter') {
	  	r_data[[dataset]] <- dat
	    r_data[[paste0(dataset,"_descr")]] %<>%
	    	paste0(., "\n\n### Subset\n\nCommand used: `", input$data_filter,
	    	       		"` to filter from dataset: ", input$dataset)
		} else if(input$tr_changeType == 'rename') {
  		r_data[[dataset]] %<>%
  			rename_(.dots = setNames(input$tr_columns, colnames(dat)))
		} else if(input$tr_changeType == 'reorg_cols') {
	  	# r_data[[dataset]] %<>% .[,input$tr_reorg_cols]
	  	r_data[[dataset]] %<>% select_(.dots = input$tr_reorg_cols)
	  } else {
			.changedata(dat, colnames(dat), dataset = dataset)
		}

		# reset input values once the changes have been applied
		updateSelectInput(session = session, inputId = "tr_changeType", selected = "none")

    if(dataset != input$dataset)
			updateSelectInput(session = session, inputId = "dataset", select = dataset)

  })
})

observe({
	# reset to original value when type is changed
	input$tr_changeType
	isolate({
		updateTextInput(session = session, inputId = "tr_transform", value = "")
	 	updateTextInput(session = session, inputId = "tr_recode", value = "")
	 	updateTextInput(session = session, inputId = "tr_create", value = "")
	 	updateTextInput(session = session, inputId = "tr_rename", value = "")
	 	updateTextInput(session = session, inputId = "tr_copyAndPaste", value = "")
	 	updateTextInput(session = session, inputId = "tr_subset", value =  "")
		updateSelectInput(session = session, inputId = "tr_typefunction", selected = "none")
		updateSelectInput(session = session, inputId = "tr_transfunction", selected = "none")
	  updateSelectInput(session = session, inputId = "tr_normalizer", selected = "none")
	})
})
