# UI-elements for transform
output$uiTr_columns <- renderUI({
	cols <- varnames()
	selectInput("tr_columns", "Select column(s):", choices  = as.list(cols), selected = NULL, multiple = TRUE, selectize = FALSE)
})

output$uiTr_normalizer <- renderUI({
  isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  if(length(vars) == 0) return(NULL)
  selectInput("tr_normalizer", "Normalizing variable:", c("None" = "none", vars), selected = "none")
})

output$uiTr_reorder_levs <- renderUI({
  fctCol <- input$tr_columns[1]
	isFct <- "factor" == getdata_class()[fctCol]
  if(!isFct) return()
	getdata() %>% .[,fctCol] %>% levels -> levs
  returnOrder("tr_reorder_levs", levs)
})

standardize_1sd <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(as.numeric(scale(x)))
	# if(is.numeric(x)) return(scale(x))
}

centerVar <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(x - mean(x, na.rm = TRUE))
	x
}

medianSplit <- function(x) cut(x, breaks=quantile(x,c(0,.5,1)), include.lowest=TRUE, labels=c("Below","Above"))
decileSplit <- function(x) cut(x, breaks=quantile(x,seq(0,1,.1)), include.lowest=TRUE, labels=seq(1,10,1))

sq <<- function(x) x^2
inv <<- function(x) 1/x
normalize <<- function(x,y) x/y
st <<- standardize_1sd
cent <<- centerVar
msp <<- medianSplit
dec <<- decileSplit


# as character needed here in case x is a factor
d_mdy <<- function(x) as.character(x) %>% mdy %>% as.Date
d_dmy <<- function(x) as.character(x) %>% dmy %>% as.Date
d_ymd <<- function(x) as.character(x) %>% ymd %>% as.Date

# http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html
d_ymd_hms <<- function(x) as.character(x) %>% ymd_hms

as.int <<- function(x) ifelse(is.factor(x),
                              return(as.integer(levels(x))[x]),
                              return(as.integer(x)))
as.num <<- function(x) ifelse(is.factor(x),
                              return(as.numeric(levels(x))[x]),
                              return(as.numeric(x)))

trans_options <- list("None" = "none", "Log" = "log", "Exp" = "exp", "Square" = "sq",
                      "Square-root" = "sqrt", "Center" = "cent", "Standardize" = "st",
                      "Invert" = "inv", "Median split" = "msp", "Deciles" = "dec")

type_options <- list("None" = "none", "As factor" = "as.factor",  "As number" = "as.num",
                     "As integer" = "as.int", "As character" = "as.character",
                     "As date (mdy)" = "d_mdy", "As date (dmy)" = "d_dmy", "As date (ymd)" = "d_ymd",
                     "As date/time (ymd_hms)" = "d_ymd_hms")

trans_types <- list("None" = "none", "Type" = "type", "Change" = "change", "Normalize" = "normalize",
                    "Create" = "create", "Clipboard" = "clip", "Recode" = "recode", "Rename" = "rename",
                    "Reorder columns" = "reorder_cols", "Reorder levels" = "reorder_levs",
                    "Remove columns" = "remove", "Remove missing" = "na.remove", "Subset" = "sub_filter")

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
#       selectInput("tr_normalizer", "Choose variable:", c("None" = "none", as.list(varnames())),
#                   selected = "none")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'create'",
	    returnTextInput("tr_transform", "Create (e.g., x = y - z):", '')
    ),
    conditionalPanel(condition = "input.tr_changeType == 'clip'",
    	HTML("<label>Paste from Excel:</label>"),
	    tags$textarea(id="tr_copyAndPaste", rows=3, cols=5, "")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'recode'",
	    returnTextInput("tr_recode", "Recode (e.g., lo:20 = 1):", '')
    ),
    conditionalPanel(condition = "input.tr_changeType == 'rename'",
	   	returnTextInput("tr_rename", "Rename (separate by ','):", '')
    ),
    conditionalPanel(condition = "input.tr_changeType == 'sub_filter'",
      returnTextInput("tr_subset", "Subset (e.g., price > 5000)", '')
    ),
    conditionalPanel(condition = "input.tr_changeType != ''",
	    actionButton("tr_save_changes", "Save changes")
	  ),
    conditionalPanel(condition = "input.tr_changeType == 'reorder_cols'",
    	br(),
    	HTML("<label>Reorder (drag-and-drop):</label>"),
	    returnOrder("tr_reorder_cols", varnames())
    ),
    conditionalPanel(condition = "input.tr_changeType == 'reorder_levs'",
    	br(),
    	HTML("<label>Reorder (drag-and-drop):</label>"),
	    uiOutput("uiTr_reorder_levs")
    )
  	),
		helpModal('Transform','transformHelp',inclMD("../base/tools/help/transform.md"))

		# Reporting option not yet available for transform
 		# helpAndReport('Explore','explore',inclRmd("tools/help/explore.Rmd"))
	)
})

transform_main <- reactive({

	if(is.null(input$tr_changeType)) return()

	dat <- getdata()
#   vars <- colnames(dat)


  ##### Fix - show data snippet if changeType == 'none' and no columns select #####
	if(input$tr_changeType == "none") {
		if(!is.null(input$tr_columns)) {
	    if(is.null(inChecker(input$tr_columns))) return()
      # select_ needs .dots when passed a vector of variable name strings
 			dat <- select_(dat, .dots = input$tr_columns)
		} else {
			return()
		}
	}

	if(input$tr_changeType == 'reorder_cols') {
    if(is.null(input$tr_reorder_cols)) {
      ordVars <- colnames(dat)
 	  } else {
   	  ordVars <- input$tr_reorder_cols
    }
 	  return(dat[,ordVars, drop = FALSE])
  }

	if(input$tr_changeType == 'na.remove') {
		if(!is.null(input$tr_columns)) {
			return(dat[complete.cases(dat[,input$tr_columns]),])
		} else {
	 	  return(na.omit( dat ))
		}
  }

	if(input$tr_changeType == 'sub_filter') {
	  if(input$tr_subset != '') {
	    selcom <- input$tr_subset
    	seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)

    	if(!is(seldat, 'try-error')) {
      	if(is.data.frame(seldat)) {
        	return(seldat)
      	}
    	}
  	}
  }

	if(!is.null(input$tr_columns)) {
		if(!all(input$tr_columns %in% colnames(dat))) return()
		dat <- select_(dat, .dots = input$tr_columns)
    vars <- colnames(dat)

		if(input$tr_transfunction != 'none') {
      dat_tr <- try(dat %>% mutate_each_(input$tr_transfunction, vars), silent = TRUE)
      # if(is(dat_tr, 'try-error')) dat_tr <- dat * NA

      if(is(dat_tr, 'try-error')) return(attr(dat_tr,"condition")$message)

  		cn <- c(vars,paste(input$tr_transfunction,vars, sep="_"))
			dat <- cbind(dat,dat_tr)
			colnames(dat) <- cn
		}
		if(input$tr_typefunction != 'none') {
      #	dat <- colwise(input$tr_typefunction)(dat)
      dat <- mutate_each_(dat,input$tr_typefunction, vars)
		}
    if(!is.null(input$tr_normalizer) && input$tr_normalizer != 'none') {

      dat_class <- getdata_class_fun(dat)
      isNum <- "numeric" == dat_class | "integer" == dat_class
      if(length(isNum) == 0) return("Please select numerical variables to normalize")
      dat_tr <- try(select(dat,which(isNum)) / getdata()[,input$tr_normalizer], silent = TRUE)
      if(is(dat_tr, 'try-error')) return(attr(dat_tr,"condition")$message)
     	cn <- c(vars,paste(vars[isNum],input$tr_normalizer, sep="_"))
			dat <- cbind(dat,dat_tr)
			colnames(dat) <- cn
 		}
	} else {
		if(!input$tr_changeType %in% c("", "sub_filter", "create", "clip")) return()
	}

	if(!is.null(input$tr_columns) & input$tr_changeType == 'reorder_levs') {
    if(!is.null(input$tr_reorder_levs)) {
    	isFct <- "factor" == getdata_class()[input$tr_columns[1]]
		  if(isFct) dat[,input$tr_columns[1]] <- factor(dat[,input$tr_columns[1]], levels = input$tr_reorder_levs)
    }
  }

	if(input$tr_changeType ==  'recode') {
		if(input$tr_recode != '') {

			recom <- input$tr_recode
			recom <- gsub("\"","\'", recom)

			newvar <- try(do.call(car::recode, list(dat[,input$tr_columns[1]],recom)), silent = TRUE)
			if(!is(newvar, 'try-error')) {

				cn <- c(colnames(dat),paste("rc",input$tr_columns[1], sep="."))
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
			if(sum(cpname %in% colnames(dat)) > 0) names(cpdat) <- paste('cp',cpname,sep = '.')
			if(is.null(input$tr_columns)) return(cpdat)
			if(nrow(cpdat) == nrow(dat)) dat <- cbind(dat,cpdat)
		}
	}

	if(input$tr_changeType == 'rename') {
		if(!is.null(input$tr_columns) && input$tr_rename != '') {
			rcom <- unlist(strsplit(gsub(" ","",input$tr_rename), ","))
			rcom <- rcom[1:min(length(rcom),length(input$tr_columns))]
			names(dat)[1:length(rcom)] <- rcom
		}
	}

	if(input$tr_changeType == 'create') {
		if(input$tr_transform != '') {
			recom <- input$tr_transform
			recom <- gsub("\"","\'", recom)

			fullDat <- getdata()
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
	isDate <- "date" == dat_class
	isChar <- "character" == dat_class
	isLogic <- "logical" == dat_class

	if(sum(isNum) > 0) {
		cat("Summarize numeric variables:\n")
  	dat[,isNum] %>% psych::describe(.,na.rm=T) %>%
      select_(.dots = c("n","mean","median","min","max","sd", "se","skew","kurtosis")) -> res

		# adding Q1 and Q3
		perc25 <- function(x) quantile(x,.25, na.rm = TRUE)
		perc75 <- function(x) quantile(x,.75, na.rm = TRUE)

    res$`25%` <- select(dat, which(isNum)) %>% summarise_each(funs(perc25)) %>% t
    res$`75%` <- select(dat, which(isNum)) %>% summarise_each(funs(perc75)) %>% t
    # nmissing function is in explore.R
    res$missing <- select(dat, which(isNum)) %>% summarise_each(funs(nmissing)) %>% t

		# print desired stats in order
		res[,c("n","mean","median","25%","75%","min","max","sd","se","skew","kurtosis","missing")] %>% print
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
		if(is.null(dat)) return()
		if(is.character(dat)) return(dat)

		if(input$tr_changeType == 'remove') {
	  	values[[input$datasets]][,colnames(dat)] <- list(NULL)
	  } else if(input$tr_changeType == 'type') {
	  	values[[input$datasets]][,colnames(dat)] <- dat
		} else if(input$tr_changeType == 'na.remove') {
	  	values[[input$datasets]] <- dat
		} else if(input$tr_changeType == 'sub_filter') {
	  	values[[input$datasets]] <- dat
		} else if(input$tr_changeType == 'rename') {
			changedata_names(input$tr_columns, colnames(dat))
		} else if(input$tr_changeType == 'reorder_cols') {
	  	values[[input$datasets]] <- values[[input$datasets]][,input$tr_reorder_cols]
	  } else {
			changedata(dat, colnames(dat))
		}

		# reset input values once the changes have been applied
	 	updateTextInput(session = session, inputId = "tr_transform", label = "Create (e.g., y = x - z):", '')
	 	updateTextInput(session = session, inputId = "tr_recode", label = "Recode (e.g., lo:20 = 1):", '')
	 	updateTextInput(session = session, inputId = "tr_rename", label = "Rename (separate by ','):", value = '')
	 	updateTextInput(session = session, inputId = "tr_copyAndPaste", label = "", '')
#     updateSelectInput(session = session, inputId = "tr_normalizer", choices = as.list(varnames()),
#                       selected = "none")
		updateSelectInput(session = session, inputId = "tr_transfunction", choices = trans_options,
                      selected = "none")

    isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
    vars <- c("None" = "none", varnames()[isNum])
    updateSelectInput(session = session, inputId = "tr_normalizer", label = "Normalizing variable:", choices = vars,
                      selected = "none")
  })
})
