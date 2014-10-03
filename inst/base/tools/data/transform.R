# UI-elements for transform
output$uiTr_columns <- renderUI({
	cols <- varnames()
	selectInput("tr_columns", "Select column(s):", choices  = as.list(cols), selected = NULL, multiple = TRUE, selectize = FALSE)
})

output$uiTr_reorder_levs <- renderUI({
	# if(is.null(input$tr_columns)) return()
	isFct <- "factor" == getdata_class()[input$tr_columns[1]]
  if(!isFct) return()
  dat <- getdata()
  returnOrder("tr_reorder_levs", levels(dat[,input$tr_columns[1]]))
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

shift <- function(x,shift_by){
	# from http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))

  if (length(shift_by) > 1)
    return(sapply(shift_by,shift, x = x))

  # prefer to have positive number create lags as normal in ts-literature
  shift_by <- -shift_by

  out <- NULL
  abs_shift_by = abs(shift_by)
  if (shift_by > 0)
    out <- c(tail(x,-abs_shift_by),rep(NA,abs_shift_by))
  else if (shift_by < 0)
    out <- c(rep(NA,abs_shift_by), head(x,-abs_shift_by))
  else
    out <- x
  out
}

sq <<- function(x) x^2
inv <<- function(x) 1/x
st <<- standardize_1sd
cent <<- centerVar
msp <<- medianSplit
dec <<- decileSplit
# lagx <<- shift

d_mdy <<- function(x) as.Date(mdy(as.character(x)))
d_dmy <<- function(x) as.Date(dmy(as.character(x)))
d_ymd <<- function(x) as.Date(ymd(as.character(x)))

trans_options <- list("None" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt",
	"Center" = "cent", "Standardize" = "st", "Invert" = "inv", "Median split" = "msp", "Deciles" = "dec")

type_options <- list("None" = "", "As factor" = "as.factor",  "As number" = "as.numeric", "As integer" = "as.integer",
	"As character" = "as.character", "As date (mdy)" = "d_mdy", "As date (dmy)" = "d_dmy", "As date (ymd)" = "d_ymd")

trans_types <- list("None" = "", "Type" = "type", "Change" = "change", "Create" = "create", "Clipboard" = "clip",
	"Recode" = "recode", "Rename" = "rename", "Reorder columns" = "reorder_cols", "Reorder levels" = "reorder_levs",
	"Remove columns" = "remove", "Remove missing" = "na.remove", "Subset" = "sub_filter")

output$ui_Transform <- renderUI({

	# Inspired by Ian Fellow's transform ui in JGR/Deducer
  list(wellPanel(
    uiOutput("uiTr_columns"),
    selectInput("tr_changeType", "Transformation type:", trans_types, selected = ""),
    conditionalPanel(condition = "input.tr_changeType == 'type'",
	    selectInput("tr_typefunction", "Change variable type:", type_options, selected = "")
    ),
    conditionalPanel(condition = "input.tr_changeType == 'change'",
	    selectInput("tr_transfunction", "Apply function:", trans_options)
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
	    actionButton("addtrans", "Save changes")
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
		helpModal('Transform','transformHelp',inclRmd("../base/tools/help/transform.Rmd"))

		# Reporting option not yet available for transform
 		# helpAndReport('Explore','explore',inclRmd("tools/help/explore.Rmd"))
	)
})

transform_main <- reactive({

	# if(input$datatabs != 'Transform') return()
	# if(is.null(input$datasets)) return()
	# if(is.null(input$tr_changeType) || input$tr_changeType == '') return()
	if(is.null(input$tr_changeType)) return()

	dat <- getdata()

	if(input$tr_changeType == "") {
		if(!is.null(input$tr_columns)) {
			# if(!all(input$tr_columns %in% colnames(dat))) return()
			dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
		} else {
			return()
		}
		# return(dat)
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

	# if(input$tr_changeType == "") {
	# if(!is.null(input$tr_columns)) {
	if(!is.null(input$tr_columns)) {

		if(!all(input$tr_columns %in% colnames(dat))) return()
		dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
		if(input$tr_transfunction != '') {
			cn <- c(colnames(dat),paste(input$tr_transfunction,colnames(dat), sep="."))

			# This might work
			# lapply(dat,function(x) x - mean(x))
			# cbind(dat,z)

			dat <- cbind(dat,colwise(input$tr_transfunction)(dat))

			colnames(dat) <- cn
		}
		if(input$tr_typefunction != '') {
			# dat <- cbind(dat,colwise(input$tr_typefunction)(dat))
			dat <- colwise(input$tr_typefunction)(dat)
		}
	} else {
		# if(input$tr_changeType != "" && input$tr_changeType != "sub_filter") return()
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

	nr <- min(nrow(dat),5)
	dat <- dat[1:nr,, drop = FALSE]

	dat <- data.frame(date2character_dat(dat))

	html <- print(xtable::xtable(dat), type='html', print.results = FALSE)
  html <- paste(html, '<label>5 rows shown. See View-tab for details.</label>')
  html <- sub("<table border=1>","<table class='table table-condensed table-hover'>", html)
  # html <- sub("<table border=\"1\">","<table class='table table-condensed table-hover'>", html)
  Encoding(html) <- 'UTF-8'
  html

})

output$transform_summary <- renderPrint({

	# if(isolate(input$datatabs) != 'Transform') return(invisible())

	dat <- transform_main()
	if(is.null(dat)) return(invisible()) 			# ...

	isFct <- sapply(dat, is.factor)
	isNum <- sapply(dat, is.numeric)
	isDate <- sapply(dat, is.Date)
	isChar <- sapply(dat, is.character)
	isLogic <- sapply(dat, is.logical)

	if(sum(isNum) > 0) {
		cat("Summarize numeric variables:\n")
		# print(describe(dat[,isNum])[,c("n","mean","median","min","max","range","sd","se","skew","kurtosis")])
		res <- data.frame(describe(dat[isNum])[,c("n","mean","median","min","max","sd","se","skew","kurtosis")])
		res$missing <- c(colwise(nmissing)(dat[,isNum, drop = FALSE]))
		print(res)
		cat("\n")
	}
	if(sum(isFct) > 0) {
		cat("Summarize factors:\n")
		print(summary(dat[,isFct]))
		cat("\n")
	}
	if(sum(isDate) > 0) {
		cat("Earliest dates:\n")
		print(colwise(min)(dat[,isDate]))
		cat("\nFinal dates:\n")
		print(colwise(max)(dat[,isDate]))
		cat("\n")
	}
	if(sum(isChar) > 0) {
		cat("Summarize character variables:\n")
		print(table(dat[,isChar]))
		cat("\n")
	}
	if(sum(isLogic) > 0) {
		cat("Summarize logical variables:\n")
		print(table(dat[,isLogic]))
		cat("\n")
	}
})

observe({
	if(is.null(input$addtrans) || input$addtrans == 0) return()
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
		updateSelectInput(session = session, inputId = "tr_transfunction", choices = trans_options, selected = "None")
	})
})
