# UI-elements for transform
output$tr_columns <- renderUI({
	cols <- varnames()
	selectInput("tr_columns", "Select column(s) to transform:", choices  = as.list(cols), selected = NULL, multiple = TRUE)
})

output$tr_nrRows <- renderUI({
	if(is.null(input$datasets)) return()
	dat <- getdata()
	nr <- nrow(dat)
	sliderInput("tr_nrRows", "Rows to show (max 50):", min = 1, max = nr, value = min(15,nr), step = 1)
})

revFactorOrder <- function(x) {
	x <- as.factor(x)
	x <- factor(x, levels=rev(levels(x)))
	x
}

standardize_1sd <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(scale(x))
	return(x)
}

centerVar <- function(x) {
	if(is.factor(x)) return(rescale(x))
	if(is.numeric(x)) return(x - mean(x, na.rm = TRUE))
	return(x)
}

sq <<- function(x) x^2
inv <<- function(x) 1/x
st1 <<- standardize_1sd
st2 <<- rescale
cent <<- centerVar 
bin2 <<- function(x) cut(x,2)
bin10 <<- function(x) cut(x,10)
fct <<- as.factor
rfct <<- revFactorOrder
num <<- as.numeric
ch <<- as.character
d <<- as.Date
# trans_options <- list("Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Sum" = "sum", "Mean" = "mean", "Standardize" = "", "Center" = "" )
trans_options <- list("None" = "", "Log" = "log", "Square" = "sq", "Square-root" = "sqrt", "Center" = "cent", "Standardize (1-sd)" = "st1", 
	"Standardize (2-sd)" = "st2","Invert" = "inv", "Bin 2" = "bin2", "Bin10" = "bin10", "As factor" = "fct", "Rev factor order" = "rfct", "As number" = "num", "As character" = "ch", 
	"As date" = "d")

output$ui_transform <- renderUI({
	ui_transform()
})

ui_transform <- function() {
	# Inspired by Ian Fellow's transform ui in JGR/Deducer
  wellPanel(
  	uiOutput("tr_nrRows"), 
    uiOutput("tr_columns"),
    selectInput("tr_transfunction", "Transform columns", trans_options),
    # textInput("tr_recode", "Recode (e.g., ...))", ''), actionButton("tr_recode_sub", "Go"),
   	# tags$style(type='text/css', "#tr_recode { max-width: 135px; }"),
    # tags$style(type='text/css', "#tr_recode_sub { vertical-align: top; width: 45px; }"),
    textInput("tr_rename", "Rename (separate by ',')", ''),
   	tags$style(type='text/css', "#tr_rename { max-width: 185px; }"),
   	HTML("<label>Copy-and-paste data from Excel</label>"),
    tags$textarea(id="tr_copyAndPaste", rows=3, cols=40, ""),
    # tags$style(type='text/css', "#tr_copyAndPaste { onfocus=\"if(this.value != '') this.value='';\" onblur=\"if(this.value != '') this.value='';\""),
    # actionButton("transfix", "Edit variables in place") # using the 'fix(mtcars)' to edit the data 'inplace'. Looks great from R-ui, not so great from Rstudio
    actionButton("addtrans", "Save new variables")
  )
}

transform <- reactive({
	if(is.null(input$datasets) || (is.null(input$tr_columns) && input$tr_copyAndPaste == '')) return()
	if(input$datatabs != 'Transform') return()

	dat <- getdata()
	if(!is.null(input$tr_columns)) {

		if(!all(input$tr_columns %in% colnames(dat))) return()
		dat <- data.frame(dat[, input$tr_columns, drop = FALSE])
		if(input$tr_transfunction != '') {
			cn <- c(colnames(dat),paste(input$tr_transfunction,colnames(dat), sep="."))
			dat <- cbind(dat,colwise(input$tr_transfunction)(dat))
			colnames(dat) <- cn
		}
	}


	if(!is.null(input$tr_recode_sub) && !input$tr_recode_sub == 0) {
		isolate({
			if(input$tr_recode != '') {
				recom <- input$tr_recode
				recom <- gsub(" ", "", recom)
				if(nchar(recom) > 50) q()
				if(length(grep("system",recom)) > 0) q()
				if(length(grep("rm\\(list",recom)) > 0) q()
					
				# use sendmail from the sendmailR package	-- sendmail('','vnijs@rady.ucsd.edu','test','test')
				# first checking if recom is a valid expression
				# parse_recom <- try(parse(text = recom)[[1]], silent = TRUE)
				parse_recom <- try(parse(text = recom)[[1]], silent = FALSE)
				if(!is(parse_recom, 'try-error')) {

					# newvar <- try(eval(parse(text = paste("recode(",input$tr_columns[1],",",recom,")"))), silent = TRUE)
					print(parse(text = paste("recode(",input$tr_columns[1],",\"",recom,"\")")))

					newvarcom <- parse(text = paste("recode(",input$tr_columns[1],",\"",recom,"\")"))
					print(newvarcom)
					# newvar <- try(eval(parse(text = paste("recode(",input$tr_columns[1],",\"",recom,"\")"))), silent = FALSE)
					newvar <- eval(newvarcom)
					print(newvar)
					newvar <- try(eval(newvarcom), silent = FALSE)
					if(!is(newvar, 'try-error')) {

						cn <- c(colnames(dat),paste("rc",input$tr_columns[1], sep="."))
						dat <- cbind(dat,newvar)
						colnames(dat) <- cn

						return(dat)
					}


				} 
			}
		})
	}



	if(input$tr_copyAndPaste != '') {
		cpdat <- read.table(header=T, text=input$tr_copyAndPaste)
		cpname <- names(cpdat)
		if(cpname %in% colnames(dat)) names(cpdat) <- paste('cp',cpname,sep = '.')
		if(is.null(input$tr_columns)) return(cpdat)
		dat <- cbind(dat,cpdat)
	}

	if(input$tr_rename != '') {
		cvars <- input$tr_rename

		rcom <- unlist(strsplit(gsub(" ","",cvars), ","))
		names(dat)[names(dat)==input$tr_columns] <- rcom
	}

	dat
})

output$transform_data <- renderTable({
	if(is.null(input$datasets) || (is.null(input$tr_columns) && input$tr_copyAndPaste == '')) return()

	nr <- input$tr_nrRows
	dat <- data.frame(transform())
	dat[max(1,nr-50):nr,, drop = FALSE]
})

# output$transform_summary <- renderTable({
output$transform_summary <- renderPrint({
	if(is.null(input$datasets) || (is.null(input$tr_columns) && input$tr_copyAndPaste == '')) return(invisible())

	dat <- transform()
	if(is.null(dat)) return(invisible()) 			# when might this happen?

	isFct <- sapply(dat, is.factor)
	isNum <- sapply(dat, is.numeric)
	# isDate <- sapply(getdata(), is.Date)
	# isChar <- sapply(getdata(), is.character)

	if(sum(isNum) > 0) {
		cat("\nSummarize numeric variables:\n")
		print(describe(dat[isNum]))
	}
	if(sum(isFct) > 0) {
		cat("\nSummarize factors:\n")
		summary(dat[isFct])
	}
})

observe({
	if(is.null(input$addtrans) || input$addtrans == 0) return()
	isolate({
		dat <- transform()
		changedata(dat, colnames(dat))
	})

})


