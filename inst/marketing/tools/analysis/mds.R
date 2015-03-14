###############################
# Multidimensional scaling
###############################
output$uiMds_id1 <- renderUI({
	# isChar <- "character" == getdata_class()
	isLabel <- "character" == getdata_class() | "factor" == getdata_class()
  # vars <- varnames()[isChar]
  vars <- varnames()[isLabel]
  # if(length(vars) == 0) return(HTML('<label>This dataset has no variables of type character.</label>'))
  selectInput(inputId = "mds_id1", label = "ID 1:", choices = vars,
   	selected = state_single("mds_id1",vars), multiple = FALSE)
})

output$uiMds_id2 <- renderUI({

  # if(is.null(input$mds_id1) || !input$mds_id1 %in% varnames()) return()
  if(input$mds_id1 %>% not_available) return()

	isLabel <- "character" == getdata_class() | "factor" == getdata_class()
  vars <- varnames()[isLabel]
  if(length(vars) > 0) vars <- vars[-which(vars == input$mds_id1)]
  # if(length(vars) == 0) return(HTML('<label>This dataset has only one variable of type character.</label>'))
  selectInput(inputId = "mds_id2", label = "ID 2:", choices = vars,
   	selected = state_single("mds_id2",vars), multiple = FALSE)
})

output$uiMds_dis <- renderUI({
  # if(is.null(input$mds_id2) || !input$mds_id2 %in% varnames()) return()
  if(input$mds_id2 %>% not_available) return()

 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  # if(length(vars) == 0) return()
  selectInput(inputId = "mds_dis", label = "Dissimilarity:", choices = vars,
   	selected = state_single("mds_dis",vars), multiple = FALSE)
})

output$uiMds_rev_dim <- renderUI({
	rev_list <- list()
	rev_list[paste("Dim",1:input$mds_dim_number)] <- 1:input$mds_dim_number
	checkboxGroupInput("mds_rev_dim", "Reverse:", rev_list,
   	selected = state_init("mds_rev_dim"),
   	inline = TRUE)
})

mds_dim_number <- c("2-dims" = 2, "3-dims" = 3)
mds_non_metric <- c("metric" = "metric", "non-metric" = "non-metric")

output$ui_mds <- renderUI({
  list(
  	wellPanel(
	  	uiOutput("uiMds_id1"),
	  	uiOutput("uiMds_id2"),
	  	uiOutput("uiMds_dis"),
		  radioButtons(inputId = "mds_non_metric", label = NULL, mds_non_metric,
		   	selected = state_init("mds_non_metric", "metric"),
		   	inline = TRUE),
		  radioButtons(inputId = "mds_dim_number", label = NULL, mds_dim_number,
		   	selected = state_init("mds_dim_number", 2),
		   	inline = TRUE),
	 	 	conditionalPanel(condition = "input.tabs_mds == 'Plots'",
	 	 		numericInput("mds_fontsz", "Font size:", state_init("mds_fontsz",1.3), .5, 4, .1),
		  	uiOutput("uiMds_rev_dim")
	    )
	  ),
		helpAndReport('(Dis)similarity based maps (MDS)','mds',inclMD("tools/help/mds.md"))
	)
})

mds_plotWidth <- function() {
	result <- .mds()
	ifelse(is.list(result), return(result$plotWidth), return(650))
}

mds_plotHeight <- function() {
	result <- .mds()
	ifelse(is.list(result), return(result$plotHeight), return(650))
}

output$mds <- renderUI({
  statTabPanel("Maps","(Dis)similarity",".mds", "mds", "mds_plotWidth", "mds_plotHeight")
})

.mds <- reactive({

	# ret_text <- "This analysis requires two id-variables of type character\nand a measure of dissimilarity of type numeric or interval.\nPlease select another dataset."
	# if(is.null(input$mds_id2) || is.null(input$mds_dis)) return(ret_text)

	# if(is.null(inChecker(c(input$mds_id1, input$mds_id2, input$mds_dis)))) return(ret_text)
	if( c(input$mds_id1, input$mds_id2, input$mds_dis) %>% not_available )
		return("This analysis requires two id-variables of type character or factor and a measure\nof dissimilarity of type numeric or interval. Please select another dataset")

	mds(input$dataset, input$mds_id1, input$mds_id2, input$mds_dis, input$mds_rev_dim,
			input$mds_non_metric, input$mds_dim_number, input$mds_fontsz)
})

observe({
  if(is.null(input$mdsReport) || input$mdsReport == 0) return()
  isolate({
		inp <- list(input$dataset, input$mds_id1, input$mds_id2, input$mds_dis, input$mds_rev_dim,
			input$mds_non_metric, input$mds_dim_number, input$mds_fontsz)
		updateReport(inp,"mds", round(7 * mds_plotWidth()/650,2), round(7 * mds_plotHeight()/650,2))
  })
})

mds <- function(dataset, mds_id1, mds_id2, mds_dis, mds_rev_dim,
                mds_non_metric, mds_dim_number, mds_fontsz) {

	dat <- r_data[[dataset]]

	nr.dim <- as.numeric(mds_dim_number)

	# dat <- read.csv("~/Desktop/Car Survey MDS 2015.csv")
	# mds_id1 <- "ID.1"
	# mds_id2 <- "ID.2"
	# mds_dis <- "Distance"
	# nr.dim <- 2
	# library(dplyr)

	dis <- dat[,mds_dis]
	id1 <- dat[,mds_id1] %>% as.character
	id2 <- dat[,mds_id2] %>% as.character

	lab <- unique(c(id1,id2))
	nr.lev <- length(lab)

	lower <- (nr.lev * (nr.lev -1)) / 2
	nr.obs <- length(dis)

	co.dist <- diag(length(lab))
	if(lower == nr.obs) {
		co.dist[lower.tri(co.dist, diag = FALSE)] <- dis
	} else if((lower + nr.lev) == nr.obs) {
		co.dist[lower.tri(co.dist, diag = TRUE)] <- dis
	} else {
		return("Number of observations and unique IDs for the brand variable do not match.\nPlease choose another brand variable or another dataset.")
	}

	rownames(co.dist) <- lab
	colnames(co.dist) <- lab
	co.dist.mat <- as.dist(co.dist)

	set.seed(1234)

	###############################################
	# Try metaMDS - requires vegan
	###############################################
	# co.mds <- suppressWarnings(metaMDS(co.dist.mat, k = nr.dim, trymax = 500))
	# if(co.mds$converged == FALSE) return("The MDS algorithm did not converge. Please try again.")

	co.mds <- MASS::isoMDS(co.dist.mat, k = nr.dim, trace = FALSE)
	co.mds$stress <- co.mds$stress / 100

	if(mds_non_metric == "metric") {
		co.mds$points <- cmdscale(co.dist.mat, k = nr.dim)
		cmdscale
		# Using R^2
		# co.mds$stress <- sqrt(1 - cor(dist(co.mds$points),co.dist.mat)^2) * 100
		# Using standard Kruskal formula for metric MDS
		co.mds$stress	<- { sum((dist(co.mds$points) - co.dist.mat)^2) / sum(co.dist.mat^2) } %>%
											 sqrt
	}

	out <- list()
	out$nr.dim <- nr.dim
	out$data <- co.dist.mat
	out$points <- co.mds$points
	out$labels <- lab
	out$nr.levels <- nr.lev
	out$lim <- max(abs(out$points))
	out$mds_non_metric <- mds_non_metric
	out$mds_rev_dim <- as.numeric(mds_rev_dim)
	out$mds_fontsz <- mds_fontsz

	nr.plots <- (nr.dim * (nr.dim - 1)) / 2
	plotHeight <- 650 * nr.plots
	plotWidth <- 650

	return(list('co.mds' = co.mds, 'co.dist.mat' = co.dist.mat, 'out' = out, 'plotHeight' = plotHeight,
		'plotWidth' = plotWidth))
}

summary_mds <- function(result = .mds()) {

	cat("Distance data:\n")
	result$co.dist.mat %>% round(3) %>% print
	cat("\nCoordinates:\n")
	co.mds <- result$co.mds
	colnames(co.mds$points) <- paste("Dim ", 1:ncol(co.mds$points))
	co.mds$points %>% round(3) %>% print

	cat("\nStress:", sprintf("%.2f", co.mds$stress))
}

plots_mds <- function(result = .mds()) {

	out <- result$out

	if(out$nr.dim == 3) {
		op <- par(mfrow=c(3,1))
	} else {
		op <- par(mfrow=c(1,1))
	}

	if(!is.null(out$mds_rev_dim)) {
		dim2rev <- out$mds_rev_dim
		out$points[,dim2rev] <- -1 * out$points[,dim2rev]
	}

	for(i in 1:(out$nr.dim-1)) {
		for(j in (i+1):out$nr.dim) {
			plot(c(-out$lim,out$lim),type = "n",xlab='', ylab='', axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-out$lim, out$lim), xlim=c(-out$lim,out$lim))
			title(paste("Dimension",i,"vs Dimension",j), cex.main = out$mds_fontsz)
			points(out$points[,i], out$points[,j], pch = 16, cex = .6)

			# text(out$points[,i], out$points[,j], out$labels, col=rainbow(out$nr.lev,start=.6,end=.1), adj = c(0.4,-.4))
			wordclould::textplot(out$points[,i], out$points[,j]+(.04*out$lim), out$labels, col=rainbow(out$nr.lev,start=.6,end=.1),
				cex = out$mds_fontsz, new = FALSE)
			abline(v=0, h=0)
		}
	}

	par(op)

}
