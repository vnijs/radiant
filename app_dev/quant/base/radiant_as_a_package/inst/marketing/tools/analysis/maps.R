###############################
# Multidimensional scaling
###############################
output$uiMds_id1 <- renderUI({
	isChar <- "character" == getdata_class()
  vars <- varnames()[isChar]
  if(length(vars) == 0) return(HTML('<label>This dataset has no variables of type character.</label>'))
  selectInput(inputId = "mds_id1", label = "ID 1:", choices = vars, 
  	# selected = names(vars[vars == values$mds_id1]), multiple = FALSE)
   	selected = state_singlevar("mds_id1",vars), multiple = FALSE)
})

output$uiMds_id2 <- renderUI({

  if(is.null(input$mds_id1) || !input$mds_id1 %in% varnames()) return()

	isChar <- "character" == getdata_class()
  vars <- varnames()[isChar]
	vars <- vars[-which(vars == input$mds_id1)]
  if(length(vars) == 0) return(HTML('<label>This dataset has only one variable of type character.</label>'))
  selectInput(inputId = "mds_id2", label = "ID 2:", choices = vars, 
  	# selected = names(vars[vars == values$mds_id2]), multiple = FALSE)
   	selected = state_singlevar("mds_id2",vars), multiple = FALSE)
})

output$uiMds_dis <- renderUI({
  if(is.null(input$mds_id2) || !input$mds_id2 %in% varnames()) return()

 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "mds_dis", label = "Dissimilarity:", choices = vars, 
  	# selected = names(vars[vars == values$mds_dis]), multiple = FALSE)
   	selected = state_singlevar("mds_dis",vars), multiple = FALSE)
})

output$uiMds_rev_dim <- renderUI({
	rev_list <- list()
	rev_list[paste("Dimension",1:input$mds_dim_number)] <- 1:input$mds_dim_number
	checkboxGroupInput("mds_rev_dim", "Reverse:", rev_list, 
		# selected = names(rev_list[rev_list == values$mds_rev_dim]))
   	selected = state_init_list("mds_rev_dim","", rev_list))
})

mds_dim_number <- c("2-dims" = 2, "3-dims" = 3)

output$ui_mds <- renderUI({
  list(
  	wellPanel(
	  	uiOutput("uiMds_id1"),
	  	uiOutput("uiMds_id2"),
	  	uiOutput("uiMds_dis"),
		  radioButtons(inputId = "mds_dim_number", label = "", mds_dim_number, 
		  	# selected = values$mds_dim_number),
		   	selected = state_init_list("mds_dim_number",2, mds_dim_number)),
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

	ret_text <- "This analysis requires two id-variables of type character\nand a measure of dissimilarity of type numeric or interval.\nPlease select another dataset."
	if(is.null(input$mds_id2) || is.null(input$mds_dis)) return(ret_text)
	# if(is.null(inChecker(c(input$mds_id1, input$mds_id2, input$mds_dis)))) return(ret_text)

	mds(input$datasets, input$mds_id1, input$mds_id2, input$mds_dis, input$mds_rev_dim,
			input$mds_dim_number, input$mds_fontsz)
})

observe({
  if(is.null(input$mdsReport) || input$mdsReport == 0) return()
  isolate({
		inp <- list(input$datasets, input$mds_id1, input$mds_id2, input$mds_dis, input$mds_rev_dim,
			input$mds_dim_number, input$mds_fontsz)
		updateReport(inp,"mds", round(7 * mds_plotWidth()/650,2), round(7 * mds_plotHeight()/650,2))
  })
})

mds <- function(datasets, mds_id1, mds_id2, mds_dis, mds_rev_dim, mds_dim_number, mds_fontsz) {

	dat <- values[[datasets]]

	nr.dim <- as.numeric(mds_dim_number)

	dis <- dat[,mds_dis]
	id1 <- dat[,mds_id1]
	id2 <- dat[,mds_id2]

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
		return("Number of observations and unique id's does not match.")
	}

	rownames(co.dist) <- lab
	colnames(co.dist) <- lab
	co.dist.mat <- as.dist(co.dist)

	set.seed(1234)

	###############################################
	# Try metaMDS
	###############################################
	# co.mds <- suppressWarnings(metaMDS(co.dist.mat, k = nr.dim, trymax = 500))
	# if(co.mds$converged == FALSE) return("The MDS algorithm did not converge. Please try again.")

	co.mds <- isoMDS(co.dist.mat, k = nr.dim, trace = FALSE)

	out <- list()
	out$nr.dim <- nr.dim
	out$data <- co.dist.mat
	out$points <- co.mds$points
	out$labels <- lab
	out$nr.levels <- nr.lev
	out$lim <- max(abs(out$points))
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
	print(result$co.dist.mat, digits = 3)
	cat("\nCoordinates:\n")
	co.mds <- result$co.mds
	coor <- co.mds$points
	colnames(coor) <- paste("Dim ", 1:ncol(coor))
	print(coor, digits = 3)

	cat("\nFinal stress measure equal to", sprintf("%.3f", co.mds$stress/100))
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
			textplot(out$points[,i], out$points[,j]+(.04*out$lim), out$labels, col=rainbow(out$nr.lev,start=.6,end=.1), 
				cex = out$mds_fontsz, new = FALSE)
			abline(v=0, h=0)
		}
	}

	par(op)


	###############################################
	# move this over to ggplot when you have time
	###############################################

	# plots <- list()
	# for(var in input$km_vars) {
	# 	plots[[var]] <- ggplot(dat, aes_string(x=var, fill='clusvar')) + geom_density(adjust=1.5, alpha=.3) 
	# }

	# dat <- data.frame(out$points)
	# colnames(dat) <- paste0('Dimension',1:out$nr.dim)
	# dim1 <- 'Dimension1'
	# dim2 <- 'Dimension2'
	# p <- ggplot(dat, aes_string(x=dim1, y=dim2)) + geom_point(shape = 1) + 
	# 			theme(axis.text.x = element_blank(), axis.text.y = element_blank())
	# print(p)

  # p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_point()
 	# p <- p + aes_string(color=input$viz_color) + scale_fill_brewer()
	# print(do.call(grid.arrange, c(plots, list(ncol = min(length(plots),2)))))
}
