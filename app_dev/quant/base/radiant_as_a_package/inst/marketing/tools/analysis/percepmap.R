#########################################
# Perceptual map using factor analysis
#########################################
output$uiPmap_brand <- renderUI({

	isChar <- "character" == getdata_class()
  vars <- varnames()[isChar]
  if(length(vars) == 0) return()
 	selectInput(inputId = "pmap_brand", label = "Brand:", choices = vars, 
   	selected = state_singlevar("pmap_brand",vars), multiple = FALSE)
})

output$uiPmap_attr <- renderUI({
  if(is.null(input$pmap_brand)) return()
  # if(is.null(input$pmap_brand) || is.null(inChecker(c(input$pmap_brand)))) return()

 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  if(length(vars) == 0) return()
  selectInput(inputId = "pmap_attr", label = "Attributes:", choices = vars, 
   	selected = state_multvar("pmap_attr",vars), multiple = TRUE, selectize = FALSE)
})

output$uiPmap_pref <- renderUI({
  if(is.null(input$pmap_attr)) return()
  # if(is.null(inChecker(c(input$pmap_pref)))) return()

 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
 	vars <- vars[-which(vars %in% input$pmap_attr)]
  if(length(vars) == 0) return()

  selectInput(inputId = "pmap_pref", label = "Preferences:", choices = vars, 
   	selected = state_multvar("pmap_pref",vars), multiple = TRUE, selectize = FALSE)
})

output$uiPmap_plot <- renderUI({
	plot_list <- c("Brands" = "brand", "Attributes" = "attr")
  if(!is.null(input$pmap_pref)) plot_list <- c(plot_list, c("Preferences" = "pref"))
	checkboxGroupInput("pmap_plot", "", plot_list,
   	selected = state_init_list("pmap_plot","", plot_list))
})

pmap_dim_number <- c("2-dims" = 2, "3-dims" = 3)

output$ui_pmap <- renderUI({
  list(
  	wellPanel(
	  	uiOutput("uiPmap_brand"),
	  	uiOutput("uiPmap_attr"),
	  	uiOutput("uiPmap_pref"),
	 	 	conditionalPanel(condition = "input.tabs_pmap == 'Plots'",
		  	uiOutput("uiPmap_plot"),
	 	    div(class="row-fluid",
		    	div(class="span6", numericInput("pmap_scaling", "Arrow scaling:", state_init("pmap_scaling",2.1), .5, 4, .1)),
		      div(class="span6", numericInput("pmap_fontsz", "Font size:", state_init("pmap_fontsz",1.3), .5, 4, .1))
		    )
	    ),
		  radioButtons(inputId = "pmap_dim_number", label = "", pmap_dim_number, 
		   	selected = state_init_list("pmap_dim_number",2, pmap_dim_number)),
	 	 	conditionalPanel(condition = "input.tabs_pmap == 'Summary'",
	    	numericInput("pmap_cutoff", label = "Loadings cutoff:", min = 0, max = 1, 
	    		state_init("pmap_cutoff",0), step = .05),
		    actionButton("pmap_savescores", "Save scores")
		  )
 		),
		helpAndReport('Attribute based maps','pmap',inclMD("tools/help/percepmap.md"))
	)
})

pmap_plotWidth <- function() {
	result <- .pmap()
	ifelse(is.list(result), return(result$plotWidth), return(650))
}

pmap_plotHeight <- function() {
	result <- .pmap()
	ifelse(is.list(result), return(result$plotHeight), return(650))
}

output$pmap <- renderUI({
  statTabPanel("Maps","Attributes",".pmap", "pmap", "pmap_plotWidth", "pmap_plotHeight")
}) 

.pmap <- reactive({

	ret_text <- "This analysis requires a brand variable of type character\nand multiple attribute variables of type numeric or integer.\nPlease select another dataset."
	if(is.null(input$pmap_brand)) return(ret_text)
	# if(is.null(inChecker(c(input$pmap_brand, input$pmap_attr)))) return(ret_text)
	if(length(input$pmap_attr) < 2) return("Please select two or more attribute variables")

	pmap(input$datasets, input$pmap_brand, input$pmap_attr, input$pmap_pref, input$pmap_dim_number, 
		input$pmap_scaling, input$pmap_fontsz, input$pmap_cutoff, input$pmap_plot)
})

observe({
  if(is.null(input$pmapReport) || input$pmapReport == 0) return()
  isolate({
		inp <- list(input$datasets, input$pmap_brand, input$pmap_attr, input$pmap_pref, 
			input$pmap_dim_number, input$pmap_scaling, input$pmap_fontsz, input$pmap_cutoff, input$pmap_plot)

		# extra command to save factor scores
		xcmd <- paste0("savePmapFactorScores(result)")
		updateReport(inp,"pmap", round(7 * pmap_plotWidth()/650,2), round(7 * pmap_plotHeight()/650,2), xcmd = xcmd)
  })
})

pmap <- function(datasets, pmap_brand, pmap_attr, pmap_pref, pmap_dim_number, pmap_scaling, 
	pmap_fontsz, pmap_cutoff, pmap_plot) {

	dat <- values[[datasets]]

	nr.dim <- as.numeric(pmap_dim_number)
	nr.attr <- length(pmap_attr)

	f.data <- dat[,pmap_attr]
	brands <- dat[,pmap_brand]

	f.res <- suppressWarnings(principal(f.data, nfactors=nr.dim, rotate='varimax', scores=FALSE, oblique.scores=FALSE))
	df <- as.data.frame(f.res$loadings[,colnames(f.res$loadings)])
	m <- as.matrix(df)
	mPm <- (t(m) %*% m)
	smPm <- solve(mPm)
	cscm <- m %*% smPm
	mcar <- as.matrix(f.data)
	scores <- scale(mcar, center = TRUE, scale = TRUE) %*% cscm
	rownames(scores) <- brands

	pc <- std.pc <- 0
	if(!is.null(pmap_pref)) {
		pc <- data.frame(cor(dat[,pmap_pref],scores))
		pc$communalities <- rowSums(pc^2)
		rownames(pc) <- pmap_pref
	}

	out <- list()
	out$nr.dim <- nr.dim
	out$scores <- scores
	out$loadings <- m
	out$pref_cor <- pc 
	out$brand_names <- brands
	out$pmap_attr <- pmap_attr
	out$pmap_pref <- pmap_pref
	out$pmap_scaling <- as.numeric(pmap_scaling)
	out$pmap_fontsz <- pmap_fontsz
	out$pmap_cutoff <- pmap_cutoff
	out$pmap_plot <- pmap_plot

	nr.plots <- (nr.dim * (nr.dim - 1)) / 2
	plotHeight <- 650 * nr.plots
	plotWidth <- 650

	return(list('f.res' = f.res, 'out' = out, 'plotHeight' = plotHeight, 'plotWidth' = plotWidth))
}

summary_pmap <- function(result = .pmap()) {

	cat("-- Attribute based perceptual map --\n")
	f.res <- result$f.res
	out <- result$out

	cat("\nBrand - Factor scores:\n")
	print(round(out$scores,2), digits = 2)

	cat("\nAttribute - Factor loadings:\n")
	print(f.res$loadings, cutoff = out$pmap_cutoff, digits = 2)
	
	if(!is.null(out$pmap_pref)) {
		cat("\nPreference correlations:\n")
		print(round(out$pref_cor,2), digits = 2)
	}

	communalities <- as.data.frame(f.res$communality)
	colnames(communalities) <- "communalities"
	cat("\nAttribute communalities:\n")
	print(round(communalities,2), digits = 2)
}

plots_pmap <- function(result = .pmap()) {

	out <- result$out

	std_pc <- out$pmap_scaling * out$pref_cor
	std_m <- out$pmap_scaling * out$loadings
	std_scores <- out$scores
	lab_buf <- 1.1

	max.max <- max(abs(std_m),abs(std_scores)) * 1.1	# adding a buffer so the labels don't move off the screen

	if(out$nr.dim == 3) {
		op <- par(mfrow=c(3,1))
	} else {
		op <- par(mfrow=c(1,1))
	}


	for(i in 1:(out$nr.dim-1)) {
			for(j in (i+1):out$nr.dim) {

				plot(c(-max.max,max.max),type = "n",xlab='', ylab='', axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-max.max, max.max), xlim=c(-max.max,max.max))
				title(paste("Dimension",i,"vs Dimension",j), cex.main = out$pmap_fontsz)
				abline(v=0, h=0)

				if("brand" %in% out$pmap_plot) {
					points(std_scores[,i], std_scores[,j], pch = 16, cex = .6)
					textplot(std_scores[,i], std_scores[,j]+(.04*max.max), out$brand_names, cex = out$pmap_fontsz, new = FALSE)
				}

				if("attr" %in% out$pmap_plot) {
					textplot(std_m[,i]*lab_buf, std_m[,j]*lab_buf, out$pmap_attr, cex = out$pmap_fontsz, col = "darkblue", new = FALSE)
					# add arrows
					for (k in out$pmap_attr) 
						arrows(0,0, x1=std_m[k,i], y1=std_m[k,j], lty='dashed', length=.05)

					# ideas
					# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_segment
					# http://docs.ggplot2.org/0.9.3.1/geom_abline.html
				}

				if("pref" %in% out$pmap_plot) {
					if(nrow(std_pc) > 1) {
						# textplot needs at least two coordinates
						textplot(std_pc[,i]*lab_buf, std_pc[,j]*lab_buf, out$pmap_pref, cex = out$pmap_fontsz, col="darkred", new = FALSE)
					} else {
						text(std_pc[,i]*lab_buf, std_pc[,j]*lab_buf, out$pmap_pref, cex = out$pmap_fontsz, col="darkred")
					}
					for (l in out$pmap_pref) {
						arrows(0,0, x1=std_pc[l,i], y1=std_pc[l,j], lty='dashed', col="red", length=.05)
					}
				}
			}
		}

	par(op)
}

savePmapFactorScores <- function(result = .pmap()) {
	facscores <- data.frame(result$out$scores)
	changedata(facscores, paste0("fac",1:ncol(facscores)))
}

# save factor scores when action button is pressed
observe({
	if(is.null(input$pmap_savescores) || input$pmap_savescores == 0) return()
	isolate({
		result <- .pmap()
		if(is.character(result)) return()
		savePmapFactorScores(result)
	})
})
