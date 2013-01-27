# UI-elements for EDAT.R
output$sm_var <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "sm_var", label = "Variable (select one):", choices = vars, selected = NULL, multiple = FALSE)
})

# for alternative hypothesis
alt <- list("Two sided" = "two.sided", "Less than" = "less", "Greater than" = "greater")

ui_singleMean <- function() {
  wellPanel(
    uiOutput("sm_var"),
    selectInput(inputId = "sm_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
    sliderInput('sm_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01),
    numericInput("sm_compValue", "Comparison value:", 0)
  )
}

summary.singleMean <- function(result) {
	result
}

plot.singleMean <- function(result) {

	dat <- getdata()
	var <- input$sm_var
	x <- dat[,var]
	ifelse(is.factor(x), bw <- .1, bw <- diff(range(x)) / 12)

	p <- ggplot(dat, aes_string(x=var)) + 
			geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) + 
			geom_vline(xintercept = input$sm_compValue, color = 'red', linetype = 'longdash', size = 1) +
			geom_vline(xintercept = mean(x), color = 'black', linetype = 'solid', size = 1) +
			geom_vline(xintercept = result$conf.int, color = 'black', linetype = 'longdash', size = .5)
	print(p)
}

singleMean <- reactive(function() {
	if(is.null(input$sm_var)) return("Please select a variable")
	var <- input$sm_var
	dat <- getdata()[,var]
	t.test(dat, mu = input$sm_compValue, alternative = input$sm_alternative, conf.level = input$sm_sigLevel)
})

output$cm_var1 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "cm_var1", label = "Select a factor or numerical variable:", choices = vars, selected = NULL, multiple = FALSE)
})

output$cm_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  isFct <- sapply(getdata(), is.factor)
 	vars <- vars[!isFct]
  sel <- which(vars == input$cm_var1)
  if(length(sel) > 0) {
  	vars <- vars[-sel]
  }
  selectInput(inputId = "cm_var2", label = "Variables (select one or more):", choices = vars, selected = NULL, multiple = TRUE)
})


ui_compareMeans <- function() {
  wellPanel(
    # tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")),
    # radioButtons(inputId = "cm_paired", label = "Test type:", c("Paired" = "paired", "Independent" = "independent"), selected = ""),
    uiOutput("cm_var1"),
    uiOutput("cm_var2"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
      # selectInput(inputId = "cm_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
      sliderInput('cm_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01),
    	helpText("If you select a factor only one numerical variable from the lower box can be used")
    )
  )
}

summary.compareMeans <- function(result) {
	result <- result$model
	print(summary(result))
	cat("\n")
	print(model.tables(result,"means"),digits=3) 
	cat("\n")
	TukeyHSD(result, ordered = TRUE, conf.level = input$cm_sigLevel)
}

plot.compareMeans <- function(result) {

	dat <- result$data

	var1 <- colnames(dat)[1]
	var2 <- colnames(dat)[-1]

	plots <- list()
	plots[["Boxplot"]] <- ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) + 
										geom_boxplot(alpha=.3) + geom_jitter()

	plots[["Density"]] <- ggplot(dat, aes_string(x=var2, fill=var1)) +
														geom_density(alpha=.3)

	print(do.call(grid.arrange, c(plots, list(ncol = 1))))
}

compareMeans <- reactive(function() {
	if(is.null(input$cm_var2)) return("Please select a variable")
	var1 <- input$cm_var1
	var2 <- input$cm_var2
	dat <- getdata()[,c(var1,var2)]
	if(!is.factor(dat[,var1])) {
		dat <- melt(dat)
		var1 <- colnames(dat)[1]
		var2 <- colnames(dat)[2]
	}

	formula <- as.formula(paste(var2[1], "~", var1))
	# list("model" = aov(formula, data = dat, conf.level = input$cm_sigLevel), "data" = data.frame(dat)) 
	list("model" = aov(formula, data = dat), "data" = data.frame(dat)) 
})

output$sp_var <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  isFct <- sapply(getdata(), is.factor)
 	vars <- vars[isFct]
  selectInput(inputId = "sp_var", label = "Variable (select one):", choices = vars, selected = NULL, multiple = FALSE)
})

ui_singleProp <- function() {
  wellPanel(
    uiOutput("sp_var"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
	    selectInput(inputId = "sp_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
  	  sliderInput('sp_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01),
    	numericInput("sp_compValue", "Comparison value:", 0.5, min = 0.01, max = 0.99, step = 0.01)
    )
  )
}

summary.singleProp <- function(result) {
	result
}

plot.singleProp <- function(result) {

	var <- input$sp_var
	dat <- getdata()[,var]
	p <- qplot(factor(dat), fill = factor(dat)) + geom_bar() + theme(legend.position = "none") +
	# p <- qplot(factor(dat)) + geom_bar(fill = 'red', alpha=.1) + theme(legend.position = "none") +
		labs(list(title = paste("Single proportion -",var), x = "Factor levels", y = "Count"))
	print(p)

}

singleProp <- reactive(function() {
	if(is.null(input$sp_var)) return("Please select a variable")
	var <- input$sp_var
	dat <- getdata()[,var]
	lev <- levels(dat)
	if(length(lev) >2) return("")
	prop.test(sum(dat == rev(lev)[1]), n = length(dat), 
		p = input$sp_compValue, alternative = input$sp_alternative, conf.level = input$sp_sigLevel, correct = FALSE)

})

###############################
# Correlation
###############################

output$cor_var <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  # isFct <- sapply(getdata(), is.factor)
 	# vars <- vars[!isFct]
  selectInput(inputId = "cor_var", label = "Select variables:", choices = vars, selected = NULL, multiple = TRUE)
})

ui_correlation <- function() {
  wellPanel(
    uiOutput("cor_var"),
	  selectInput(inputId = "cor_type", label = "Method", choices = c("pearson", "kendall", "spearman"), selected = "pearson")
  )
}

summary.correlation <- function(dat) {
	cmat <- cor(dat, method = input$cor_type)
	print(as.dist(cmat), digits = 1)
}

plot.correlation <- function(dat) {
	# based mostly on http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=137
	rady.panel.plot <- function(x, y) {
	    usr <- par("usr"); on.exit(par(usr))
	    par(usr = c(0, 1, 0, 1))
	    ct <- cor.test(x,y, method = input$cor_type)
	    sig <- symnum(ct$p.value, corr = FALSE, na = FALSE,
	                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
	                  symbols = c("***", "**", "*", ".", " "))
	    r <- ct$estimate
	    rt <- format(r, digits=2)[1]
	    cex <- 0.5/strwidth(rt)
	    
	    text(.5, .5, rt, cex=cex * abs(r))
	    text(.8, .8, sig, cex=cex, col='blue')
	}
	rady.panel.smooth <- function (x, y) {
    points(x, y)
    abline(lm(y~x), col="blue")
	}
	pairs(dat, lower.panel=rady.panel.smooth, upper.panel=rady.panel.plot)
}

correlation <- reactive(function() {
	vars <- input$cor_var
	if(is.null(vars) || (length(vars) < 2)) return("Please select two or more variables")
	dat <- getdata()[,vars]
	data.frame(lapply(dat,as.numeric))
})

output$cp_var1 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  isFct <- sapply(getdata(), is.factor)
 	vars <- vars[isFct]
  selectInput(inputId = "cp_var1", label = "Select a grouping factor:", choices = vars, selected = NULL, multiple = FALSE)
})

output$cp_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  dat <- getdata()
  isFct <- sapply(dat, is.factor)
 	vars <- vars[isFct]
  sel <- which(vars == input$cp_var1)
  selectInput(inputId = "cp_var2", label = "Select a 2-level factor:", choices = vars[-sel], selected = NULL, multiple = FALSE)
})

ui_compareProps <- function() {
  wellPanel(
    uiOutput("cp_var1"),
    uiOutput("cp_var2"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
      selectInput(inputId = "cp_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
      sliderInput('cp_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01)
    )
  )
}
summary.compareProps <- function(result) {
	result
}

plot.compareProps <- function(result) {

	dat <- getdata()[,c(input$cp_var1,input$cp_var2)]
	p <- qplot(factor(dat[,1]), fill = factor(dat[,2])) + geom_bar() + 
		labs(list(title = paste("Comparing proportions of ",input$cp_var2,"$",levels(dat[,1])[1], " across levels of ",input$cp_var1, sep = ""), 
							x = paste("Factor levels for ", input$cp_var1), y = "Count", fill = input$cp_var2))

	print(p)
}

compareProps <- reactive(function() {
	if(is.null(input$cp_var2)) return("Please select a factor")
	var1 <- input$cp_var1
	var2 <- input$cp_var2
	dat <- getdata()[,c(var1,var2)]
	lev1 <- levels(dat[,1])
	lev2 <- levels(dat[,2])

	# tab <- table(group = input$cp_var1, variable = input$cp_var2)
	tab <- table(group = dat[,input$cp_var1], variable = dat[,input$cp_var2])
	pt <- prop.test(tab, correct = FALSE, alternative = input$cp_alternative, conf.level = input$cp_sigLevel)
	pt$data.name <- paste("Group = ",var1,", variable = ",var2, " (level ", levels(dat[,var2])[1],")",sep = "")
	names(pt$estimate) <-  paste(paste("P(",var2,"$",lev2[1],")|",var1, sep = ""),"$",lev1, sep = "")
	pt
})