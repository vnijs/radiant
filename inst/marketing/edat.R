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

extra.singleMean <- function(result) {
	cat("Under development\n")
}

singleMean <- reactive(function() {
	if(is.null(input$sm_var)) return("Please select a variable")
	var <- input$sm_var
	dat <- getdata()[,var]
	t.test(dat, mu = input$sm_compValue, alternative = input$sm_alternative, conf.level = input$sm_sigLevel)
})

summary.compareMeans <- function(result) {
	result
}

plot.compareMeans <- function(result) {

	var1 <- input$cm_var1
	var2 <- input$cm_var2

	dat <- getdata()[,c(var1,var2)]
	dat[,var1] < as.factor(dat[,var1])

	plots <- list()
	plots[["Boxplot"]] <- ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) + 
										geom_boxplot() + geom_jitter()

	# plots[["Histogram"]] <- ggplot(dat, aes_string(x=var2, fill=var1)) + 
														# geom_histogram(binwidth=.5, alpha=.5, position="identity")

	plots[["Density"]] <- ggplot(dat, aes_string(x=var2, fill=var1)) +
														geom_density(alpha=.3)

	print(do.call(grid.arrange, c(plots, list(ncol = 1))))
}

extra.compareMeans <- function(result) {
	# nothing here yet, could put in test variance equality
	cat("Under development\n")
}

compareMeans <- reactive(function() {
	if(is.null(input$cm_var2)) return("Please select a variable")
	var1 <- input$cm_var1
	var2 <- input$cm_var2

	formula <- as.formula(paste(var2[1], "~", var1))
	t.test(formula, data = getdata(), alternative = input$cm_alternative, conf.level = input$cm_sigLevel)
})
