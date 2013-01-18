summary.singleMean <- function(result) {
	result
}

plot.singleMean <- function(result) {

	dat <- getdata()
	x <- dat[,input$var1]
	ifelse(is.factor(x), bw <- .1, bw <- diff(range(x)) / 12)

	p <- ggplot(dat, aes_string(x=input$var1)) + 
			geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) + 
			geom_vline(xintercept = input$compValue, color = 'red', linetype = 'longdash', size = 1) +
			geom_vline(xintercept = mean(x), color = 'black', linetype = 'solid', size = 1) +
			geom_vline(xintercept = result$conf.int, color = 'black', linetype = 'longdash', size = .5)
	print(p)
}

extra.singleMean <- function(result) {
	cat("Under development\n")
}

singleMean <- reactive(function() {
	if(is.null(input$var1)) return("Please select a variable")
	dat <- getdata()[,input$var1]
	t.test(dat, mu = input$compValue, alternative = input$alternative, conf.level = input$sigLevel)
})

summary.compareMeans <- function(result) {
	result
}

plot.compareMeans <- function(result) {

	dat <- getdata()[,c(input$var1,input$var2)]
	dat[,input$var1] < as.factor(dat[,input$var1])

	plots <- list()
	plots[["Boxplot"]] <- ggplot(dat, aes_string(x=input$var1, y=input$var2, fill=input$var1)) + 
										geom_boxplot() + geom_jitter()

	# plots[["Histogram"]] <- ggplot(dat, aes_string(x=input$var2, fill=input$var1)) + 
														# geom_histogram(binwidth=.5, alpha=.5, position="identity")

	plots[["Density"]] <- ggplot(dat, aes_string(x=input$var2, fill=input$var1)) +
														geom_density(alpha=.3)


	print(do.call(grid.arrange, c(plots, list(ncol = 1))))
}

extra.compareMeans <- function(result) {
	# nothing here yet, could put in test variance equality
	cat("Under development\n")
}

compareMeans <- reactive(function() {
	if(is.null(input$var2)) return("Please select a variable")
	formula <- as.formula(paste(input$var2[1], "~", input$var1))
	t.test(formula, data = getdata(), alternative = input$alternative, conf.level = input$sigLevel)
})
