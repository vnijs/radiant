summary.singleMean <- function(state) {
	# varname <- state$var1
	# assign(varname, getdata()[,state$var1])
	dat <- getdata()[,state$var1]

	comp <- 0
	# alt <- "two.sided"
	# conf <- .95

	# t.test(dat, mu = comp, alternative = state$alternativealt, conf.level = conf )
	t.test(dat, mu = state$compValue, alternative = state$alternative, conf.level = state$sigLevel)

}

plot.singleMean <- function(state) {

	dat <- getdata()

	x <- dat[,input$var1]
	ifelse(is.factor(x), bw <- .1, bw <- diff(range(x)) / 12)

	p <- ggplot(dat, aes_string(x=input$var1)) + 
			geom_histogram(colour = 'black', fill = 'blue', binwidth = bw) + 
			geom_vline(xintercept = state$compValue, color = 'red', linetype = 'longdash', size = 1) +
			geom_vline(xintercept = mean(x), color = 'green', linetype = 'solid', size = 1)
	print(p)
}

extra.singleMean <- function(state) {
	cat("Under development\n")
}

singleMean <- reactive(function() {
	if(is.null(input$var1)) return("Please select a variable")
	as.list(input)
})

summary.compareMeans <- function(state) {
	formula <- as.formula(paste(state$var2[1], "~", state$var1))
	t.test(formula, data = getdata())
}

plot.compareMeans <- function(state) {
	if(is.null(state$datasets) || is.null(state$var2)) return()

	dat <- getdata()
	# plotting through ggplot not working yet
	# y <- dat[,state$var2]
	# x <- as.factor(dat[,state$var1])
	# dat <- data.frame(cbind(x,y))
	print(qplot(factor(dat[,state$var1]), dat[,state$var2[1]], data = dat, xlab = state$var1, ylab = state$var2, geom = c("boxplot", "jitter")))
	# print(ggplot(dat, aes_string(x=x, y=state$var2)) + geom_boxplot()) # x must be specified as a factor --> doesn't work with aes_string
	# print(ggplot(dat, aes(x=x, y=y)) + geom_boxplot()) # x must be specified as a factor --> doesn't work with aes_string
}

extra.compareMeans <- function(state) {
	# nothing here yet, could put in test variance equality
	cat("Nothing yet\n")
}

compareMeans <- reactive(function() {
	if(is.null(input$var2)) return("Please select a variable")
	as.list(input)
})
