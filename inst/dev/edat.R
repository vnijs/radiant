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
