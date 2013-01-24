# UI-elements for EDAT.R

# variable selection - singleMean
output$sm_var <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "sm_var", label = "Variable (select one):", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection - compareMeans
output$cm_var1 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "cm_var1", label = "Select group or numeria variable:", choices = vars, selected = NULL, multiple = FALSE)
})

# variable selection - compareMeans
output$cm_var2 <- reactiveUI(function() {
  vars <- varnames()
  if(is.null(vars)) return()
  selectInput(inputId = "cm_var2", label = "Variables (select one):", choices = vars[-which(vars == input$cm_var1)], selected = NULL, multiple = TRUE)
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

ui_compareMeans <- function() {
  wellPanel(
    # tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }")),
    # radioButtons(inputId = "cm_paired", label = "Test type:", c("Paired" = "paired", "Independent" = "independent"), selected = ""),
    uiOutput("cm_var1"),
    uiOutput("cm_var2"),
    conditionalPanel(condition = "input.analysistabs == 'Summary'",
      # selectInput(inputId = "cm_alternative", label = "Alternative hypothesis", choices = alt, selected = "Two sided"),
      sliderInput('cm_sigLevel',"Significance level:", min = 0.85, max = 0.99, value = 0.95, step = 0.01)
    )
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
	# if(class(result)[1] == "aov") {
		print(summary(result))
		cat("\n")
		print(model.tables(result,"means"),digits=3) 
		cat("\n")
		TukeyHSD(result, ordered = TRUE, conf.level = input$cm_sigLevel)
	# } else {
	# 	result
	# }
}

plot.compareMeans <- function(result) {

	var1 <- input$cm_var1
	var2 <- input$cm_var2

	dat <- getdata()[,c(var1,var2)]

	# dat[,var1] < as.factor(dat[,var1])

	plots <- list()
	plots[["Boxplot"]] <- ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) + 
										geom_boxplot() + geom_jitter()

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
	dat <- getdata()[,c(var1,var2)]
	if(!is.factor(dat[,var1])) {
		dat <- data.frame(cbind(as.factor(c(rep(var1,nrow(dat)),rep(var2,nrow(dat)))),c(dat)))
		# colnames(dat) <- c(var1,var2)
		var1 <- "Numeric"
		var2 <- "Factor"
		colnames(dat) <- c(var1,var2)
	}

	formula <- as.formula(paste(var2[1], "~", var1))
	aov(formula, data = dat, conf.level = input$cm_sigLevel)
})



