# Draw sample

# Check function {{{
.sampleCheckFunction <- function(state){
	if(state$n == "")
		return("Please specify the desired sample size")
	return("")
} #}}}

# run function {{{
.sampleRunFunction <- function(state) {

	cmd <- paste("s.id <- sample(1:dim(",state$data,")[1], size = ", as.numeric(state$n), ", replace = FALSE)\n", sep = "")
	cmd <- paste(cmd, paste("selected.sample <- ",state$data,"[s.id,]\n", sep = ""))
	cmd <- paste(cmd, "print(selected.sample)")

	execute(cmd)
} #}}}

# Dialog {{{
makeRadySampleDialog <-function() {

	# make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L, 400L)
	dialog$setTitle("Single mean")

	# add variable selector
	variableSelector <-new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	variableList <- new(VariableListWidget, variableSelector)
	addComponent(dialog, variableSelector,1,500,850,10)	

	n <- new(TextAreaWidget,"Sample size")
	n$setTitle("n")
	n$setDefaultModel("1")
	addComponent(dialog, n, 100 ,1000, 250, 550)

	dialog$setCheckFunction(toJava(.sampleCheckFunction))
	dialog$setRunFunction(toJava(.sampleRunFunction))

	### dialog$run()
	return(dialog)
} #}}}
