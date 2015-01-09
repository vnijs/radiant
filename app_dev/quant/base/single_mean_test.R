

test <- FALSE
if(test) {
	source("base.R", local = TRUE)
	values <- list()
	values$mtcars <- mtcars
	result <- single_mean("mtcars", "mpg")
	summary_single_mean(result)
	result <- single_mean("mtcars", "mpg", sm_comp_value = 10)
	summary_single_mean(result)
	result <- single_mean("mtcars", "mpg", sm_comp_value = 10,
	                      sm_alternative = 'less', sm_sig_level = .90)
	summary_single_mean(result)
	plots_single_mean(result)
}
