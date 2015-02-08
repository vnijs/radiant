#' Compare a sample mean to a population mean
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param sm_var The variable selected for the mean comparison
#' @param sm_comp_value Population value to compare the sample mean with
#' @param sm_alternative The alternative hypothesis (two.sided, greater or less)
#' @param sm_sig_level Span of the confidence interval
#'
#' @return A list with all variables defined in the function as an object of class single_mean
#'
#' @examples
#' single_mean("diamonds","price")
#'
#' @seealso \code{\link{summary.single_mean}} to summarize results
#' @seealso \code{\link{plots.single_mean}} to plot results
#'
#' @export
single_mean <- function(dataset, sm_var,
                        data_filter = "",
                        sm_comp_value = 0,
                        sm_alternative = "two.sided",
                        sm_sig_level = .95,
                        sm_plots = "hist") {
                        # sm_plots = c("hist","simulate")) {

	dat <- getdata_exp(dataset, sm_var, filt = data_filter)

	t.test(dat, mu = sm_comp_value, alternative = sm_alternative,
	       conf.level = sm_sig_level) %>% tidy -> res

	plot_height <- 400 * length(sm_plots)

  environment() %>% as.list %>% set_class(c("single_mean",class(.)))
}

#' Summarize results from the single_mean function. This is a method of class single_mean and can be called as summary or summary.single_mean
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{single_mean}}
#'
#' @examples
#' result <- single_mean("diamonds","price")
#' summary(result)
#'
#' @seealso \code{\link{single_mean}} to generate the results
#' @seealso \code{\link{plots.single_mean}} to plot results
#'
#' @export
summary.single_mean <- function(result) {

  cat("Single mean test\n")
	cat("Data     :", result$dataset, "\n")
	if(result$data_filter != "")
		cat("Filter   :", gsub("\\n","",result$data_filter), "\n")
	cat("Variable :", result$sm_var, "\n")

	hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[result$sm_alternative]

	cat("Null hyp.: the mean of", result$sm_var, "=", result$sm_comp_value, "\n")
	cat("Alt. hyp.: the mean of", result$sm_var, "is", hyp_symbol,
	    result$sm_comp_value, "\n\n")

	# determine lower and upper % for ci
	{100 * (1-result$sm_sig_level)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%") -> ci_perc

	result$res$sd <- sd(result$dat[,result$sm_var])
	result$res$n <- nrow(result$dat)
	res <- round(result$res, 3) 	# restrict to 3 decimal places
	names(res)[1:6] <- c("mean","t.value","p.value","df", ci_perc[1], ci_perc[2])
	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

#' Plot results from the single_mean function. This is a method of class single_mean and can be called as plot or plot.single_mean
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{single_mean}}
#'
#' @examples
#' result <- single_mean("diamonds","price")
#' plot(result)
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{summary.single_mean}} to summarize results
#'
#' @export
plot.single_mean <- function(result) {

 	plots <- list()

	if("hist" %in% result$sm_plots) {
		bw <- result$dat %>% range(na.rm = TRUE) %>% diff %>% divide_by(10)

		plots[[which("hist" == result$sm_plots)]] <-
			ggplot(result$dat, aes_string(x=result$sm_var)) +
				geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
				geom_vline(xintercept = result$sm_comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = result$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = c(result$res$conf.low, result$res$conf.high),
				           color = 'black', linetype = 'longdash', size = .5)
	}
	if("simulate" %in% result$sm_plots) {

		simdat <- matrix(0, nrow = 1000)
		for(i in 1:nrow(simdat)) {
			simdat[i] <- result$dat[,result$sm_var] %>%
										 sample(., length(.), replace = TRUE) %>%
										 mean
		}

		simdat %<>% { (. - mean(.)) + result$sm_comp_value } %>%
									as.data.frame %>% set_colnames(result$sm_var)

		ci_perc <- {if(result$sm_alternative == 'two.sided') {
									{(1-result$sm_sig_level)/2}  %>% c(., 1 - .)
								} else if(result$sm_alternative == 'less') {
									{1-result$sm_sig_level}
								} else {
									result$sm_sig_level
								}} %>%
									quantile(simdat[,result$sm_var], probs = . )

		bw <- simdat %>% range %>% diff %>% divide_by(20)

		plots[[which("simulate" == result$sm_plots)]] <-
			ggplot(simdat, aes_string(x=result$sm_var)) +
				geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
				geom_vline(xintercept = result$sm_comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = result$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = ci_perc,
				           color = 'red', linetype = 'longdash', size = .5) +
	 	 		ggtitle(paste0("Simulated means if null hyp. is true (", result$sm_var, ")"))
	}

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}

# test
# library(gridExtra)
# library(ggplot2)
# library(dplyr)
# diamonds <- diamonds[1:100,]
# result <- single_mean("diamonds","price", sm_plots = c("hist","simulate"))
# summary(result)
# result <- single_mean("diamonds","price", sm_plots = c("hist","simulate"),
#                       sm_alternative = "less")
# plot(result)
# result <- single_mean("diamonds","price", sm_plots = c("hist","simulate"),
#                       sm_alternative = "greater")
# plot(result)
# end test

# bootstrap using dplyr and broom NOT WORKING
# library(broom)
# select(mtcars, mpg) %>% bootstrap(10)
# mtcars %>% bootstrap(10) %>% summarize(mean(mpg))
# boot <- bootstrap(mtcars,10)

# mtcars %>%
# 	group_by(vs) %>%
# 	summarize(mean = mean(mpg))

# mtcars %>%
# 	bootstrap(10) %>%
# 	summarize(mean = mean(mpg))

# bootstrap(mtcars, 5) %>% summarize(min(mpg))

# select(mtcars,mpg) %>% sample(., length(.), replace = TRUE) %>% summarize(mean = mean(mpg))

# don't use the suggestion below. seems much slower than a simple loop
# library(broom)
# library(ggplot2)
# library(dplyr)

# nruns <- 1000

# system.time(
# 	simdat1 <- select(diamonds,price) %>% bootstrap(nruns) %>% do(data.frame(mean = mean(.$price)))
# )

# system.time({
# 	simdat2 <- matrix(0, nrow = nruns)
# 	for(i in 1:nrow(simdat)) {
# 		simdat2[i] <- diamonds$price %>%
# 									 sample(., length(.), replace = TRUE) %>%
# 									 mean
# 	}
# })

# simdat1
# simdat2[1:10]
