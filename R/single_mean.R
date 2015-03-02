#' Compare a sample mean to a population mean
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param sm_var The variable selected for the mean comparison
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param sm_comp_value Population value to compare to the sample mean
#' @param sm_alternative The alternative hypothesis ("two.sided", "greater", or "less")
#' @param sm_sig_level Span for the confidence interval
#'
#' @return A list of variables defined in single_mean as an object of class single_mean
#'
#' @examples
#' single_mean("diamonds","price")
#'
#' @seealso \code{\link{summary.single_mean}} to summarize results
#' @seealso \code{\link{plot.single_mean}} to plot results
#'
#' @export
single_mean <- function(dataset, sm_var,
                        data_filter = "",
                        sm_comp_value = 0,
                        sm_alternative = "two.sided",
                        sm_sig_level = .95) {

	dat <- getdata(dataset, sm_var, filt = data_filter)

	t.test(dat, mu = sm_comp_value, alternative = sm_alternative,
	       conf.level = sm_sig_level) %>% tidy -> res

	# time_main <- now()

  environment() %>% as.list %>% set_class(c("single_mean",class(.)))
}

#' Summary method for single_mean
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
#' @seealso \code{\link{plot.single_mean}} to plot results
#'
#' @export
summary.single_mean <- function(result) {

  # cat("Time - main",result$time_main,"\n")
  # cat("Time - summary",now(),"\n")
  cat("Single mean test\n")
	cat("Data     :", result$dataset, "\n")
	if(result$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
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
	res <- round(result$res, 3) 	# restrict to 3 decimals
	names(res)[1:6] <- c("mean","t.value","p.value","df", ci_perc[1], ci_perc[2])
	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

#' Plot method for single_mean
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{single_mean}}
#' @param sm_plots Plots to generate. "hist" shows a histogram of the data along with vertical lines that indicate the sample mean and the confidence interval. "simulate" shows the location of the sample mean and the comparison value (sm_comp_value). Simulation is used to demonstrate the sampling variability in the data under the null-hypothesis
#'
#' @examples
#' result <- single_mean("diamonds","price", sm_comp_value = 3500)
#' plot(result, sm_plots = c("hist", "simulate"))
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{summary.single_mean}} to summarize results
#'
#' @export
plot.single_mean <- function(result, sm_plots = "hist") {

 	plots <- list()

	if("hist" %in% sm_plots) {
		bw <- result$dat %>% range(na.rm = TRUE) %>% diff %>% divide_by(10)

		plots[[which("hist" == sm_plots)]] <-
			ggplot(result$dat, aes_string(x=result$sm_var)) +
				geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
				geom_vline(xintercept = result$sm_comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = result$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = c(result$res$conf.low, result$res$conf.high),
				           color = 'black', linetype = 'longdash', size = .5)
	}
	if("simulate" %in% sm_plots) {

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
									1-result$sm_sig_level
								} else {
									result$sm_sig_level
								}} %>%
									quantile(simdat[,result$sm_var], probs = .)

		bw <- simdat %>% range %>% diff %>% divide_by(20)

		plots[[which("simulate" == sm_plots)]] <-
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
