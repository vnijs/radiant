#' Compare a sample mean to a population mean
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in a values list from Radiant
#' @param sm_var The variable selected for the mean comparison
#' @param dataset_file Feature not yet implemented
#' @param sm_comp_value Population value to compare the sample mean with
#' @param sm_alternative The alternative hypothesis (two.sided, greater or less)
#' @param sm_sig_level Span of the confidence interval
#'
#' @return A list with all variables defined in the function
#'
#' @examples
#' single_mean("diamonds","price")
#'
#' @seealso \code{\link{summary_single_mean}} to print results
#' @seealso \code{\link{plots_single_mean}} to plot results
#'
#' @export
single_mean <- function(dataset, sm_var,
                        dataset_filter = "",
                        sm_comp_value = 0,
                        sm_alternative = "two.sided",
                        sm_sig_level = .95) {

	if(exists("values"))
		dat <- select_(values[[dataset]], sm_var)
		# if a data_filter has been defined
		# dat <- filter(dat, values[[dataset_filter]])
	else
		dat <- select_(get(dataset), sm_var)

	t.test(dat, mu = sm_comp_value, alternative = sm_alternative,
	       conf.level = sm_sig_level) %>% tidy -> res


  # as.list(environment())
  environment() %>% as.list
}

#' Summarize results from the single_mean function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{single_mean}}
#'
#' @examples
#' result <- single_mean("diamonds","price")
#' summary_single_mean(result)
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{plots_single_mean}} to plot results
#'
#' @export
summary_single_mean <- function(result) {

	cat("Data     :", result$dataset, "\n")
	# cat("Filter   :", result$xtra$filter, "\n")
	cat("Variable :", result$sm_var, "\n")

	if (result$sm_alternative == "two.sided") {
		hyp_symbol <- "not equal to"
	} else if (result$sm_alternative == "less") {
		hyp_symbol <- "<"
	} else {
		hyp_symbol <- ">"
	}

	cat("Null hyp.: the mean of", result$sm_var, "=", result$sm_comp_value, "\n")
	cat("Alt. hyp.: the mean of", result$sm_var, "is", hyp_symbol,
	    result$sm_comp_value, "\n\n")

	# determine lower and upper % for ci
	{100 * (1-result$sm_sig_level)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%") -> ci_perc

	result$res$n <- nrow(result$dat)
	result$res$sd <- sd(result$dat[,result$sm_var])
	res <- round(result$res, 3) 	# restrict to 3 decimal places
	names(res)[1:6] <- c("mean","t.value","p.value","df", ci_perc[1], ci_perc[2])
	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

#' Plot results from the single_mean function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{single_mean}}
#'
#' @examples
#' result <- single_mean("diamonds","price")
#' plots_single_mean(result)
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{summary_single_mean}} to print results
#'
#' @export
plots_single_mean <- function(result) {

	bw <- diff(range(result$dat, na.rm = TRUE)) / 12
	res <- result$res
	p <- ggplot(result$dat, aes_string(x=result$sm_var)) +
		geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
		geom_vline(xintercept = result$sm_comp_value, color = 'red',
		           linetype = 'longdash', size = 1) +
		geom_vline(xintercept = result$res$estimate, color = 'black',
		           linetype = 'solid', size = 1) +
		geom_vline(xintercept = c(result$res$conf.low, result$res$conf.high),
		           color = 'black', linetype = 'longdash', size = .5)
	print(p)
}
