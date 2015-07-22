#' Compare a sample mean to a population mean
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var The variable selected for the mean comparison
#' @param comp_value Population value to compare to the sample mean
#' @param alternative The alternative hypothesis ("two.sided", "greater", or "less")
#' @param conf_lev Span for the confidence interval
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
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
single_mean <- function(dataset, var,
                        comp_value = 0,
                        alternative = "two.sided",
                        conf_lev = .95,
                        data_filter = "") {

	dat <- getdata(dataset, var, filt = data_filter)
	if (!is_string(dataset)) dataset <- "-----"

	t.test(dat[[var]], mu = comp_value, alternative = alternative,
	       conf.level = conf_lev) %>% tidy -> res

  environment() %>% as.list %>% set_class(c("single_mean",class(.)))
}

#' Summary method for the single_mean function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{single_mean}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_mean("diamonds","price")
#' summary(result)
#' diamonds %>% single_mean("price") %>% summary
#'
#' @seealso \code{\link{single_mean}} to generate the results
#' @seealso \code{\link{plot.single_mean}} to plot results
#'
#' @export
summary.single_mean <- function(object, ...) {

  cat("Single mean test\n")
	cat("Data      :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter    :", gsub("\\n","", object$data_filter), "\n")
	cat("Variable  :", object$var, "\n")
	cat("Confidence:", object$conf_lev, "\n")

	hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[object$alternative]

	cat("Null hyp. : the mean of", object$var, "=", object$comp_value, "\n")
	cat("Alt. hyp. : the mean of", object$var, "is", hyp_symbol,
	    object$comp_value, "\n\n")

	# determine lower and upper % for ci
	{100 * (1-object$conf_lev)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%") -> ci_perc

	object$res$sd <- sd(object$dat[[object$var]])
	object$res$n <- nrow(object$dat)
	res <- round(object$res, 3) 	# restrict to 3 decimals
	names(res)[1:6] <- c("mean","t.value","p.value","df", ci_perc[1], ci_perc[2])
	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

#' Plot method for the single_mean function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_mean.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{single_mean}}
#' @param plots Plots to generate. "hist" shows a histogram of the data along with vertical lines that indicate the sample mean and the confidence interval. "simulate" shows the location of the sample mean and the comparison value (comp_value). Simulation is used to demonstrate the sampling variability in the data under the null-hypothesis
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_mean("diamonds","price", comp_value = 3500)
#' plot(result, plots = c("hist", "simulate"))
#'
#' @seealso \code{\link{single_mean}} to generate the result
#' @seealso \code{\link{summary.single_mean}} to summarize results
#'
#' @export
plot.single_mean <- function(x,
                             plots = "hist",
                             shiny = FALSE,
                             ...) {

  object <- x; rm(x)

 	plot_list <- list()

	if ("hist" %in% plots) {
		bw <- object$dat %>% range(na.rm = TRUE) %>% diff %>% divide_by(10)

		plot_list[[which("hist" == plots)]] <-
			ggplot(object$dat, aes_string(x=object$var)) +
				geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
				geom_vline(xintercept = object$comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = object$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = c(object$res$conf.low, object$res$conf.high),
				           color = 'black', linetype = 'longdash', size = .5)
	}
	if ("simulate" %in% plots) {

		simdat <- matrix(0, nrow = 1000)
		for (i in 1:nrow(simdat)) {
			simdat[i] <- object$dat[[object$var]] %>%
										 sample(., length(.), replace = TRUE) %>%
										 mean
		}

		simdat %<>% { (. - mean(.)) + object$comp_value } %>%
									as.data.frame %>% set_colnames(object$var)

		ci_perc <- {if (object$alternative == 'two.sided') {
									{(1-object$conf_lev)/2}  %>% c(., 1 - .)
								} else if (object$alternative == 'less') {
									1-object$conf_lev
								} else {
									object$conf_lev
								}} %>%
									quantile(simdat[[object$var]], probs = .)

		bw <- simdat %>% range %>% diff %>% divide_by(20)

		plot_list[[which("simulate" == plots)]] <-
			ggplot(simdat, aes_string(x=object$var)) +
				geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
				geom_vline(xintercept = object$comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = object$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = ci_perc,
				           color = 'red', linetype = 'longdash', size = .5) +
	 	 		ggtitle(paste0("Simulated means if null hyp. is true (", object$var, ")"))
	}

	sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
	  { if (shiny) . else print(.) }
}
