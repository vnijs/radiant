#' Compare a sample proportion to a population proportion
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var The variable selected for the proportion comparison
#' @param lev The factor level selected for the proportion comparison
#' @param comp_value Population value to compare to the sample proportion
#' @param alternative The alternative hypothesis ("two.sided", "greater", or "less")
#' @param conf_lev Span of the confidence interval
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of variables used in single_prop as an object of class single_prop
#'
#' @examples
#' result <- single_prop("diamonds","cut")
#' result <- single_prop("diamonds","clarity", lev = "IF", comp_value = 0.05)
#'
#' @seealso \code{\link{summary.single_prop}} to summarize the results
#' @seealso \code{\link{plot.single_prop}} to plot the results
#'
#' @export
single_prop <- function(dataset, var,
                        lev = "",
                        comp_value = 0.5,
                        alternative = "two.sided",
                        conf_lev = .95,
                        data_filter = "") {

	dat <- getdata(dataset, var, filt = data_filter) %>% mutate_each(funs(as.factor))
	if (!is_string(dataset)) dataset <- "-----"

	levs <- levels(dat[[var]])
	if (lev != "") {
		if (lev %in% levs && levs[1] != lev) {
			dat[[var]] %<>% as.character %>% as.factor %>% relevel(lev)
			levs <- levels(dat[[var]])
		}
	} else {
		lev <- levs[1]
	}

	n <- nrow(dat)
	ns <- sum(dat == lev)

	# res <- prop.test(ns, n, p = comp_value, alternative = alternative,
	#                  conf.level = conf_lev, correct = FALSE) %>% tidy

	## use binom.test for exact
	res <- binom.test(ns, n, p = comp_value, alternative = alternative,
	                  conf.level = conf_lev) %>% tidy

  environment() %>% as.list %>% set_class(c("single_prop",class(.)))
}

#' Summary method for the single_prop function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{single_prop}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_prop("diamonds","clarity", lev = "IF", comp_value = 0.05)
#' summary(result)
#' diamonds %>% single_prop("clarity", lev = "IF", comp_value = 0.05) %>% summary
#'
#' @seealso \code{\link{single_prop}} to generate the results
#' @seealso \code{\link{plot.single_prop}} to plot the results
#'
#' @export
summary.single_prop <- function(object, ...) {

  cat("Single proportion test (binomial exact)\n")
	cat("Data      :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter    :", gsub("\\n","", object$data_filter), "\n")
	cat("Variable  :", object$var, "\n")
	cat("Confidence:", object$conf_lev, "\n")

	hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[object$alternative]

	cat("Null hyp. : the proportion of", object$lev, "in", object$var, "=",
	    object$comp_value, "\n")
	cat("Alt. hyp. : the proportion of", object$lev, "in", object$var, hyp_symbol,
	    object$comp_value, "\n\n")

	## determine lower and upper % for ci
	{100 * (1 - object$conf_lev)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%") -> ci_perc

	# res <- round(object$res, 3) 	## restrict to 3 decimal places
	# res$ns <- object$ns
	# res$n <- object$n
	# names(res) <- c("prop","chisq.value","p.value","df", ci_perc[1], ci_perc[2],
	# 								"ns","n")

	res <- round(object$res, 3) 	## restrict to 3 decimal places
	res$statistic <- res$parameter <- NULL
	res$ns <- object$ns
	res$n <- object$n
	# names(res) <- c("prop","p.value","df", ci_perc[1], ci_perc[2], "ns","n")
	names(res) <- c("prop","p.value", ci_perc[1], ci_perc[2], "ns","n")

	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

#' Plot method for the single_prop function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{single_prop}}
#' @param plots Plots to generate. "hist" shows a histogram of the data along with vertical lines that indicate the sample proportion and the confidence interval. "simulate" shows the location of the sample proportion and the comparison value (comp_value). Simulation is used to demonstrate the sampling variability in the data under the null-hypothesis
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- single_prop("diamonds","clarity", lev = "IF", comp_value = 0.05)
#' plot(result, plots = c("hist", "simulate"))
#' result <- single_prop("titanic","pclass", lev = "1st")
#' plot(result, plots = c("hist","simulate"))
#'
#' @seealso \code{\link{single_prop}} to generate the result
#' @seealso \code{\link{summary.single_prop}} to summarize the results
#'
#' @export
plot.single_prop <- function(x,
                             plots = "hist",
                             shiny = FALSE,
                             ...) {

  object <- x; rm(x)

	lev_name <- object$levs[1]

 	plot_list <- list()
	if ("hist" %in% plots) {
		plot_list[[which("hist" == plots)]] <-
			ggplot(object$dat, aes_string(x = object$var, fill = object$var)) +
	 			geom_histogram(alpha = .7) +
	 	 		ggtitle(paste0("Single proportion: ", lev_name, " in ", object$var)) +
	 	 		theme(legend.position = "none")
	}
	if ("simulate" %in% plots) {
		simdat <- rbinom(1000, prob = object$comp_value, object$n) %>%
								divide_by(object$n) %>%
							  data.frame %>%
							  set_colnames(lev_name)

		ci_perc <- {if (object$alternative == 'two.sided') {
									{(1 - object$conf_lev)/2}  %>% c(., 1 - .)
								 } else if (object$alternative == 'less') {
									{1 - object$conf_lev}
								 } else {
									object$conf_lev
								 }
							 } %>% quantile(simdat[[lev_name]], probs = . )

		bw <- simdat %>% range %>% diff %>% divide_by(20)

		# to avoid problems with levels that start with numbers or contain spaces
		# http://stackoverflow.com/questions/13445435/ggplot2-aes-string-fails-to-handle-names-starting-with-numbers-or-containing-s
		names(simdat) <- "col1"

		plot_list[[which("simulate" == plots)]] <-
			ggplot(simdat, aes(x = col1)) +
				geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
				geom_vline(xintercept = object$comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = object$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = ci_perc,
				           color = 'red', linetype = 'longdash', size = .5) +
	 	 		ggtitle(paste0("Simulated proportions if null hyp. is true (", lev_name, " in ", object$var, ")")) +
	 	 		labs(x = paste0("Level ",lev_name, " in variable ", object$var))
	}

	sshhr( do.call(arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
	  { if (shiny) . else print(.) }
}
