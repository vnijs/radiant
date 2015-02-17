#' Compare a sample proportion to a population proportion
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param sp_var The variable selected for the proportion comparison
#' @param data_filter Expression intered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param sp_levels The factor level selected for the proportion comparison
#' @param sp_comp_value Population value to compare the sample proportion to
#' @param sp_alternative The alternative hypothesis (two.sided, greater or less)
#' @param sp_sig_level Span of the confidence interval
#'
#' @return A list with all variables defined in the function as an object of class single_prop
#'
#' @examples
#' single_prop("diamond","cut","IF")
#'
#' @seealso \code{\link{summary.single_prop}} to summarize results
#' @seealso \code{\link{plots.single_prop}} to plot results
#'
#' @export
single_prop <- function(dataset, sp_var,
                        data_filter = "",
                        sp_levels = "",
                        sp_comp_value = 0.5,
                        sp_alternative = "two.sided",
                        sp_sig_level = .95,
                        sp_plots = "hist") {

	dat <- getdata_exp(dataset, sp_var, filt = data_filter) %>% mutate_each(funs(as.factor))

	levs <- levels(dat[,sp_var])
	if(sp_levels != "") {
		if(sp_levels %in% levs && levs[1] != sp_levels) {
			dat[,sp_var] %<>% as.character %>% as.factor %>% relevel(sp_levels)
			levs <- levels(dat[,sp_var])
		}
	}

	n <- nrow(dat)
	ns <- sum(dat == sp_levels)
	# use binom.test for exact
	prop.test(ns, n, p = sp_comp_value, alternative = sp_alternative,
	           conf.level = sp_sig_level, correct = FALSE) %>% tidy -> res

	plot_height <- 400 * length(sp_plots)

  environment() %>% as.list %>% set_class(c("single_prop",class(.)))
}

#' Summarize results from the single_prop function. This is a method of class single_prop and can be called as summary or summary.single_prop
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{single_prop}}
#'
#' @examples
#' result <- single_prop("diamond","cut")
#' summary(result)
#'
#' @seealso \code{\link{single_prop}} to generate the results
#' @seealso \code{\link{plots.single_prop}} to plot results
#'
#' @export
summary.single_prop <- function(result) {

  cat("Single proportion test\n")
	cat("Data     :", result$dataset, "\n")
	if(result$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
	cat("Variable :", result$sp_var, "\n")

	hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[result$sp_alternative]

	cat("Null hyp.: the proportion of", result$sp_levels, "in", result$sp_var, "=",
	    result$sp_comp_value, "\n")
	cat("Alt. hyp.: the proportion of", result$sp_levels, "in", result$sp_var, hyp_symbol,
	    result$sp_comp_value, "\n\n")

	# determine lower and upper % for ci
	{100 * (1-result$sp_sig_level)/2} %>%
		c(., 100 - .) %>%
		round(1) %>%
		paste0(.,"%") -> ci_perc

	# result$res$n <- nrow(result$dat)
	res <- round(result$res, 3) 	# restrict to 3 decimal places
	res$ns <- result$ns
	res$n <- result$n
	names(res) <- c("prop","chisq.value","p.value","df", ci_perc[1], ci_perc[2],
									"ns","n")
	if (res$p.value < .001) res$p.value <- "< .001"

	print(res, row.names = FALSE)
}

#' Plot results from the single_prop function. This is a method of class single_prop and can be called as plot or plot.single_prop
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/single_prop.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{single_prop}}
#'
#' @examples
#' result <- single_prop("diamond","cut")
#' plot(result)
#'
#' @seealso \code{\link{single_prop}} to generate the result
#' @seealso \code{\link{summary.single_prop}} to summarize results
#'
#' @export
plot.single_prop <- function(result) {

	lev_name <- result$levs[1]

 	plots <- list()
	if("hist" %in% result$sp_plots) {
		plots[[which("hist" == result$sp_plots)]] <-
			ggplot(result$dat, aes_string(x = result$sp_var, fill = result$sp_var)) +
	 			geom_histogram(alpha=.7) +
	 	 		ggtitle(paste0("Single proportion: ", lev_name, " in ", result$sp_var))
	}
	if("simulate" %in% result$sp_plots) {
		simdat <- rbinom(1000, prob = result$sp_comp_value, result$n) %>%
								divide_by(result$n) %>%
							  data.frame %>%
							  set_colnames(lev_name)

		ci_perc <- {if(result$sp_alternative == 'two.sided') {
									{(1-result$sp_sig_level)/2}  %>% c(., 1 - .)
								} else if(result$sp_alternative == 'less') {
									{1-result$sp_sig_level}
								} else {
									result$sp_sig_level
								}} %>%
									quantile(simdat[,lev_name], probs = . )

		bw <- simdat %>% range %>% diff %>% divide_by(20)

		plots[[which("simulate" == result$sp_plots)]] <-
			ggplot(simdat, aes_string(x=lev_name)) +
				geom_histogram(colour = 'black', fill = 'blue', binwidth = bw, alpha = .1) +
				geom_vline(xintercept = result$sp_comp_value, color = 'red',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = result$res$estimate, color = 'black',
				           linetype = 'solid', size = 1) +
				geom_vline(xintercept = ci_perc,
				           color = 'red', linetype = 'longdash', size = .5) +
	 	 		ggtitle(paste0("Simulated proportions if null hyp. is true (", lev_name, " in ", result$sp_var, ")"))
	}

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}

# rm(list = ls())
# library(ggplot2)
# library(dplyr)
# library(broom)
# dataset <- "diamonds"
# single_prop("diamonds","clarity","VS")

# diamonds[1:10,"clarity"] %>% as.character %>% as.factor
# lapply(diamonds,class)
# x <- diamonds[,"clarity"] %>% as.character %>% as.factor
# class(x)
# diamonds$x

# %>% relevel(sp_levels)

# dat <- diamonds
# sp_var <- "clarity"
# sp_levels <- "VVS2"

# dat[,sp_var] %>% as.factor %>% relevel(sp_levels)
# dat[,sp_var] %>% as.character %>% as.factor %>% relevel(sp_levels)

# sp_levels <- "Fair"
# sp_comp_value = 0.05
# sp_alternative = "less"
# sp_sig_level = .95
# sp_plots = "hist"
