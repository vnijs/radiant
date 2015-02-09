#' Compare means for two or more variables
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param cm_var1 A numeric variable or factor selected for comparison
#' @param cm_var2 One or more numeric variable for comparison. If cm_var1 is a factor only one variable can be selected and the mean of this variable is compared across the factor levels of cm_var1
#' @param data_filter Expression intered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param cm_alternative The alternative hypothesis (two.sided, greater or less)
#' @param cm_sig_level Span of the confidence interval
#' @param cm_adjust Adjustment for multiple comparisons (none or bonferroni)
#' @param cm_plots One or more plots of mean values (bar, box, or density plot)
#'
#' @return A list with all variables defined in the function as an object of class compare_means
#'
#' @examples
#' compare_means("diamonds","price",c("carats","table"))
#'
#' @seealso \code{\link{summary.compare_means}} to summarize results
#' @seealso \code{\link{plots.compare_means}} to plot results
#'
#' @export
compare_means <- function(dataset, cm_var1, cm_var2,
                          data_filter = "",
                          cm_paired = "independent",
                          cm_alternative = "two.sided",
                          cm_sig_level = .95,
                          cm_adjust = "none",
                          cm_plots = "bar") {

# dataset <- "diamonds"
# cm_var1 <- "price"
# cm_var2 <- "table"
# cm_paired = "independent"
# cm_alternative = "two.sided"
# cm_sig_level = .95
# cm_adjust = "none"
# cm_plots = "bar"

	vars <- c(cm_var1, cm_var2)
	# dat <- getdata_exp(dataset, vars)
	dat <- getdata_exp(dataset, vars, filt = data_filter)

	if(dat[,cm_var1] %>% is.factor) {
		colnames(dat) <- c("variable","values")
	} else {
		dat %<>% gather_("variable", "values", vars)
	}

	# check variances in the data
  if(dat %>% summarise_each(., funs(var(.,na.rm = TRUE))) %>% min %>% {. == 0})
  	return("Test could not be calculated. Please select another variable.")

	# resetting option to independent if the number of observations is unequal
  if(cm_paired == "paired")
    if(summary(dat$variable) %>% { max(.) != min(.) })
      cm_paired <- "independent (obs. per level unequal)"

	##############################################
	# flip the order of pairwise testing - part 1
	##############################################
  flip_alt <- c("two.sided" = "two.sided",
                "less" = "greater",
                "greater" = "less")
	##############################################

	pairwise.t.test(dat[,"values"], dat[,"variable"], pool.sd = FALSE,
	                p.adj = cm_adjust, paired = cm_paired == "paired",
                  alternative = flip_alt[cm_alternative]) %>% tidy -> res

	##############################################
	# flip the order of pairwise testing - part 2
	##############################################
	res[,c("group1","group2")] <- res[,c("group2","group1")]
	##############################################

	plot_height <- 400 * length(cm_plots)

	# from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
	ci_calc <- function(se, n, conf.lev = .95)
	 	se * qt(conf.lev/2 + .5, n-1)

	dat %>%
		group_by(variable) %>%
    summarise_each(funs(mean, n = length(.), sd,
                   			se = sd/sqrt(n),
                   			ci = ci_calc(se,n,cm_sig_level))) %>%
    rename_(.dots = setNames("variable", " ")) -> dat_summary

	vars <- paste0(vars, collapse=", ")
  environment() %>% as.list %>% set_class(c("compare_means",class(.)))
}

# library(broom)
# library(dplyr)
# library(tidyr)
# library(magrittr)
# library(ggplot2)
# source("~/gh/radiant_dev/R/radiant.R")
# rm(r_env)
# result <- compare_means("diamonds","cut","price",cm_alternative = "greater")
# summary(result)
# load("~/Desktop/PairwiseBrand.rda")
# PairwiseBrand
# result <- compare_means("PairwiseBrand","BrandA","BrandB",cm_alternative = "less")
# summary(result)

#' Summarize method for output from compare_means
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @examples
#' result <- compare_means("diamonds","price",c("carats","table"))
#' summary(result)
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{plot.compare_means}} to plot results
#'
#' @export
summary.compare_means <- function(result) {

  if(result$cm_adjust == "bonf") {
    cat("Pairwise comparisons (bonferroni adjustment)\n")
  } else {
	  cat("Pairwise comparisons (no adjustment)\n")
  }

	cat("Data     :", result$dataset, "\n")
	if(result$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
	cat("Variables:", result$vars, "\n")
	cat("Samples  :", result$cm_paired, "\n\n")

  result$dat_summary[,-1] %<>% round(3)
  print(result$dat_summary %>% as.data.frame, row.names = FALSE)
	cat("\n")

  hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[result$cm_alternative]

  means <- result$dat_summary$mean
  names(means) <- result$dat_summary$` ` %>% as.character

	mod <- result$res
	mod$`Alt. hyp.` <- paste(mod$group1,hyp_symbol,mod$group2," ")
	mod$`Null hyp.` <- paste(mod$group1,"=",mod$group2, " ")
	mod$diff <- means[mod$group1 %>% as.character] -
							means[mod$group2 %>% as.character]
	mod <- mod[,c("Alt. hyp.", "Null hyp.", "diff", "p.value")]
	mod$` ` <- sig_stars(mod$p.value)
	mod$p.value <- round(mod$p.value,3)
	mod$p.value[ mod$p.value < .001 ] <- "< .001"
	print(mod, row.names = FALSE, right = FALSE)
	cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' Plot results from the compare_means function. This is a method of class compare_means and can be called as plot or plot.compare_means
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/compare_means.html} for an example in Radiant
#'
#' @examples
#' result <- compare_means("diamonds","price",c("carats","table"))
#' plot(result)
#'
#' @seealso \code{\link{compare_means}} to calculate results
#' @seealso \code{\link{summary.compare_means}} to summarize results
#'
#' @export
plot.compare_means <- function(result) {

	dat <- result$dat
	var1 <- colnames(dat)[1]
	var2 <- colnames(dat)[-1]

	# from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
	plots <- list()
	if("bar" %in% result$cm_plots) {
		colnames(result$dat_summary)[1] <- "variable"
		# use of `which` allows the user to change the order of the plots shown
		plots[[which("bar" == result$cm_plots)]] <- ggplot(result$dat_summary,
	    aes_string(x="variable", y="mean", fill="variable")) +
	    geom_bar(stat="identity")  +
	 		geom_errorbar(width=.1, aes(ymin=mean-ci, ymax=mean+ci)) +
	 		geom_errorbar(width=.05, aes(ymin=mean-se, ymax=mean+se), colour = "blue")
	}

	# graphs on full data
	if("box" %in% result$cm_plots) {
		plots[[which("box" == result$cm_plots)]] <-
			ggplot(dat, aes_string(x=var1, y=var2, fill=var1)) +
				geom_boxplot(alpha=.7)
	}

	if("density" %in% result$cm_plots) {
		plots[[which("density" == result$cm_plots)]] <-
			ggplot(dat, aes_string(x=var2, fill=var1)) + geom_density(alpha=.7)
	}

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}
