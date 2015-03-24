#' Compare proportions across groups
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_props.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param cp_var1 A grouping variable to split the data for comparisons
#' @param cp_var2 The variable to calculate proportions for
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param cp_levels The factor level selected for the proportion comparison
#' @param cp_alternative The alternative hypothesis ("two.sided", "greater" or "less")
#' @param cp_sig_level Span of the confidence interval
#' @param cp_adjust Adjustment for multiple comparisons ("none" or "bonf" for Bonferroni)
#'
#' @return A list of all variables defined in the function as an object of class compare_props
#'
#' @examples
#' result <- compare_props("titanic", "pclass", "survived")
#'
#' @seealso \code{\link{summary.compare_props}} to summarize results
#' @seealso \code{\link{plot.compare_props}} to plot results
#'
#' @importFrom tidyr spread_
#'
#' @export
compare_props <- function(dataset, cp_var1, cp_var2,
                         data_filter = "",
                         cp_levels = "",
                         cp_alternative = "two.sided",
                         cp_sig_level = .95,
                         cp_adjust = "none") {

	vars <- c(cp_var1, cp_var2)
	dat <- getdata(dataset, vars, filt = data_filter) %>% mutate_each(funs(as.factor))

	levs <- levels(dat[,cp_var2])
	if(cp_levels != "") {
		# levs <- levels(dat[,cp_var2])
		if(cp_levels %in% levs && levs[1] != cp_levels) {
			dat[,cp_var2] %<>% as.character %>% as.factor %>% relevel(cp_levels)
			levs <- levels(dat[,cp_var2])
		}
	}

	# check variances in the data
  if(dat %>% summarise_each(., funs(var(.,na.rm = TRUE))) %>% min %>% {. == 0})
  	return("Test could not be calculated. Please select another variable.")

  rn <- ""
  dat %>%
  group_by_(cp_var1, cp_var2) %>%
  summarise(n = n()) %>%
  spread_(cp_var2, "n") %>%
  {
  	.[,1][[1]] %>% as.character ->> rn
	  select(., -1) %>%
	  as.matrix %>%
	  set_rownames(rn)
  } -> prop_input

	##############################################
	# flip the order of pairwise testing - part 1
	##############################################
  flip_alt <- c("two.sided" = "two.sided",
                "less" = "greater",
                "greater" = "less")
	##############################################

	sshhr( pairwise.prop.test(prop_input, p.adjust.method = cp_adjust,
	       alternative = flip_alt[cp_alternative]) ) %>% tidy -> res

	##############################################
	# flip the order of pairwise testing - part 2
	##############################################
	res[,c("group1","group2")] <- res[,c("group2","group1")]
	##############################################

	# from http://www.cookbook-r.com/Graphs/Plotting_props_and_error_bars_(ggplot2)/
	ci_calc <- function(se, conf.lev = .95)
	 	se * qnorm(conf.lev/2 + .5, lower.tail = TRUE)

	class(prop_input)

	prop_input %>%
		data.frame %>%
		mutate(n = .[,1:2] %>% rowSums, p = .[,1] / n,
					 se = (p * (1-p) / n) %>% sqrt,
       		 ci = ci_calc(se, cp_sig_level)) %>%
		set_rownames({prop_input %>% rownames})-> dat_summary

	vars <- paste0(vars, collapse=", ")
  environment() %>% as.list %>% set_class(c("compare_props",class(.)))
}

#' Summary method for the compare_props function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_props.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{compare_props}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_props("titanic", "pclass", "survived")
#' summary(result)
#'
#' @seealso \code{\link{compare_props}} to calculate results
#' @seealso \code{\link{plot.compare_props}} to plot results
#'
#' @export
summary.compare_props <- function(object, ...) {

  if(object$cp_adjust == "bonf") {
    cat("Pairwise comparisons (bonferroni adjustment)\n")
  } else {
	  cat("Pairwise comparisons (no adjustment)\n")
  }

	cat("Data     :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables:", object$vars, "\n")
	cat("Level    :", object$cp_levels, "in", object$cp_var2, "\n\n")

  object$dat_summary[,-1] %<>% round(3)
  print(object$dat_summary %>% as.data.frame, row.names = FALSE)
	cat("\n")

  hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[object$cp_alternative]

  props <- object$dat_summary$p
  names(props) <- object$rn
	res <- object$res
	res$`Alt. hyp.` <- paste(res$group1,hyp_symbol,res$group2," ")
	res$`Null hyp.` <- paste(res$group1,"=",res$group2, " ")
	res$diff <- (props[res$group1 %>% as.character] - props[res$group2 %>% as.character]) %>% round(3)
	res <- res[,c("Alt. hyp.", "Null hyp.", "diff", "p.value")]
	res$` ` <- sig_stars(res$p.value)
	res$p.value <- round(res$p.value,3)
	res$p.value[ res$p.value < .001 ] <- "< .001"
	print(res, row.names = FALSE, right = FALSE)
	cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' Plot method for the compare_props function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_props.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{compare_props}}
#' @param cp_plots One or more plots of proportions or counts ("props" or "counts")
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_props("titanic", "pclass", "survived")
#' plot(result, cp_plots = c("props","counts"))
#'
#' @seealso \code{\link{compare_props}} to calculate results
#' @seealso \code{\link{summary.compare_props}} to summarize results
#'
#' @export
plot.compare_props <- function(x,
                               cp_plots = "props",
                               ...) {

	object <- x; rm(x)

	dat <- object$dat
	var1 <- colnames(dat)[1]
	var2 <- colnames(dat)[-1]
	object$dat_summary[,var1] <- object$rn
	lev_name <- object$levs[1]

	# from http://www.cookbook-r.com/Graphs/Plotting_props_and_error_bars_(ggplot2)/
	plots <- list()
	if("props" %in% cp_plots) {
		# use of `which` allows the user to change the order of the plots shown
		plots[[which("props" == cp_plots)]] <-
			ggplot(object$dat_summary, aes_string(x = var1, y = "p", fill = var1)) +
			geom_bar(stat = "identity") +
	 		geom_errorbar(width = .1, aes(ymin = p-ci, ymax = p+ci)) +
	 		geom_errorbar(width = .05, aes(ymin = p-se, ymax = p+se), colour = "blue")
	}

	if("counts" %in% cp_plots) {
		plots[[which("counts" == cp_plots)]] <-
			ggplot(object$dat, aes_string(x = var1, fill = var2)) +
			geom_bar(position = "dodge")
	}

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}
