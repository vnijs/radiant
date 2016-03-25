#' Compare proportions across groups
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_props.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param var1 A grouping variable to split the data for comparisons
#' @param var2 The variable to calculate proportions for
#' @param levs The factor level selected for the proportion comparison
#' @param alternative The alternative hypothesis ("two.sided", "greater" or "less")
#' @param conf_lev Span of the confidence interval
#' @param comb Combinations to evaluate
#' @param adjust Adjustment for multiple comparisons ("none" or "bonf" for Bonferroni)
#' @param dec Number of decimals to show
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables defined in the function as an object of class compare_props
#'
#' @examples
#' result <- compare_props("titanic", "pclass", "survived")
#' result <- titanic %>% compare_props("pclass", "survived")
#'
#' @seealso \code{\link{summary.compare_props}} to summarize results
#' @seealso \code{\link{plot.compare_props}} to plot results
#'
#' @importFrom tidyr spread_
#'
#' @export
compare_props <- function(dataset, var1, var2,
                         levs = "",
                         alternative = "two.sided",
                         conf_lev = .95,
                         comb = "",
                         adjust = "none",
                         dec = 3,
                         data_filter = "") {

	vars <- c(var1, var2)
	dat <- getdata(dataset, vars, filt = data_filter) %>% mutate_each(funs(as.factor))
	if (!is_string(dataset)) dataset <- "-----"

	lv <- levels(dat[[var2]])
	if (levs != "") {
		if (levs %in% lv && lv[1] != levs) {
			dat[[var2]] %<>% as.character %>% as.factor %>% relevel(levs)
			lv <- levels(dat[[var2]])
		}
	}

  ## check there is variation in the data
  if (any(summarise_each(dat, funs(does_vary)) == FALSE))
  	return("One or more selected variables show no variation. Please select other variables." %>%
  	       set_class(c("compare_props",class(.))))

  rn <- ""
  prop_input <-
	  dat %>%
	  group_by_(var1, var2) %>%
	  summarise(n = n()) %>%
	  spread_(var2, "n") %>%
	  as.data.frame %>%
	  { rn <<- .[[1]] %>% as.character
		  select(., -1) %>%
		  as.matrix %>%
		  set_rownames(rn)
	  }

	prop_input[is.na(prop_input)] <- 0

  lv <- rownames(prop_input)
  cmb <- combn(lv, 2) %>% t %>% as.data.frame

  rownames(cmb) <- cmb %>% apply(1, paste, collapse = ":")
  colnames(cmb) <- c("group1","group2")

	if (!is_empty(comb)) {
		if (all(comb %in% rownames(cmb))) {
			cmb <- cmb[comb, ]
		} else {
			cmb <- cmb[1,]
  	}
	}

  res <- cmb
  res[ ,c("chisq.value","p.value", "df", "ci_low", "ci_high", "sim")] <- 0
  for (i in 1:nrow(cmb)) {
  	ind <- c(which(cmb[i,1] == rownames(prop_input)), which(cmb[i,2] == rownames(prop_input)))

    pinp <- prop_input[ind,]

  	res[i, c("chisq.value","p.value", "df", "ci_low", "ci_high")] <-
	    sshhr( prop.test(pinp, alternative = alternative, conf.level = conf_lev,
	             correct = FALSE) ) %>%
	    tidy %>% .[1, c("statistic", "p.value", "parameter", "conf.low", "conf.high")]

    n <- rowSums(pinp)
    p <- pinp[[1]] / n
    E <- cbind(n * p, n * (1 - p))
    if (any(E < 5)) {
    	res[i, "p.value"] <- sshhr( chisq.test(pinp, simulate.p.value = TRUE, B = 2000) %>% tidy %>% .$p.value )
    	res[i, "df"] <- NA
    }
  }

	if (adjust != "none")
		res$p.value %<>% p.adjust(method = adjust)

	## from http://www.cookbook-r.com/Graphs/Plotting_props_and_error_bars_(ggplot2)/
	ci_calc <- function(se, conf.lev = .95)
	 	se * qnorm(conf.lev/2 + .5, lower.tail = TRUE)

	dat_summary <-
		prop_input %>%
			data.frame(check.names = FALSE) %>%
			mutate(n = rowSums(.[,1:2]), p = .[[1]] / n,
						 se = (p * (1 - p) / n) %>% sqrt,
	       		 ci = ci_calc(se, conf_lev)) %>%
			set_rownames({rownames(prop_input)}) %>%
			add_rownames(var = var1)

	dat_summary[[var1]] %<>% factor(., levels = .)

	vars <- paste0(vars, collapse = ", ")
  environment() %>% as.list %>% set_class(c("compare_props",class(.)))
}

#' Summary method for the compare_props function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_props.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{compare_props}}
#' @param show Show additional output (i.e., chisq.value, df, and confidence interval)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_props("titanic", "pclass", "survived")
#' summary(result)
#' titanic %>% compare_props("pclass", "survived") %>% summary
#'
#' @seealso \code{\link{compare_props}} to calculate results
#' @seealso \code{\link{plot.compare_props}} to plot results
#'
#' @export
summary.compare_props <- function(object, show = FALSE, ...) {

	if (is.character(object)) return(object)
	dec <- object$dec

  cat("Pairwise proportion comparisons\n")
	cat("Data      :", object$dataset, "\n")
	if (object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter    :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables :", object$vars, "\n")
	cat("Level     :", object$levs, "in", object$var2, "\n")
	cat("Confidence:", object$conf_lev, "\n")
	cat("Adjustment:", if (object$adjust == "bonf") "Bonferroni" else "None", "\n\n")

  object$dat_summary[,-1] %<>% round(dec)
  print(object$dat_summary %>% as.data.frame, row.names = FALSE)
	cat("\n")

  hyp_symbol <- c("two.sided" = "not equal to",
                  "less" = "<",
                  "greater" = ">")[object$alternative]

  props <- object$dat_summary$p
  names(props) <- object$rn

	## determine lower and upper % for ci
	ci_perc <- ci_label(object$alternative, object$conf_lev)

	res <- object$res
	res$`Alt. hyp.` <- paste(res$group1,hyp_symbol,res$group2," ")
	res$`Null hyp.` <- paste(res$group1,"=",res$group2, " ")
	res$diff <- (props[res$group1 %>% as.character] - props[res$group2 %>% as.character]) %>% round(dec)

	res_sim <- is.na(res$df)
	if (show) {
	  res <- res[,c("Null hyp.", "Alt. hyp.", "diff", "p.value", "chisq.value", "df", "ci_low", "ci_high")]
		res[,c("chisq.value","ci_low","ci_high")] %<>% round(dec)

		## apparantely you can get negative number here
		res$ci_low[res$ci_low < 0] <- 0
		res$df[res_sim] <- "*1*"
	  res <- rename_(res, .dots = setNames(c("ci_low","ci_high"), ci_perc))
	} else {
	  res <- res[,c("Null hyp.", "Alt. hyp.", "diff", "p.value")]
	}

	res$` ` <- sig_stars(res$p.value)
	res$p.value[res$p.value >= .001] %<>% round(dec)
	res$p.value[res$p.value < .001] <- "< .001"
	res$p.value[res_sim] %<>% paste0(" (2000 replicates)")
	print(res, row.names = FALSE, right = FALSE)
	cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
}

#' Plot method for the compare_props function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/compare_props.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{compare_props}}
#' @param plots One or more plots of proportions ("bar" or "dodge")
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- compare_props("titanic", "pclass", "survived")
#' plot(result, plots = c("bar","dodge"))
#'
#' @seealso \code{\link{compare_props}} to calculate results
#' @seealso \code{\link{summary.compare_props}} to summarize results
#'
#' @export
plot.compare_props <- function(x,
                               plots = "bar",
                               shiny = FALSE,
                               ...) {

	if (is.character(x)) return(x)
	object <- x; rm(x)

	dat <- object$dat
	v1 <- colnames(dat)[1]
	v2 <- colnames(dat)[-1]
	lev_name <- object$levs

	## from http://www.cookbook-r.com/Graphs/Plotting_props_and_error_bars_(ggplot2)/
	plot_list <- list()
	if ("bar" %in% plots) {
		## use of `which` allows the user to change the order of the plots shown
		plot_list[[which("bar" == plots)]] <-
			ggplot(object$dat_summary, aes_string(x = v1, y = "p", fill = v1)) +
				geom_bar(stat = "identity") +
		 		geom_errorbar(width = .1, aes(ymin = p-ci, ymax = p+ci)) +
		 		geom_errorbar(width = .05, aes(ymin = p-se, ymax = p+se), colour = "blue") +
		 		theme(legend.position = "none") +
		 		scale_y_continuous(labels = scales::percent) +
		 		ylab(paste0("Proportion of \"", lev_name, "\" in ", v2))

	}

	if ("dodge" %in% plots) {
		plot_list[[which("dodge" == plots)]] <-
			dat %>%
				group_by_(v1, v2) %>%
				summarise(count = n()) %>%
				group_by_(v1) %>%
				mutate(perc = count/ sum(count)) %>%
				ggplot(aes_string(x = v1, y = "perc", fill = v2)) +
					geom_bar(stat = "identity", position = "dodge") +
			 		scale_y_continuous(labels = scales::percent) +
			 		ylab(paste0("Proportions per level of ", v1))
	}

	sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
 	  { if (shiny) . else print(.) }
}
