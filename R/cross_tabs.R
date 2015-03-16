#' Evaluate associations between categorical variables
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/cross_tabs.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param ct_var1 A categorical variable
#' @param ct_var2 Another categorical variable
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of all variables used in cross_tabs as an object of class cross_tabs
#'
#' @examples
#' result <- cross_tabs("newspaper", "Income", "Newspaper")
#'
#' @seealso \code{\link{summary.cross_tabs}} to summarize results
#' @seealso \code{\link{plot.cross_tabs}} to plot results
#'
#' @export
cross_tabs <- function(dataset, ct_var1, ct_var2,
                       data_filter = "") {

	dat <- getdata(dataset, c(ct_var1, ct_var2), filt = data_filter)

	dnn = c(paste("Group(",ct_var1,")",sep = ""), paste("Variable(",ct_var2,")",sep = ""))
	tab <- table(dat[,ct_var1], dat[,ct_var2], dnn = dnn)
	cst <- sshhr( chisq.test(tab, correct = FALSE) )

	# adding the % deviation table
	cst$deviation <- with(cst, (observed-expected) / expected)
	cst$chi_sq	<- with(cst, (observed - expected)^2 / expected)

	# dat not needed in summary or plot
	rm(dat)

  environment() %>% as.list %>% set_class(c("cross_tabs",class(.)))
}

#' Summary method for the cross_tabs function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/cross_tabs.html} for an example in Radiant

#' @param object Return value from \code{\link{cross_tabs}}
#' @param ct_check Show table(s) for variables ct_var1 and ct_var2. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "dev_perc" for the percentage difference between the observed and expected frequencies (i.e., (o - e) / e)
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' result <- cross_tabs("newspaper", "Income", "Newspaper")
#' summary(result, ct_check = c("observed","expected","chi_sq"))
#'
#' @seealso \code{\link{cross_tabs}} to calculate results
#' @seealso \code{\link{plot.cross_tabs}} to plot results
#'
#' @export
summary.cross_tabs <- function(object,
                               ct_check = "",
                               ...) {

  cat("Cross-tabs\n")
	cat("Data     :", object$dataset, "\n")
	if(object$data_filter %>% gsub("\\s","",.) != "")
		cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
	cat("Variables:", paste0(c(object$ct_var1, object$ct_var2), collapse=", "), "\n")
	cat("Null hyp.: there is no association between", object$ct_var1, "and", object$ct_var2, "\n")
	cat("Alt. hyp.: there is an association between", object$ct_var1, "and", object$ct_var2, "\n")

	object$cst$observed %>% rownames %>% c(., "Total") -> rnames
	object$cst$observed %>% colnames %>% c(., "Total") -> cnames

	if("observed" %in% ct_check) {
		cat("\nObserved:\n")
		object$cst$observed %>%
			rbind(colSums(.)) %>%
			set_rownames(rnames) %>%
			cbind(rowSums(.)) %>%
			set_colnames(cnames) %>%
			print
	}

	if("expected" %in% ct_check) {
		cat("\nExpected: (row total x column total) / total\n")
		object$cst$expected %>%
			rbind(colSums(.)) %>%
			set_rownames(rnames) %>%
			cbind(rowSums(.)) %>%
			set_colnames(cnames) %>%
			round(2) %>%
			print
	}

	if("chi_sq" %in% ct_check) {
		cat("\nContribution to chi-squared: (o - e)^2 / e\n")
		# ((object$cst$observed - object$cst$expected)^2 / object$cst$expected) %>%
		object$cst$chi_sq %>%
			rbind(colSums(.)) %>%
			set_rownames(rnames) %>%
			cbind(rowSums(.)) %>%
			set_colnames(cnames) %>%
			round(2) %>%
			print
	}

	if("dev_std" %in% ct_check) {
		cat("\nDeviation standardized: (o - e) / sqrt(e)\n")
		print(round(object$cst$residuals, 2)) 	# these seem to be the correct std.residuals
	}

	if("dev_perc" %in% ct_check) {
		cat("\nDeviation %: (o - e) / e\n")
		print(round(object$cst$deviation, 2)) 	# % deviation
	}
	# if(object$ct_cellperc) {
	# 	cat("\nCell percentages:\n")
	# 	print(prop.table(object$table), digits = 2)  	# cell percentages
	# }
	# if(object$ct_rowperc) {
	# 	cat("\nRow percentages:\n")
	# 	print(prop.table(object$table, 1), digits = 2) # row percentages
	# }
	# if(object$ct_colperc) {
	# 	cat("\nColumn percentages:\n")
	# 	print(prop.table(object$table, 2), digits = 2) # column percentages
	# }

	object$cst %>% tidy %>% round(3) -> res
	if(res$p.value < .001) res$p.value  <- "< .001"
	cat(paste0("\nChi-squared: ", res$statistic, " df(", res$parameter, "), p.value ", res$p.value), "\n\n")
	cat(paste(sprintf("%.1f",100 * (sum(object$cst$expected < 5) / length(object$cst$expected))),"% of cells have expected values below 5"), sep = "")
}

#' Plot method for the cross_tabs function
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/cross_tabs.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{cross_tabs}}
#' @param ct_check Show plots for variables ct_var1 and ct_var2. "observed" for the observed frequencies table, "expected" for the expected frequencies table (i.e., frequencies that would be expected if the null hypothesis holds), "chi_sq" for the contribution to the overall chi-squared statistic for each cell (i.e., (o - e)^2 / e), "dev_std" for the standardized differences between the observed and expected frequencies (i.e., (o - e) / sqrt(e)), and "dev_perc" for the percentage difference between the observed and expected frequencies (i.e., (o - e) / e)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- cross_tabs("newspaper", "Income", "Newspaper")
#' plot(result, ct_check = c("observed","expected","chi_sq"))
#'
#' @seealso \code{\link{cross_tabs}} to calculate results
#' @seealso \code{\link{summary.cross_tabs}} to summarize results
#'
#' @export
plot.cross_tabs <- function(x,
                            ct_check = "",
                            ...) {

	object <- x; rm(x)

	gather_table <- function(tab) {
		tab %>%
			data.frame(., check.names = FALSE) %>%
			mutate(rnames = rownames(.)) %>%
			{ sshhr( gather_(., "variable", "values") ) }
	}

	plots <- list()

	if("observed" %in% ct_check) {
		fact_names <- object$cst$observed %>% dimnames %>% as.list
  	tab <- gather_table(object$cst$observed)
		colnames(tab)[1:2] <- c(object$ct_var1, object$ct_var2)
		tab$object$ct_var1 %<>% as.factor %>% factor(levels = fact_names[[1]])
		tab$object$ct_var1 %<>% as.factor %>% factor(levels = fact_names[[2]])

		plots[['observed']] <-
		ggplot(tab, aes_string(x = object$ct_var1, y = "values", fill = object$ct_var2)) +
         			geom_bar(stat="identity", position = "fill", alpha = .7) +
         			labs(list(title = paste("Observed frequencies for ",object$ct_var2," versus ",object$ct_var1, sep = ""),
							x = "", y = "", fill = object$ct_var2))
	}

	if("expected" %in% ct_check) {
		fact_names <- object$cst$expected %>% dimnames %>% as.list
  	tab <- gather_table(object$cst$expected)
		tab$rnames %<>% as.factor %>% factor(levels = fact_names[[1]])
		tab$variable %<>% as.factor %>% factor(levels = fact_names[[2]])
		plots[['expected']] <- ggplot(tab, aes_string(x = "rnames", y = "values", fill = "variable")) +
         			geom_bar(stat="identity", position = "fill", alpha = .7) +
         			labs(list(title = paste("Expected frequencies for ",object$ct_var2," versus ",object$ct_var1, sep = ""),
							x = "", y = "", fill = object$ct_var2))
	}

	if("chi_sq" %in% ct_check) {

		tab <- gather_table(object$cst$chi_sq)
		# tab <- gather_table((object$cst$observed - object$cst$expected)^2 / object$cst$expected)
		colnames(tab)[1:2] <- c(object$ct_var1, object$ct_var2)
		plots[['chi_sq']] <- ggplot(tab, aes_string(x = object$ct_var1, y = "values", fill = object$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) +
         			labs(list(title = paste("Contribution to chi-squared for ",object$ct_var2," versus ",object$ct_var1, sep = ""), x = object$ct_var1))
  }

	if("dev_std" %in% ct_check) {
		tab <- gather_table(object$cst$residuals)
		colnames(tab)[1:2] <- c(object$ct_var1, object$ct_var2)
		plots[['dev_std']] <- ggplot(tab, aes_string(x = object$ct_var1, y = "values", fill = object$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) +
     					geom_hline(yintercept = c(-1.96,1.96,-1.64,1.64), color = 'black', linetype = 'longdash', size = .5) +
     					geom_text(data = NULL, x = 1, y = 2.11, label = "95%") +
     					geom_text(data = NULL, x = 1, y = 1.49, label = "90%") +
         			labs(list(title = paste("Deviation standardized for ",object$ct_var2," versus ",object$ct_var1, sep = ""), x = object$ct_var1))
	}

	if("dev_perc" %in% ct_check) {
		tab <- gather_table(object$cst$deviation)
		colnames(tab)[1:2] <- c(object$ct_var1, object$ct_var2)
		plots[['dev_prec']] <- ggplot(tab, aes_string(x = object$ct_var1, y = "values", fill = object$ct_var2)) +
         			geom_bar(stat="identity", position = "dodge", alpha = .7) + ylim(-1,1) +
         			labs(list(title = paste("Deviation % for ",object$ct_var2," versus ",object$ct_var1, sep = ""), x = object$ct_var1))
  }

	sshh( do.call(grid.arrange, c(plots, list(ncol = 1))) )
}
