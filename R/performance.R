#' Model performance
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/performance.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param pred Prediction or predictor
#' @param rvar Response variable
#' @param lev The level in the response variable defined as _success_
#' @param qnt Number of bins to create
#' @param method Use either ntile or xtile to split the data (default is xtile)
#' @param train Use data from training ("Training"), validation ("Validation"), both ("Both"), or all data ("All") to evaluate model performance
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#'
#' @return A list of results
#'
#' @seealso \code{\link{summary.performance}} to summarize results
#' @seealso \code{\link{plot.performance}} to plot results
#'
#' @examples
#' result <- performance("titanic", c("age","fare"), "survived")
#'
#' @export
performance <- function(dataset, pred, rvar,
                        lev = "",
                        qnt = 10,
                        method = "xtile",
                        train = "",
                        data_filter = "") {

	# library(radiant)
	# dataset <- "bbb"
	# pred <- c("last","total")
	# rvar <- "buyer"
	# method <- "xtile"
	# lev <- ""
	# qnt <- 10
	# resp_lev <- 1
	# data_filter <- ""
	# train <- "All"

	## to avoid 'global not defined' warnings
	nr_resp <- nr_obs <- cum_resp <- cum_resp_rate <- everything <- NULL

	if (is_empty(qnt)) qnt <- 10

	dat_list <- list()
	vars <- c(pred, rvar)
	if (train == "Both") {
		dat_list[["Training"]] <- getdata(dataset, vars, filt = data_filter)
		dat_list[["Validation"]] <- getdata(dataset, vars, filt = paste0("!(",data_filter,")"))
	} else if (train == "Training") {
		dat_list[["Training"]] <- getdata(dataset, vars, filt = data_filter)
	} else if (train == "Validation") {
		dat_list[["Validation"]] <- getdata(dataset, vars, filt = paste0("!(",data_filter,")"))
	} else {
		dat_list[["All"]] <- getdata(dataset, vars, filt = "")
	}

	if (!is_string(dataset)) dataset <- "-----"

  qnt_name <- "bins"
  if (method == "xtile") method <- "radiant::xtile"

  auc_list <- list()
  lg_list <- list()
	pext <- c(All = "", Training = " (train)", Validation = " (val)")

	for (i in names(dat_list)) {
		dat <- dat_list[[i]]
	  rv <- dat[[rvar]]
	  if (is.factor(rv)) {
	    levs <- levels(rv)
	  } else {
	    levs <- rv %>% as.character %>% as.factor %>% levels
	  }

	  if (lev == "") {
	  	lev <- levs[1]
	  } else {
	  	if (!lev %in% levs) return(set_class("", c("performance",class("character"))))
	  }

	  ## transformation to TRUE/FALSE depending on the selected level (lev)
	  dat[[rvar]] <- dat[[rvar]] == lev

	  ## tip for summarise_ from http://stackoverflow.com/a/27592077/1974918
	  ## put summaries in list so you can print and plot
		# tot_resp = sum(dat[[rvar]] == 1)
		tot_resp = sum(dat[[rvar]])
		tot_obs = nrow(dat)
		tot_rate = tot_resp / tot_obs

	  for (j in seq_along(pred)) {
	  	pname <- paste0(pred[j], pext[i])
	  	auc_list[[pname]] <- auc(dat[[pred[j]]],dat[[rvar]], TRUE)[["W"]]
	  	lg_list[[pname]] <-
			  dat %>%
			  select_(.dots = c(pred[j],rvar)) %>%
				mutate_(.dots = setNames(paste0(method,"(",pred[j],",", qnt,")"), pred[j])) %>%
				setNames(c(qnt_name,rvar)) %>%
		    group_by_(.dots = qnt_name) %>%
			  summarise_(.dots = c(
			    nr_obs = "n()",
			    nr_resp = paste0("sum(",rvar,")")
			  )) %>%
			  mutate(
			    resp_rate = nr_resp / nr_obs,
			    gains = nr_resp / tot_resp
			  ) %>%
			  { if (first(.$resp_rate) < last(.$resp_rate)) mutate_each(., funs(rev))
			  	else . } %>%
			  # arrange(desc(resp_rate)) %>%
			  mutate(
			    cum_prop = cumsum(nr_obs / tot_obs),
			    cum_resp = cumsum(nr_resp),
			    cum_resp_rate = cum_resp / cumsum(nr_obs),
			    cum_lift = cum_resp_rate / tot_rate,
			    cum_gains = cum_resp / tot_resp
			  ) %>%
			  mutate(pred = pname) %>%
			  select(pred, everything())
		}
	}
	dat <- bind_rows(lg_list)
	rm(lg_list)

	environment() %>% as.list %>% set_class(c("performance",class(.)))
}

#' Summary method for the performance function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/performance.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{performance}}
#' @param prn Print model performance results (default is TRUE)
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{performance}} to summarize results
#' @seealso \code{\link{plot.performance}} to plot results
#'
#' @examples
#' performance("titanic", "age", "survived") %>% summary
#' performance("titanic", c("age","fare"), "survived") %>% summary
#'
#' @export
summary.performance <- function(object, prn = TRUE, ...) {

  if (is.character(object)) return(object)

	if (prn) {
		cat("Model performance\n")
		cat("Data       :", object$dataset, "\n")
		if (object$data_filter %>% gsub("\\s","",.) != "")
			cat("Filter     :", gsub("\\n","", object$data_filter), "\n")
		cat("Results for:", object$train, "\n")
		cat("Perdictors :", paste0(object$pred, collapse=", "), "\n")
		cat("Response   :", object$rvar, "\n")
	  cat("Level      :", object$lev, "in", object$rvar, "\n")
		# cat("Method    :", gsub("radiant::","",object$method), "\n")
		cat("Bins       :", object$qnt, "\n")
		auc <- unlist(object$auc_list)
		cat("AUC        :", paste0(names(auc), " (", round(auc,3), ")", collapse=", "), "\n\n")
		print(dfprint(as.data.frame(object$dat), 3), row.names = FALSE)
	} else {
    return(object$dat %>% set_class(c("performance",class(.))))
	}
}

#' Plot method for the performance function
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/performance.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{performance}}
#' @param plots Plots to return
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @seealso \code{\link{performance}} to generate results
#' @seealso \code{\link{summary.performance}} to summarize results
#'
#' @examples
#' performance("titanic", "age", "survived") %>% plot
#' performance("titanic", c("age","fare"), "survived") %>% plot
#' performance("titanic", c("age","fare"), "survived", method = "xtile") %>% plot
#' performance("titanic", c("age","fare"), "survived") %>% summary
#'
#' @export
plot.performance <- function(x,
                             plots = c("lift","gains"),
                             shiny = FALSE,
                             ...) {


	## to avoid 'global not defined' warnings
	pred <- cum_prop <- cum_gains <- obs <- NULL

	object <- x; rm(x)
  if (is.character(object) || is.null(object$dat) || any(is.na(object$dat$cum_lift)) ||
      is.null(plots)) return(invisible())

	plot_list <- list()
	if ("lift" %in% plots) {
		plot_list[["lift"]] <-
			visualize(object$dat, xvar = "cum_prop", yvar = "cum_lift", type = "line", color = "pred", custom = TRUE) +
			geom_point() +
			geom_segment(aes(x = 0, y = 1, xend = 1, yend = 1), size = .1, color = "black") +
			ylab("Cumulative lift") +
			xlab("Proportion of customers")
	}

	if ("gains" %in% plots) {
		init <- object$dat[1,] %>% {.[1,] <- 0; .}
		dat <-
	    object$dat %>%
		  select(pred, cum_prop, cum_gains) %>%
		  group_by(pred) %>%
		  mutate(obs = 1:n())
		init <- dat %>% filter(obs == 1)
		init$cum_prop <- init$cum_gains <- init$obs <- 0
		dat <- bind_rows(init, dat) %>% arrange(pred, obs)

		plot_list[["gains"]] <-
		  visualize(dat, xvar = "cum_prop", yvar = "cum_gains", type = "line", color = "pred", custom = TRUE) +
			geom_point() +
			geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), size = .1, color = "black") +
			ylab("Cumulative gains") +
			xlab("Proportion of customers")
	}

	for (i in names(plot_list)) {
		if (length(object$pred) < 2 && object$train != "Both")
			plot_list[[i]] <- plot_list[[i]] + theme(legend.position = "none")
		else
			plot_list[[i]] <- plot_list[[i]] + labs(colour = "Predictor")
	}

	dots <- list(...)
  if ("custom" %in% names(dots) && dots$custom == TRUE) {
    if (length(plot_list) == 1) return(plot_list[[plots]]) else return(plot_list)
  }

	sshhr( do.call(gridExtra::arrangeGrob, c(plot_list, list(ncol = 1))) ) %>%
	 	{ if (shiny) . else print(.) }
}

#' Area Under the Curve (AUC)
#'
#' @details See \url{http://vnijs.github.io/radiant/analytics/performance.html} for an example in Radiant
#'
#' @param pred Prediction or predictor
#' @param rvar Response variable
#' @param lev The level in the response variable defined as _success_
#'
#' @return AUC statistic
#'
#' @seealso \code{\link{performance}} to calculate results
#' @seealso \code{\link{summary.performance}} to summarize results
#' @seealso \code{\link{plot.performance}} to plot results
#'
#' @examples
#' auc(mtcars$mpg, mtcars$vs, 1)
#'
#' @export
auc <- function(pred, rvar, lev) {
 	## based on examples in colAUC at ...
 	## https://cran.r-project.org/web/packages/caTools/caTools.pdf
 	stopifnot(length(lev) == 1, lev %in% rvar)
 	x1 <- pred[rvar == lev]
  x2 <- pred[rvar != lev]
 	denom <- as.numeric(length(x1)) * length(x2)
	wt <- wilcox.test(x1, x2, exact = FALSE)$statistic / denom
	ifelse (wt < .5, 1 - wt, wt)
}
