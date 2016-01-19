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
                        data_filter = "") {

	# dataset <- "bbb"
	# pred <- c("last","total")
	# rvar <- "buyer"
	# method <- "ntile"
	# qnt <- 10
	# resp_lev <- 1
	# data_filter <- ""

	# print(match.call())

	if (is_empty(qnt)) qnt <- 10

	vars <- c(pred, rvar)
	dat <- getdata(dataset, vars, filt = data_filter)
	if (!is_string(dataset)) dataset <- "-----"

  ## converting factors for interger (1st level)
  ## see also R/visualize.R
  # if ("factor" %in% class(dat[[rvar]]))
  #   dat[[rvar]] %<>% {as.integer(. == levels(.)[resp_lev])}

  rv <- dat[[rvar]]
  # if (lev == "") {
  #   if (is.factor(rv))
  #     lev <- levels(rv)[1]
  #   else
  #     lev <- rv %>% as.character %>% as.factor %>% levels %>% .[1]
  # }
  if (is.factor(rv))
    levs <- levels(rv)
  else
    levs <- rv %>% as.character %>% as.factor %>% levels

  if (lev == "") {
  	lev <- levs[1]
  } else {
  	if (!lev %in% levs) return(set_class("", c("performance",class("character"))))
  }

  ## transformation to TRUE/FALSE depending on the selected level (lev)
  dat[[rvar]] <- dat[[rvar]] == lev

	# tot_resp = sum(dat[[rvar]] == 1)
	tot_resp = sum(dat[[rvar]])
	tot_obs = nrow(dat)
	tot_rate = tot_resp / tot_obs

  # dots <- sub("^","~",dots) %>% lapply(as.formula, env = r_env)
  # nnames <- paste0(pred,"_",qnt)
  # qnt_name <- paste0("quant_",qnt)
  qnt_name <- "bins"
  if (method == "xtile") method <- "radiant::xtile"

  # dots <- sub("^","~",dots) %>% lapply(as.formula, env = r_env)
  # dots <- as.formula(paste0("~",method))
  # nvar <- try(group_by_(dataset, .dots = byvar) %>% mutate_(.dots = setNames(dots, vars)), silent = TRUE)

  ## tip for summarise_ from http://stackoverflow.com/a/27592077/1974918
  ## put summaries in list so you can print and plot
  auc_list <- rep(0, length(pred)) %>% set_names(pred)

  dat_list <- list()
  for (i in seq_along(pred)) {
  	pname <- pred[i]
  	auc_list[[pname]] <- auc(dat[[pname]],dat[[rvar]], TRUE)
  	dat_list[[pname]] <-
		  dat %>%
		  select_(.dots = c(pred[i],rvar)) %>%
			# mutate_each_(funs(xtile(.,qnt)), vars = pred[i]) %>%
			# mutate_each_(funs(get(method)(.,qnt)), vars = pred[i]) %>%
			# mutate_each_(funs_(paste0(method,"(.,qnt)")), vars = pred[i]) %>%
			mutate_(.dots = setNames(paste0(method,"(",pred[i],",", qnt,")"), pred[i])) %>%
			# mutate_(.dots = setNames(as.formula(paste0("~",method,"(",pred[i],",", qnt,")")), pred[i])) %>%
			setNames(c(qnt_name,rvar)) %>%
	    group_by_(.dots = qnt_name) %>%
		  summarise_(.dots = c(
		    nr_obs = "n()",
		    nr_resp = paste0("sum(",rvar,")")
		    # nr_resp = paste0("sum(",rvar," == 1)")
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
		  mutate(pred = pred[i]) %>%
		  select(pred, everything())
	}
	dat <- bind_rows(dat_list)
	rm(dat_list)

	environment() %>% as.list %>% set_class(c("performance",class(.)))
}

	# dataset <- "bbb"
	# pred <- "last"
	# rvar <- "buyer"
	# qnt <- 10
	# data_filter <- ""
 #  object <- performance("bbb", "last", "buyer")

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
		cat("Data      :", object$dataset, "\n")
		if (object$data_filter %>% gsub("\\s","",.) != "")
			cat("Filter    :", gsub("\\n","", object$data_filter), "\n")
		cat("Perdictors:", paste0(object$pred, collapse=", "), "\n")
		cat("Response  :", object$rvar, "\n")
	  cat("Level     :", object$lev, "in", object$rvar, "\n")
		cat("Method    :", gsub("radiant::","",object$method), "\n")
		cat("Bins      :", object$qnt, "\n")
		cat("AUC       :", paste0(object$pred, "(", round(object$auc,3), ")", collapse=", "), "\n\n")

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
		if (length(object$pred) < 2)
			plot_list[[i]] <- plot_list[[i]] + theme(legend.position = "none")
		else
			plot_list[[i]] <- plot_list[[i]] + labs(colour = "Predictor")
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
 	denom <- length(x1) * length(x2)
	wilcox.test(x1, x2, exact = FALSE)$statistic / denom
}
