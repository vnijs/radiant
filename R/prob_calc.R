#' Probability calculator for the normal distribution
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param mean Mean
#' @param stdev Standard deviation
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_norm <- function(mean,
                      stdev,
                      lb = NA,
                      ub = NA,
                      plb = NA,
                      pub = NA,
                      dec = 3) {

	p_ub <- pnorm(ub, mean, stdev)
	p_lb <- pnorm(lb, mean, stdev)
	p_int <- max(p_ub - p_lb, 0) %>% round(dec)

	p_ub %<>% round(dec)
	p_lb %<>% round(dec)

	if (!is.na(pub)) {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	}

	if (!is.na(plb)) {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	}

	v_ub <- qnorm(pub, mean, stdev) %>% round(dec)
	v_lb <- qnorm(plb, mean, stdev) %>% round(dec)

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(plb) && !is.na(pub)) {
		if (plb > pub) {
			plb <- pub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  environment() %>% as.list %>% set_class(c("prob_norm",class(.)))
}

#' Plot method for the probability calculator (normal)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_norm}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.prob_norm <- function(x, type = "values", shiny = FALSE, ...) {

	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)

	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$v_lb
		ub <- object$v_ub
	}

	mean <- object$mean
	stdev <- object$stdev

  limits <- c(mean - 3*stdev, mean + 3*stdev)

  dnorm_limit <- function(x) {
    y <- dnorm(x, mean = mean, sd = stdev)
    # y[x < lb | x > ub] <- NA
    y[x < lb | x > ub] <- 0
    y
  }

  dnorm_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dnorm(x, mean = mean, sd = stdev)
    # y[x > lb] <- NA
    y[x > lb] <- 0
    y
  }

  dnorm_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dnorm(x, mean = mean, sd = stdev)
    # y[x < ub] <- NA
    y[x < ub] <- 0
    y
  }

  dnorm_lines <- c(ub,lb) %>% na.omit
  if (length(dnorm_lines) == 0) dnorm_lines <- c(-Inf, Inf)

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	plt <- ggplot(data.frame(x=limits), aes_string(x="x")) +
	  stat_function(fun=dnorm, args = list(mean = mean, sd = stdev)) +
	  stat_function(fun=dnorm_limit, geom="area", fill="blue", alpha=0.2, n = 501) +
	  stat_function(fun=dnorm_lb, geom="area", fill="red", alpha=0.2, n = 501) +
	  stat_function(fun=dnorm_ub, geom="area", fill="red", alpha=0.2, n = 501) +
	  geom_vline(xintercept = dnorm_lines, color = 'black', linetype = 'dashed', size = .5) +
	  xlab("") + ylab("")

   if (shiny) plt else print(plt)
}

#' Summary method for the probability calculator function (normal)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_norm}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.prob_norm <- function(object, type = "values",  ...) {

	mean <- object$mean
	stdev <- object$stdev
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb

	v_ub <- object$v_ub
	v_lb <- object$v_lb

  cat("Probability calculator\n")
  cat("Distribution: Normal\n")
	cat("Mean        :", mean, "\n")
	cat("St. dev     :", stdev, "\n")

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound :", if (is.na(lb)) "-Inf" else lb, "\n")
		cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X < ", lb,") = ", p_lb, "\n"))
				cat(paste0("P(X > ", lb,") = ", round(1 - p_lb, dec), "\n"))
			}

			if (!is.na(ub)) {
				cat(paste0("P(X < ", ub,") = ", p_ub, "\n"))
				cat(paste0("P(X > ", ub,") = ", round(1 - p_ub, dec), "\n"))
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " < X < ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " < X < ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {
		pub <- if (is.na(pub)) 2 else pub
		plb <- if (is.na(plb)) -1 else plb

		cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
		cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

		if (pub <= 1 || plb >= 0) {
		  cat("\n")

			if (plb >= 0) {
				cat(paste0("P(X < ", v_lb,") = ", plb, "\n"))
				cat(paste0("P(X > ", v_lb,") = ", round(1 - plb, dec), "\n"))
			}

			if (pub <= 1) {
				cat(paste0("P(X < ", v_ub,") = ", pub, "\n"))
				cat(paste0("P(X > ", v_ub,") = ", round(1 - pub, dec), "\n"))
			}

		  if (pub <= 1 && plb >= 0) {
				cat(paste0("P(", v_lb, " < X < ", v_ub,")     = ", pub - plb, "\n"))
				cat(paste0("1 - P(", v_lb, " < X < ", v_ub,") = ", round(1 - (pub - plb), dec), "\n"))
			}
		}
	}
}

#' Probability calculator for the t distribution
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param df Degrees of freedom
#' @param mean Mean
#' @param stdev Standard deviation
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_tdist <- function(df,
                       mean = 0,
                       stdev = 1,
                       lb = NA,
                       ub = NA,
                       plb = NA,
                       pub = NA,
                       dec = 3) {

	## transform ub and lb to t-stats
	## values need to be transformed to regular units using sd and mean

	# tstdev <- n/(n-2)
	# r_pt <- pt(ub, df)


	p_ub <- pt(ub, df)
	p_lb <- pt(lb, df)
	p_int <- max(p_ub - p_lb, 0)

	p_ub %<>% round(dec)
	p_lb %<>% round(dec)
	p_int %<>% round(dec)

	if (!is.na(pub)) {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	}

	if (!is.na(plb)) {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	}

	v_ub <- qt(pub, df)
	v_lb <- qt(plb, df)

	v_ub %<>% round(dec)
	v_lb %<>% round(dec)

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(plb) && !is.na(pub)) {
		if (plb > pub) {
			plb <- pub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  environment() %>% as.list %>% set_class(c("prob_tdist",class(.)))
}

#' Plot method for the probability calculator (t-distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_tdist}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.prob_tdist <- function(x, type = "values", shiny = FALSE, ...) {

	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)
	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$v_lb
		ub <- object$v_ub
	}

	df <- object$df

  limits <- c(-3, 3)
  dt_limit <- function(x) {
    y <- dt(x, df = df)
    # y[x < lb | x > ub] <- NA
    y[x < lb | x > ub] <- 0
    y
  }

  dt_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dt(x, df = df)
    # y[x > lb] <- NA
    y[x > lb] <- 0
    y
  }

  dt_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dt(x, df = df)
    # y[x < ub] <- NA
    y[x < ub] <- 0
    y
  }

  dt_lines <- c(ub,lb) %>% na.omit
  if (length(dt_lines) == 0) dt_lines <- c(-Inf, Inf)

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	plt <- ggplot(data.frame(x=limits), aes_string(x="x")) +
	  stat_function(fun=dt, args = list(df = df)) +
	  stat_function(fun=dt_limit, geom="area", fill="blue", alpha=0.2, n = 501) +
	  stat_function(fun=dt_lb, geom="area", fill="red", alpha=0.2, n = 501) +
	  stat_function(fun=dt_ub, geom="area", fill="red", alpha=0.2, n = 501) +
	  geom_vline(xintercept = dt_lines, color = 'black', linetype = 'dashed', size = .5) +
	  xlab("") + ylab("")

   if (shiny) plt else print(plt)
}


#' Summary method for the probability calculator function (t-distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_tdist}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.prob_tdist <- function(object, type = "values",  ...) {

	df <- object$df
	n <- df + 1
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb

	v_ub <- object$v_ub
	v_lb <- object$v_lb

  cat("Probability calculator\n")
  cat("Distribution: t\n")
	cat("Df          :", df, "\n")
	cat("Mean        :", 0, "\n")
	cat("St. dev     :", {if (n > 2) round(n/(n-2),3) else "NA"}, "\n")

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound :", if (is.na(lb)) "-Inf" else lb, "\n")
		cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X < ", lb,") = ", p_lb, "\n"))
				cat(paste0("P(X > ", lb,") = ", round(1 - p_lb, dec), "\n"))
			}

			if (!is.na(ub)) {
				cat(paste0("P(X < ", ub,") = ", p_ub, "\n"))
				cat(paste0("P(X > ", ub,") = ", round(1 - p_ub, dec), "\n"))
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " < X < ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " < X < ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {
		pub <- if (is.na(pub)) 2 else pub
		plb <- if (is.na(plb)) -1 else plb

		cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
		cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

		if (pub <= 1 || plb >= 0) {
		  cat("\n")

			if (plb >= 0) {
				cat(paste0("P(X < ", v_lb,") = ", plb, "\n"))
				cat(paste0("P(X > ", v_lb,") = ", round(1 - plb, dec), "\n"))
			}

			if (pub <= 1) {
				cat(paste0("P(X < ", v_ub,") = ", pub, "\n"))
				cat(paste0("P(X > ", v_ub,") = ", round(1 - pub, dec), "\n"))
			}

		  if (pub <= 1 && plb >= 0) {
				cat(paste0("P(", v_lb, " < X < ", v_ub,")     = ", pub - plb, "\n"))
				cat(paste0("1 - P(", v_lb, " < X < ", v_ub,") = ", round(1 - (pub - plb), dec), "\n"))
			}
		}
	}
}

#' Probability calculator for the F-distribution
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param df1 Degrees of freedom
#' @param df2 Degrees of freedom
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_fdist <- function(df1, df2,
                       lb = NA,
                       ub = NA,
                       plb = NA,
                       pub = NA,
                       dec = 3) {


	# df1 <- 2
	# df2 <- 2998
	# ub <- 2.365

	# lm(price ~ depth, data = diamonds) %>% summary

	p_ub <- pf(ub, df1, df2)
	p_lb <- pf(lb, df1, df2)
	p_int <- max(p_ub - p_lb, 0)

	p_ub %<>% round(dec)
	p_lb %<>% round(dec)
	p_int %<>% round(dec)

	if (!is.na(pub)) {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	}

	if (!is.na(plb)) {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	}

	v_ub <- qf(pub, df1, df2)
	v_lb <- qf(plb, df1, df2)

	v_ub %<>% round(dec)
	v_lb %<>% round(dec)

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(plb) && !is.na(pub)) {
		if (plb > pub) {
			plb <- pub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  environment() %>% as.list %>% set_class(c("prob_fdist",class(.)))
}

#' Plot method for the probability calculator (F-distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_fdist}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.prob_fdist <- function(x, type = "values", shiny = FALSE, ...) {


	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)
	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$v_lb
		ub <- object$v_ub
	}

	df1 <- object$df1
	df2 <- object$df2

	limits <- c(qf(0.01, df1 = df1, df2 = df2) %>% floor,
	            qf(1 - 0.01, df1 = df1, df2 = df2) %>% ceiling)

  dat <- data.frame(
    x = limits,
    Probability = df(limits, df1 = df1, df2 = df2),
    df1 = df1,
    df2 = df2
  )

  df_line <- function(x) df(x, df1 = df1, df2 = df2)

  df_limit <- function(x) {
    y <- df(x, df1 = df1, df2 = df2)
    # y[x < lb | x > ub] <- NA
    y[x < lb | x > ub] <- 0
    y
  }

  df_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- df(x, df1 = df1, df2 = df2)
    # y[x > lb] <- NA
    y[x > lb] <- 0
    y
  }

  df_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- df(x, df1 = df1, df2 = df2)
    # y[x < ub] <- NA
    y[x < ub] <- 0
    y
  }

  vlines <- c(ub,lb) %>% na.omit
  if (length(vlines) == 0) vlines <- c(-Inf, Inf)

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	plt <- ggplot(dat, aes_string(x="x"))  +
	  stat_function(fun=df_line, geom="line") +
	  stat_function(fun=df_limit, geom="area", fill="blue", alpha=0.2, n = 501) +
	  stat_function(fun=df_lb, geom="area", fill="red", alpha=0.2, n = 501) +
	  stat_function(fun=df_ub, geom="area", fill="red", alpha=0.2, n = 501) +
	  geom_vline(xintercept = vlines, color = 'black', linetype = 'dashed', size = .5) +
	  xlab("") + ylab("")

   if (shiny) plt else print(plt)
}


#' Summary method for the probability calculator function (F-distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_fdist}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.prob_fdist <- function(object, type = "values",  ...) {

	df1 <- object$df1
	df2 <- object$df2
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb

	v_ub <- object$v_ub
	v_lb <- object$v_lb

  cat("Probability calculator\n")
  cat("Distribution: F\n")
	cat("Df 1        :", df1, "\n")
	cat("Df 2        :", df2, "\n")
	# cat("Mean        :", n, "\n")
	# cat("Variance    :", 2*n, "\n")

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound :", if (is.na(lb)) "0" else lb, "\n")
		cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X < ", lb,") = ", p_lb, "\n"))
				cat(paste0("P(X > ", lb,") = ", round(1 - p_lb, dec), "\n"))
			}

			if (!is.na(ub)) {
				cat(paste0("P(X < ", ub,") = ", p_ub, "\n"))
				cat(paste0("P(X > ", ub,") = ", round(1 - p_ub, dec), "\n"))
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " < X < ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " < X < ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {
		pub <- if (is.na(pub)) 2 else pub
		plb <- if (is.na(plb)) -1 else plb

		cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
		cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

		if (pub <= 1 || plb >= 0) {
		  cat("\n")

			if (plb >= 0) {
				cat(paste0("P(X < ", v_lb,") = ", plb, "\n"))
				cat(paste0("P(X > ", v_lb,") = ", round(1 - plb, dec), "\n"))
			}

			if (pub <= 1) {
				cat(paste0("P(X < ", v_ub,") = ", pub, "\n"))
				cat(paste0("P(X > ", v_ub,") = ", round(1 - pub, dec), "\n"))
			}

		  if (pub <= 1 && plb >= 0) {
				cat(paste0("P(", v_lb, " < X < ", v_ub,")     = ", pub - plb, "\n"))
				cat(paste0("1 - P(", v_lb, " < X < ", v_ub,") = ", round(1 - (pub - plb), dec), "\n"))
			}
		}
	}
}

#' Probability calculator for the chi-squared distribution
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param df Degrees of freedom
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_chisq <- function(df,
                       lb = NA,
                       ub = NA,
                       plb = NA,
                       pub = NA,
                       dec = 3) {

	p_ub <- pchisq(ub, df)
	p_lb <- pchisq(lb, df)
	p_int <- max(p_ub - p_lb, 0)

	p_ub %<>% round(dec)
	p_lb %<>% round(dec)
	p_int %<>% round(dec)

	if (!is.na(pub)) {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	}

	if (!is.na(plb)) {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	}

	v_ub <- qchisq(pub, df)
	v_lb <- qchisq(plb, df)

	v_ub %<>% round(dec)
	v_lb %<>% round(dec)

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(plb) && !is.na(pub)) {
		if (plb > pub) {
			plb <- pub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  environment() %>% as.list %>% set_class(c("prob_chisq",class(.)))
}

#' Plot method for the probability calculator (Chi-squared distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_chisq}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.prob_chisq <- function(x, type = "values", shiny = FALSE, ...) {

	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)
	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$v_lb
		ub <- object$v_ub
	}

	df <- object$df

	limits <- c(qchisq(0.001, df = df) %>% floor,
	            qchisq(1 - 0.001, df = df) %>% ceiling)

  dat <- data.frame(
    x = limits,
    Probability = dchisq(limits, df = df),
    df = df
  )

  dchisq_limit <- function(x) {
    y <- dchisq(x, df = df)
    y[x < lb | x > ub] <- 0
    y
  }

  dchisq_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dchisq(x, df = df)
    y[x > lb] <- 0
    y
  }

  dchisq_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dchisq(x, df = df)
    y[x < ub] <- 0
    y
  }

  vlines <- c(ub,lb) %>% na.omit
  if (length(vlines) == 0) vlines <- c(-Inf, Inf)

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	# plt <- ggplot(data.frame(x=limits), aes_string(x="x")) +
	plt <- ggplot(dat, aes_string(x="x")) +
	  stat_function(fun=dchisq, args = list(df = df)) +
	  stat_function(fun=dchisq_limit, geom="area", fill="blue", alpha=0.2, n = 501) +
	  stat_function(fun=dchisq_lb, geom="area", fill="red", alpha=0.2, n = 501) +
	  stat_function(fun=dchisq_ub, geom="area", fill="red", alpha=0.2, n = 501) +
	  geom_vline(xintercept = vlines, color = 'black', linetype = 'dashed', size = .5) +
	  xlab("") + ylab("")

   if (shiny) plt else print(plt)
}


#' Summary method for the probability calculator function (Chi-squared distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_chisq}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.prob_chisq <- function(object, type = "values",  ...) {

	df <- object$df
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb

	v_ub <- object$v_ub
	v_lb <- object$v_lb

  cat("Probability calculator\n")
  cat("Distribution: Chi-squared\n")
	cat("Df          :", df, "\n")
	cat("Mean        :", df, "\n")
	cat("Variance    :", 2*df, "\n")

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound :", if (is.na(lb)) "0" else lb, "\n")
		cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X < ", lb,") = ", p_lb, "\n"))
				cat(paste0("P(X > ", lb,") = ", round(1 - p_lb, dec), "\n"))
			}

			if (!is.na(ub)) {
				cat(paste0("P(X < ", ub,") = ", p_ub, "\n"))
				cat(paste0("P(X > ", ub,") = ", round(1 - p_ub, dec), "\n"))
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " < X < ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " < X < ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {
		pub <- if (is.na(pub)) 2 else pub
		plb <- if (is.na(plb)) -1 else plb

		cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
		cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

		if (pub <= 1 || plb >= 0) {
		  cat("\n")

			if (plb >= 0) {
				cat(paste0("P(X < ", v_lb,") = ", plb, "\n"))
				cat(paste0("P(X > ", v_lb,") = ", round(1 - plb, dec), "\n"))
			}

			if (pub <= 1) {
				cat(paste0("P(X < ", v_ub,") = ", pub, "\n"))
				cat(paste0("P(X > ", v_ub,") = ", round(1 - pub, dec), "\n"))
			}

		  if (pub <= 1 && plb >= 0) {
				cat(paste0("P(", v_lb, " < X < ", v_ub,")     = ", pub - plb, "\n"))
				cat(paste0("1 - P(", v_lb, " < X < ", v_ub,") = ", round(1 - (pub - plb), dec), "\n"))
			}
		}
	}
}

#' Probability calculator for the uniform distribution
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param min Minmum value
#' @param max Maximum value
#' @param lb Lower bound
#' @param ub Upper bound
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_unif <- function(min,
                      max,
                      lb = NA,
                      ub = NA,
                      plb = NA,
                      pub = NA,
                      dec = 3) {

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(plb) && !is.na(pub)) {
		if (plb > pub) {
			plb <- pub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

	p_ub <- punif(ub, min, max)
	p_lb <- punif(lb, min, max)
	p_int <- max(p_ub - p_lb, 0) %>% round(dec)

	p_ub %<>% round(dec)
	p_lb %<>% round(dec)

	if (!is.na(pub)) {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	}

	if (!is.na(plb)) {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	}

	v_ub <- qunif(pub, min, max) %>% round(dec)
	v_lb <- qunif(plb, min, max) %>% round(dec)

	mean <- (max+min) / 2
	stdev <- sqrt((max-min)^2 / 12)

  environment() %>% as.list %>% set_class(c("prob_unif",class(.)))
}

#' Plot method for the probability calculator (uniform)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_unif}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.prob_unif <- function(x, type = "values", shiny = FALSE, ...) {

	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)
	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$v_lb
		ub <- object$v_ub
	}

	min <- object$min
	max <- object$max

	if (min > max) return(invisible())

  limits <- c(min, max)
  dunif_limit <- function(x) {
    y <- dunif(x, min = min, max = max)
    # y[x < lb | x > ub] <- NA
    y[x < lb | x > ub] <- 0
    y
  }

  dunif_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dunif(x, min = min, max = max)
    # y[x > lb] <- NA
    y[x > lb] <- 0
    y
  }

  dunif_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dunif(x, min = min, max = max)
    # y[x < ub] <- NA
    y[x < ub] <- 0
    y
  }

  dunif_lines <- c(ub,lb) %>% na.omit %>% setdiff(c(min,max))
  if (length(dunif_lines) == 0) dunif_lines <- c(-Inf, Inf)

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	# plt <- ggplot(data.frame(x=limits), aes_string(x="x")) +
	plt <- ggplot(data.frame(x=limits, y = dunif(limits, limits[1], limits[2]), lb = lb, ub = ub), aes_string(x="x")) +
	  # stat_function(fun=dunif, args = list(min = min, max = max)) +
	  stat_function(fun=dunif_limit, geom="area", fill="blue", alpha=0.2, n = 501) +
	  stat_function(fun=dunif_lb, geom="area", fill="red", alpha=0.2, n = 501) +
	  stat_function(fun=dunif_ub, geom="area", fill="red", alpha=0.2, n = 501) +
	  geom_vline(xintercept = dunif_lines, color = 'black', linetype = 'dashed', size = .5) +
	  # geom_vline(xintercept = c(min,max), color = 'black', linetype = 'solid', size = .5) +
	  # geom_segment(aes(x = x[1], y = 0, xend = x[1], yend = dunif(x[1], x[1], x[2]))) +
	  geom_segment(aes(x = x[1], y = 0, xend = x[1], yend = y[1])) +
	  # geom_segment(aes(x = x[2], y = 0, xend = x[2], yend = dunif(x[1], x[1], x[2]))) +
	  geom_segment(aes(x = x[2], y = 0, xend = x[2], yend = y[2])) +
	  # geom_segment(aes(x = x[1], y = dunif(x[1], x[1], x[2]), xend = x[2], yend = dunif(x[1], x[1], x[2]))) +
	  geom_segment(aes(x = x[1], y = y[1], xend = x[2], yend = y[2])) +
	  # geom_segment(aes(x = x[1], y = 0, xend = x[1] - abs(.05*(x[2]-x[1])), yend = 0)) +
	  # geom_segment(aes(x = x[2], y = 0, xend = x[2] + abs(.05*(x[2]-x[1])), yend = 0)) +
	  # geom_rect(aes(ymin = 0, ymax = y[1], xmin = lb, xmax = ub), fill = "blue", alpha = 0.2) +
	  xlab("") + ylab("")

   if (shiny) plt else print(plt)
}

#' Summary method for the probability calculator function (uniform)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_unif}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.prob_unif <- function(object, type = "values",  ...) {

	# suppressMessages(attach(object))

	min <- object$min
	max <- object$max
	mean <- object$mean
	stdev <- object$stdev
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb

	v_ub <- object$v_ub
	v_lb <- object$v_lb

  cat("Probability calculator\n")
  cat("Distribution: Uniform\n")
	cat("Min         :", min, "\n")
	cat("Max         :", max, "\n")
	cat("Mean        :", mean %>% round(dec), "\n")
	cat("St. dev     :", stdev %>% round(dec), "\n")

	# if (is.null(min) || is.na(min) || is.null(max) || is.na(max)) {
	# 	cat("\nPlease specify both a minimum and maximum value\n")
	# 	return(invisible())
	# }

	# if (min > max) {
	# 	cat("\nThe maximum value must be larger than the minimum value\n")
	# 	return(invisible())
	# }

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound :", {if (is.na(lb)) min else lb}, "\n")
		cat("Upper bound :", {if (is.na(ub)) max else ub}, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X < ", lb,") = ", p_lb, "\n"))
				cat(paste0("P(X > ", lb,") = ", round(1 - p_lb, dec), "\n"))
			}

			if (!is.na(ub)) {
				cat(paste0("P(X < ", ub,") = ", p_ub, "\n"))
				cat(paste0("P(X > ", ub,") = ", round(1 - p_ub, dec), "\n"))
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " < X < ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " < X < ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {
		pub <- if (is.na(pub)) 2 else pub
		plb <- if (is.na(plb)) -1 else plb

		cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
		cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

		if (pub <= 1 || plb >= 0) {
		  cat("\n")

			if (plb >= 0) {
				cat(paste0("P(X < ", v_lb,") = ", plb, "\n"))
				cat(paste0("P(X > ", v_lb,") = ", round(1 - plb, dec), "\n"))
			}

			if (pub <= 1) {
				cat(paste0("P(X < ", v_ub,") = ", pub, "\n"))
				cat(paste0("P(X > ", v_ub,") = ", round(1 - pub, dec), "\n"))
			}

		  if (pub <= 1 && plb >= 0) {
				cat(paste0("P(", v_lb, " < X < ", v_ub,")     = ", pub - plb, "\n"))
				cat(paste0("1 - P(", v_lb, " < X < ", v_ub,") = ", round(1 - (pub - plb), dec), "\n"))
			}
		}
	}
}

#' Probability calculator for the binomial distribution (binomial)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param n Number of trials
#' @param p Probability
#' @param lb Lower bound on the number of successes
#' @param ub Upper bound on the number of successes
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_binom <- function(n,
                       p,
                       lb = NA,
                       ub = NA,
                       plb = NA,
                       pub = NA,
                       dec = 3) {

	## making sure n is integer
	n <- as_integer(n)

	if (is.na(lb) || lb < 0) {
		p_elb <- p_lb <- lb <- NA
	} else {
		if (lb > n) lb <- n
		p_elb <- dbinom(lb, n, p) %>% round(dec)
		p_lelb <- pbinom(lb, n, p) %>% round(dec)
		if (lb > 0)
	    p_lb <- sum(dbinom(0:max((lb-1),0), n, p)) %>% round(dec)
	  else
	  	p_lb <- 0
	}

	if (is.na(ub) || ub < 0) {
		p_eub <- p_ub <- ub <- NA
	} else {
		if (ub > n) ub <- n
		p_eub <- dbinom(ub, n, p) %>% round(dec)
		p_leub <- pbinom(ub, n, p) %>% round(dec)
		if (ub > 0)
		  p_ub <- sum(dbinom(0:max((ub-1),0), n, p)) %>% round(dec)
		else
			p_ub <- 0
	}

	if (!is.na(ub) && !is.na(lb)) {
	  p_int <- sum(dbinom(lb:ub, n, p)) %>% max(0) %>% round(dec)
	} else {
		p_int <- NA
	}

	# if (is.na(plb) || plb < 0) {
	if (is.na(plb)) {
		vlb <- NA
	} else {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	  vlb <- qbinom(plb, n, p)

		vp_elb <- dbinom(vlb, n, p) %>% round(dec)
		vp_lelb <- pbinom(vlb, n, p) %>% round(dec)
		if (vlb > 0)
	    vp_lb <- sum(dbinom(0:max((vlb-1),0), n, p)) %>% round(dec)
	  else
	  	vp_lb <- 0
	}

	# if (is.na(pub) || pub < 0) {
	if (is.na(pub)) {
		vub <- NA
	} else {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	  vub <- qbinom(pub, n, p)

		vp_eub <- dbinom(vub, n, p) %>% round(dec)
		vp_leub <- pbinom(vub, n, p) %>% round(dec)
		if (vub > 0)
		  vp_ub <- sum(dbinom(0:max((vub-1),0), n, p)) %>% round(dec)
		else
			vp_ub <- 0
	}

	if (!is.na(pub) && !is.na(plb)) {
	  vp_int <- sum(dbinom(vlb:vub, n, p)) %>% max(0) %>% round(dec)
	} else {
		vp_int <- NA
	}

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(vlb) && !is.na(vub)) {
		if (vlb > vub) {
			plb <- pub <- vlb <- vub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }


  environment() %>% as.list %>% set_class(c("prob_binom",class(.)))
}

#' Plot method for the probability calculator function (binomial)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_binom}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.prob_binom <- function(x, type = "values", shiny = FALSE, ...) {

	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)
	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$vlb
		ub <- object$vub
	}

	n <- object$n
	p <- object$p

  limits <- 0:n

  k <- factor(rep("below",n+1), levels = c("below","equal","above"))
  if (!is.null(ub) && !is.na(ub)) {
  	k[ub+1] <- "equal"
    if (!is.na(lb)) k[(lb:ub)+1] <- "equal"
  	k[0:n > ub] <- "above"
  } else if (!is.null(lb) && !is.na(lb)) {
  	k[lb+1] <- "equal"
  	k[0:n > lb] <- "above"
  } else {
  	return(invisible())
  }

  dat <- data.frame(
    x = limits %>% as_factor,
    Probability = dbinom(limits, size = n, prob = p),
    k = k
  ) %>% filter(., .$Probability > 0.00001)

  if (nrow(dat) < 40) {
  	breaks <- dat$x
  } else {
  	x <- as_integer(dat$x)
  	breaks <- seq(min(x), max(x), length.out = 20) %>% round(0)
  }

  cols <- c(below = "red", equal = "blue", above = "black")

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	plt <- ggplot(dat, aes_string(x = "x", y = "Probability", fill = "k")) +
	  geom_bar(stat="identity", alpha = .3) +
	  xlab("") + scale_fill_manual(values = cols) +
	  theme(legend.position="none") +
	  scale_x_discrete(breaks = breaks)

   if (shiny) plt else print(plt)
}

#' Summary method for the probability calculator function
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_binom}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.prob_binom <- function(object, type = "values",  ...) {

	# suppressMessages(attach(object))
	n <- object$n
	p <- object$p
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_eub <- object$p_eub
	p_elb <- object$p_elb
	p_leub <- object$p_leub
	p_lelb <- object$p_lelb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb
	vub <- object$vub
	vlb <- object$vlb

	vp_ub <- object$vp_ub
	vp_lb <- object$vp_lb
	vp_eub <- object$vp_eub
	vp_elb <- object$vp_elb
	vp_leub <- object$vp_leub
	vp_lelb <- object$vp_lelb
	vp_int <- object$vp_int

  cat("Probability calculator\n")
  cat("Distribution: Binomial\n")
	cat("n           :", n, "\n")
	cat("p           :", p, "\n")
	cat("Mean        :", n*p, "\n")
	cat("St. dev     :", sqrt(n*p*(1-p)) %>% round(dec), "\n")

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound :", {if (is.na(lb)) "" else lb}, "\n")
		cat("Upper bound :", {if (is.na(ub)) "" else ub}, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X  = ", lb,") = ", p_elb, "\n"))
				if (lb > 0) {
					cat(paste0("P(X  < ", lb,") = ", p_lb, "\n"))
				  cat(paste0("P(X <= ", lb,") = ", p_lelb, "\n"))
				}
				if (lb < n) {
				  cat(paste0("P(X  > ", lb,") = ", round(1 - (p_lb + p_elb), dec), "\n"))
			  	cat(paste0("P(X >= ", lb,") = ", round(1 - p_lb, dec), "\n"))
				}
			}

			if (!is.na(ub)) {
				cat(paste0("P(X  = ", ub,") = ", p_eub, "\n"))
				if (ub > 0) {
					cat(paste0("P(X  < ", ub,") = ", p_ub, "\n"))
				  cat(paste0("P(X <= ", ub,") = ", p_leub, "\n"))
				}
				if (ub < n) {
				  cat(paste0("P(X  > ", ub,") = ", round(1 - (p_ub + p_eub), dec), "\n"))
				  cat(paste0("P(X >= ", ub,") = ", round(1 - p_ub, dec), "\n"))
				}
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " <= X <= ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " <= X <= ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {

		cat("Lower bound :", if (is.na(plb)) "\n" else paste0(plb, " (", vlb, ")\n"))
		cat("Upper bound :", if (is.na(pub)) "\n" else paste0(pub, " (", vub, ")\n"))

		if (!is.na(pub) || !is.na(plb)) {
		  cat("\n")

			if (!is.na(plb)) {
				cat(paste0("P(X  = ", vlb,") = ", vp_elb, "\n"))
				if (vlb > 0) {
					cat(paste0("P(X  < ", vlb,") = ", vp_lb, "\n"))
				  cat(paste0("P(X <= ", vlb,") = ", vp_lelb, "\n"))
				}
				if (vlb < n) {
				  cat(paste0("P(X  > ", vlb,") = ", round(1 - (vp_lb + vp_elb), dec), "\n"))
			  	cat(paste0("P(X >= ", vlb,") = ", round(1 - vp_lb, dec), "\n"))
				}
			}

			if (!is.na(pub)) {
				cat(paste0("P(X  = ", vub,") = ", vp_eub, "\n"))
				if (vub > 0) {
					cat(paste0("P(X  < ", vub,") = ", vp_ub, "\n"))
				  cat(paste0("P(X <= ", vub,") = ", vp_leub, "\n"))
				}
				if (vub < n) {
				  cat(paste0("P(X  > ", vub,") = ", round(1 - (vp_ub + vp_eub), dec), "\n"))
		      cat(paste0("P(X >= ", vub,") = ", round(1 - vp_ub, dec), "\n"))
				}
			}

			if (!is.na(plb) && !is.na(pub)) {
				cat(paste0("P(", vlb, " <= X <= ", vub,")     = ", vp_int, "\n"))
				cat(paste0("1 - P(", vlb, " <= X <= ", vub,") = ", round(1 - vp_int, dec), "\n"))
		  }
		}
	}
}

#' Probability calculator for the discrete distribution (discrete)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param v Values
#' @param p Probabilities
#' @param lb Lower bound on the number of successes
#' @param ub Upper bound on the number of successes
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show

#'
#' @export
prob_disc <- function(v, p,
                      lb = NA,
                      ub = NA,
                      plb = NA,
                      pub = NA,
                      dec = 3) {

	# Think about adding an "expand.grid" setup so you can run this n times. e.g., rolling multiple dice
	# expand.grid(height = 1:6, weight = 1:6)
	# expand.grid(1, 1:6)
	# Think about adding an "expand.grid" setup so you can run this n times. e.g., rolling multiple dice

	# library(radiant)
	# v <- "1 2 3 4 5"
	# p <- ".2 .3 .3 .15 .05"
	# p <- "1/6    \n1/12 1/6 1/4"
	# p <- "1/6 1/12 1/6 abbb"

	v <- unlist(strsplit(v, "\\s+")) %>% as_numeric
	p <- unlist(strsplit(p, "\\s+"))

	cp <- c()
	for (i in p) {
		res <- try(eval(parse(text = i)), silent = TRUE)
	  if (is(res, 'try-error')) {
	    mess_probs <- mess_values <- paste0("Invalid inputs:\n\n", attr(res,"condition")$message)
      return(environment() %>% as.list %>% set_class(c("prob_disc",class(.))))
	  }
	  cp <- c(cp, res)
	}
	p <- cp %>% set_names(NULL)

	if (any(is(p, 'try-error'))) {
	  mess_probs <- mess_values <- paste0("Invalid inputs:\n\n", attr(res,"condition")$message)
    return(environment() %>% as.list %>% set_class(c("prob_disc",class(.))))
	}

	if(length(v) %% length(p) > 0) {
		mess_probs <- mess_values <- "The number of values entered must be a multiple of the number of probabilities"
    return(environment() %>% as.list %>% set_class(c("prob_disc",class(.))))
	}

	## make sure values and probabilities are ordered correctly
  df <- data.frame(v = v, p = p) %>% arrange(v)
  p <- df$p
  v <- df$v

	if (sum(p) < .99 || sum(p) > 1.01) {
		mess_probs <- mess_values <- paste0("Probabilities for a discrete variable do not sum to 1 (",round(sum(p),3),")")
    return(environment() %>% as.list %>% set_class(c("prob_disc",class(.))))
	}

	ddisc <- function(b, df) filter(df, v == b)$p
	pdisc <- function(b, df) filter(df, v < b)$p  %>% sum
	# qdisc <- function(prob, df) mutate(df, p = cumsum(df$p)) %>% filter(p <= prob) %>% tail(1) %>% .$v
	## consistent with http://www.stat.umn.edu/geyer/old/5101/rlook.html#qbinom
	qdisc <- function(prob, df) mutate(df, p = cumsum(df$p)) %>% filter(p >= prob) %>% .$v %>% min

	if (is.na(lb)) {
		p_elb <- p_lb <- lb <- NA
	} else if (!lb %in% v) {
		p_elb <- 0
		p_lb <- ifelse ( lb < min(v), 0, pdisc(lb, df) %>% round(dec))
		p_lelb <- p_elb + p_lb
	} else {
		p_elb <- ddisc(lb, df) %>% round(dec)
		p_lb <- pdisc(lb, df) %>% round(dec)
		p_lelb <- p_elb + p_lb
	}

	if (is.na(ub)) {
		p_eub <- p_ub <- ub <- NA
	} else if (!ub %in% v) {
	  p_eub <- 0
		p_ub <- ifelse ( ub < min(v), 0, pdisc(ub, df) %>% round(dec))
		p_leub <- p_eub + p_ub
	} else {
		p_eub <- ddisc(ub, df) %>% round(dec)
		p_ub <- pdisc(ub, df) %>% round(dec)
		p_leub <- p_eub + p_ub
	}

	if (!is.na(ub) && !is.na(lb)) {
	  p_int <- p_leub - p_lb
	} else {
		p_int <- NA
	}

	if (is.na(plb)) {
		plb <- vlb <- NA
	} else if (length(qdisc(plb, df)) == 0) {
		mess_probs <- "Lower bound is too low"
    return(environment() %>% as.list %>% set_class(c("prob_disc",class(.))))
	} else {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	  vlb <- qdisc(plb, df)
		vp_elb <- ddisc(vlb, df) %>% round(dec)
	  vp_lb <- pdisc(vlb, df) %>% round(dec)
		vp_lelb <-  vp_elb + vp_lb
	}

	if (is.na(pub)) {
		pub <- vub <- NA
	} else if (length(qdisc(pub, df)) == 0) {
		mess_probs <- "Upper bound is too low"
    return(environment() %>% as.list %>% set_class(c("prob_disc",class(.))))
	} else {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	  vub <- qdisc(pub, df)
		vp_eub <- ddisc(vub, df) %>% round(dec)
	  vp_ub <- pdisc(vub, df) %>% round(dec)
		vp_leub <-  vp_eub + vp_ub
	}

	if (!is.na(pub) && !is.na(plb)) {
	  vp_int <- vp_leub - vp_lb
	} else {
		vp_int <- NA
	}

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(vlb) && !is.na(vub)) {
		if (vlb > vub) {
			plb <- pub <- vlb <- vub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  environment() %>% as.list %>% set_class(c("prob_disc",class(.)))
}

#' Plot method for the probability calculator function (discrete)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_disc}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- prob_disc(v = "5 6 7 8 9 10 11 ", p = ".1 .2 .3 .15 .1 .1 .05", pub = 0.95)
#' plot(result, type = "probs")
#'
#' @export
plot.prob_disc <- function(x, type = "values", shiny = FALSE, ...) {

	# object <- result

	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)
	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$vlb
		ub <- object$vub
	}
	# lb
	# ub

	v <- object$v
	p <- object$p

  limits <- v

  k <- factor(rep("below",length(v)), levels = c("below","equal","above"))
  # if (!is.null(ub) && !is.na(ub)) {
  if (!is_empty(ub)) {
    if (!is.na(lb)) {
    	k[v >= lb & v <= ub] <- "equal"
    } else if (ub %in% v) {
    	k[v == ub] <- "equal"
    }
  	k[v > ub] <- "above"
  # } else if (!is.null(lb) && !is.na(lb)) {
  } else if (!is_empty(lb)) {
  	if (lb %in% v) k[v == lb] <- "equal"
  	k[v > lb] <- "above"
  } else {
  	return(invisible())
  }

  dat <- data.frame(
    x = limits %>% as_factor,
    Probability = p,
    k = k
  )

  if (nrow(dat) < 40) {
  	breaks <- dat$x
  } else {
  	x <- as_integer(dat$x)
  	breaks <- seq(min(x), max(x), length.out = 20) %>% round(0)
  }

  cols <- c(below = "red", equal = "blue", above = "black")

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	plt <- ggplot(dat, aes_string(x = "x", y = "Probability", fill = "k")) +
	  geom_bar(stat="identity", alpha = .3) +
	  xlab("") + scale_fill_manual(values = cols) +
	  theme(legend.position="none") +
	  scale_x_discrete(breaks = breaks)

   if (shiny) plt else print(plt)
}

#' Summary method for the probability calculator function (discrete)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_disc}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- prob_disc(v = "5 6 7 8 9 10 11 ", p = ".1 .2 .3 .15 .1 .1 .05", pub = 0.95)
#' summary(result, type = "probs")
#'
#' @export
summary.prob_disc <- function(object, type = "values",  ...) {

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	# suppressMessages(attach(object))
	v <- object$v
	p <- object$p
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_eub <- object$p_eub
	p_elb <- object$p_elb
	p_leub <- object$p_leub
	p_lelb <- object$p_lelb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb
	vub <- object$vub
	vlb <- object$vlb

	vp_ub <- object$vp_ub
	vp_lb <- object$vp_lb
	vp_eub <- object$vp_eub
	vp_elb <- object$vp_elb
	vp_leub <- object$vp_leub
	vp_lelb <- object$vp_lelb
	vp_int <- object$vp_int

  cat("Probability calculator\n")
  cat("Distribution : Discrete\n")
  cat("Values       :", paste0(v, collapse=" "), "\n")
  cat("Probabilities:", paste0(p %>% round(dec), collapse=" "), "\n")
  m <- sum(v*p); std <- sum(p * (v - m)^2) %>% sqrt
	cat("Mean         :", m %>% round(dec), "\n")
	cat("St. dev      :", std %>% round(dec), "\n")

	if (type == "values") {
		cat("Lower bound  :", {if (is.na(lb)) "" else lb}, "\n")
		cat("Upper bound  :", {if (is.na(ub)) "" else ub}, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X  = ", lb,") = ", p_elb, "\n"))
				if (lb > min(v)) {
					cat(paste0("P(X  < ", lb,") = ", p_lb, "\n"))
				  cat(paste0("P(X <= ", lb,") = ", p_lelb, "\n"))
				}
				if (lb < max(v)) {
				  cat(paste0("P(X  > ", lb,") = ", round(1 - (p_lb + p_elb), dec), "\n"))
				  cat(paste0("P(X >= ", lb,") = ", round(1 - p_lb, dec), "\n"))
				}
			}

			if (!is.na(ub)) {
				cat(paste0("P(X  = ", ub,") = ", p_eub, "\n"))
				if (ub > min(v)) {
					cat(paste0("P(X  < ", ub,") = ", p_ub, "\n"))
				  cat(paste0("P(X <= ", ub,") = ", p_leub, "\n"))
				}
				if (ub < max(v)) {
				  cat(paste0("P(X  > ", ub,") = ", round(1 - (p_ub + p_eub), dec), "\n"))
				 	cat(paste0("P(X >= ", ub,") = ", round(1 - p_ub, dec), "\n"))
				}
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " <= X <= ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " <= X <= ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {

		cat("Lower bound  :", if (is.na(plb)) "\n" else paste0(plb, " (", vlb, ")\n"))
		cat("Upper bound  :", if (is.na(pub)) "\n" else paste0(pub, " (", vub, ")\n"))

		if (!is.na(pub) || !is.na(plb)) {
		  cat("\n")

			if (!is.na(plb)) {
				cat(paste0("P(X  = ", vlb,") = ", vp_elb, "\n"))
				if (vlb > min(v)) {
					cat(paste0("P(X  < ", vlb,") = ", vp_lb, "\n"))
				  cat(paste0("P(X <= ", vlb,") = ", vp_lelb, "\n"))
				}
				if (vlb < max(v)) {
				  cat(paste0("P(X  > ", vlb,") = ", round(1 - (vp_lb + vp_elb), dec), "\n"))
			  	cat(paste0("P(X >= ", vlb,") = ", round(1 - vp_lb, dec), "\n"))
				}
			}

			if (!is.na(pub)) {
				cat(paste0("P(X  = ", vub,") = ", vp_eub, "\n"))
				if (vub > min(v)) {
					cat(paste0("P(X  < ", vub,") = ", vp_ub, "\n"))
				  cat(paste0("P(X <= ", vub,") = ", vp_leub, "\n"))
				}
				if (vub < max(v)) {
				  cat(paste0("P(X  > ", vub,") = ", round(1 - (vp_ub + vp_eub), dec), "\n"))
				  cat(paste0("P(X >= ", vub,") = ", round(1 - vp_ub, dec), "\n"))
				}
			}

			if (!is.na(plb) && !is.na(pub)) {
				cat(paste0("P(", vlb, " <= X <= ", vub,")     = ", vp_int, "\n"))
				cat(paste0("1 - P(", vlb, " <= X <= ", vub,") = ", round(1 - vp_int, dec), "\n"))
		  }
		}
	}
}

#' Probability calculator for the exponential distribution
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param rate Rate
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_expo <- function(rate,
                      lb = NA,
                      ub = NA,
                      plb = NA,
                      pub = NA,
                      dec = 3) {

	p_ub <- pexp(ub, rate)
	p_lb <- pexp(lb, rate)
	p_int <- max(p_ub - p_lb, 0)

	p_ub %<>% round(dec)
	p_lb %<>% round(dec)
	p_int %<>% round(dec)

	if (!is.na(pub)) {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	}

	if (!is.na(plb)) {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	}

	v_ub <- qexp(pub, rate) %>% round(dec)
	v_lb <- qexp(plb, rate) %>% round(dec)

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(plb) && !is.na(pub)) {
		if (plb > pub) {
			plb <- pub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  environment() %>% as.list %>% set_class(c("prob_expo",class(.)))
}

#' Plot method for the probability calculator (Exponential distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_expo}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.prob_expo <- function(x, type = "values", shiny = FALSE, ...) {

	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)
	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$v_lb
		ub <- object$v_ub
	}

	rate <- object$rate

	limits <- c(qexp(0.001, rate = rate) %>% floor,
	            qexp(1 - 0.001, rate = rate) %>% ceiling)

  dat <- data.frame(
    x = limits,
    Probability = dexp(limits, rate = rate),
    rate = rate
  )

  dexp_limit <- function(x) {
    y <- dexp(x, rate = rate)
    # y[x < lb | x > ub] <- NA
    y[x < lb | x > ub] <- 0
    y
  }

  dexp_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dexp(x, rate = rate)
    # y[x > lb] <- NA
    y[x > lb] <- 0
    y
  }

  dexp_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dexp(x, rate = rate)
    # y[x < ub] <- NA
    y[x < ub] <- 0
    y
  }

  vlines <- c(ub,lb) %>% na.omit
  if (length(vlines) == 0) vlines <- c(-Inf, Inf)

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	# plt <- ggplot(data.frame(x=limits), aes_string(x="x")) +
	plt <- ggplot(dat, aes_string(x="x")) +
	  stat_function(fun=dexp, args = list(rate = rate)) +
	  stat_function(fun=dexp_limit, geom="area", fill="blue", alpha=0.2, n = 501) +
	  stat_function(fun=dexp_lb, geom="area", fill="red", alpha=0.2, n = 501) +
	  stat_function(fun=dexp_ub, geom="area", fill="red", alpha=0.2, n = 501) +
	  geom_vline(xintercept = vlines, color = 'black', linetype = 'dashed', size = .5) +
	  xlab("") + ylab("")

   if (shiny) plt else print(plt)
}


#' Summary method for the probability calculator function (Exponential distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_expo}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.prob_expo <- function(object, type = "values",  ...) {

	rate <- object$rate
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb

	v_ub <- object$v_ub
	v_lb <- object$v_lb

  cat("Probability calculator\n")
  cat("Distribution: Exponential\n")
	cat("Rate        :", rate, "\n")
	cat("Mean        :", 1/rate, "\n")
	cat("Variance    :", rate^-2, "\n")

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound :", if (is.na(lb)) "0" else lb, "\n")
		cat("Upper bound :", if (is.na(ub)) "Inf" else ub, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X < ", lb,") = ", p_lb, "\n"))
				cat(paste0("P(X > ", lb,") = ", round(1 - p_lb, dec), "\n"))
			}

			if (!is.na(ub)) {
				cat(paste0("P(X < ", ub,") = ", p_ub, "\n"))
				cat(paste0("P(X > ", ub,") = ", round(1 - p_ub, dec), "\n"))
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " < X < ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " < X < ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {
		pub <- if (is.na(pub)) 2 else pub
		plb <- if (is.na(plb)) -1 else plb

		cat("Lower bound :", if (plb < 0) "0" else plb, "\n")
		cat("Upper bound :", if (pub > 1) "1" else pub, "\n")

		if (pub <= 1 || plb >= 0) {
		  cat("\n")

			if (plb >= 0) {
				cat(paste0("P(X < ", v_lb,") = ", plb, "\n"))
				cat(paste0("P(X > ", v_lb,") = ", round(1 - plb, dec), "\n"))
			}

			if (pub <= 1) {
				cat(paste0("P(X < ", v_ub,") = ", pub, "\n"))
				cat(paste0("P(X > ", v_ub,") = ", round(1 - pub, dec), "\n"))
			}

		  if (pub <= 1 && plb >= 0) {
				cat(paste0("P(", v_lb, " < X < ", v_ub,")     = ", pub - plb, "\n"))
				cat(paste0("1 - P(", v_lb, " < X < ", v_ub,") = ", round(1 - (pub - plb), dec), "\n"))
			}
		}
	}
}


#' Probability calculator for the poisson distribution
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param lambda Rate
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_pois <- function(lambda,
                      lb = NA,
                      ub = NA,
                      plb = NA,
                      pub = NA,
                      dec = 3) {

	if (!is.integer(lambda))
	  mess_values <- "\nLambda must be a postivie integer value"

	# lambda <- as_integer(lambda)

	# lb <- 2
	# dec <- 3
	# lambda <- 3
	# library(magrittr)

	if (is.na(lb) || lb < 0) {
		p_elb <- p_lb <- lb <- NA
	} else {
		# if (lb > n) lb <- n
		p_elb <- dpois(lb, lambda) %>% round(dec)
		p_lelb <- ppois(lb, lambda) %>% round(dec)
		if (lb > 0)
	    p_lb <- (ppois(lb, lambda) - dpois(lb, lambda)) %>% round(dec)
	  else
	  	p_lb <- 0
	}

	if (is.na(ub) || ub < 0) {
		p_eub <- p_ub <- ub <- NA
	} else {
		# if (ub > n) ub <- n
		p_eub <- dpois(ub, lambda) %>% round(dec)
		p_leub <- ppois(ub, lambda) %>% round(dec)
		if (ub > 0)
	    p_ub <- (ppois(ub, lambda) - dpois(ub, lambda)) %>% round(dec)
		else
			p_ub <- 0
	}

	if (!is.na(ub) && !is.na(lb)) {
	  p_int <- sum(dpois(lb:ub, lambda)) %>% max(0) %>% round(dec)
	} else {
		p_int <- NA
	}

	# if (is.na(plb) || plb < 0) {
	if (is.na(plb)) {
		vlb <- NA
	} else {
		if (plb > 1) plb <- 1
		if (plb < 0) plb <- 0
	  vlb <- qpois(plb, lambda)

		vp_elb <- dpois(vlb, lambda) %>% round(dec)
		vp_lelb <- ppois(vlb, lambda) %>% round(dec)
		if (vlb > 0)
	    vp_lb <- (ppois(vlb, lambda) - dpois(vlb, lambda)) %>% round(dec)
	  else
	  	vp_lb <- 0
	}

	if (is.na(pub)) {
		vub <- NA
	} else {
		if (pub > 1) pub <- 1
		if (pub < 0) pub <- 0
	  vub <- qpois(pub, lambda)

		vp_eub <- dpois(vub, lambda) %>% round(dec)
		vp_leub <- ppois(vub, lambda) %>% round(dec)
		if (vub > 0)
	    vp_ub <- (ppois(vub, lambda) - dpois(vub, lambda)) %>% round(dec)
		else
			vp_ub <- 0
	}

	if (!is.na(pub) && !is.na(plb)) {
	  vp_int <- sum(dpois(vlb:vub, lambda)) %>% max(0) %>% round(dec)
	} else {
		vp_int <- NA
	}

	if (!is.na(lb) && !is.na(ub)) {
		if (lb > ub) {
			lb <- ub <- NA
			mess_values <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  if (!is.na(vlb) && !is.na(vub)) {
		if (vlb > vub) {
			plb <- pub <- vlb <- vub <- NA
			mess_probs <- "\nPlease ensure the lower bound is smaller than the upper bound"
		}
  }

  environment() %>% as.list %>% set_class(c("prob_pois",class(.)))
}


#' Plot method for the probability calculator function (Poisson distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{prob_pois}}
#' @param type Probabilities or values
#' @param shiny Did the function call originate inside a shiny app
#' @param ... further arguments passed to or from other methods
#'
#' @export
plot.prob_pois <- function(x, type = "values", shiny = FALSE, ...) {

	mess <- paste0("mess_",type)
	if (!is.null(x[[mess]])) return(invisible())

	object <- x; rm(x)
	if (type == "values") {
		lb <- object$lb
		ub <- object$ub
	} else {
		lb <- object$vlb
		ub <- object$vub
	}

	lambda <- object$lambda
	limits <- 0:(qpois(1 - 0.00001, lambda) %>% ceiling)
	n <- max(limits)

	if (!is.na(lb) && lb > n) {
		limits <- 0:lb
		n <- lb
	}

	if (!is.na(ub) && ub > n) {
		limits <- 0:ub
		n <- ub
	}

  k <- factor(rep("below",n+1), levels = c("below","equal","above"))
  if (!is.null(ub) && !is.na(ub)) {
  	k[ub+1] <- "equal"
    if (!is.na(lb)) k[(lb:ub)+1] <- "equal"
  	k[0:n > ub] <- "above"
  } else if (!is.null(lb) && !is.na(lb)) {
  	k[lb+1] <- "equal"
  	k[0:n > lb] <- "above"
  } else {
  	return(invisible())
  }

  dat <- data.frame(
    x = limits %>% as_factor,
    Probability = dpois(limits, lambda),
    k = k
  ) %>% filter(., .$Probability > 0.00001)

  if (nrow(dat) < 40) {
  	breaks <- dat$x
  } else {
  	x <- as_integer(dat$x)
  	breaks <- seq(min(x), max(x), length.out = 20) %>% round(0)
  }

  cols <- c(below = "red", equal = "blue", above = "black")

	## based on http://rstudio-pubs-static.s3.amazonaws.com/58753_13e35d9c089d4f55b176057235778679.html
	## and R Graphics Cookbook
	plt <- ggplot(dat, aes_string(x = "x", y = "Probability", fill = "k")) +
	  geom_bar(stat="identity", alpha = .3) +
	  xlab("") + scale_fill_manual(values = cols) +
	  theme(legend.position="none") +
	  scale_x_discrete(breaks = breaks)

   if (shiny) plt else print(plt)
}

#' Summary method for the probability calculator function (Poisson distribution)
#'
#' @details See \url{http://vnijs.github.io/radiant/quant/prob_calc.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{prob_pois}}
#' @param type Probabilities or values
#' @param ... further arguments passed to or from other methods
#'
#' @export
summary.prob_pois <- function(object, type = "values",  ...) {

	# suppressMessages(attach(object))
	lambda <- object$lambda
	dec <- object$dec

	ub <- object$ub
	lb <- object$lb
	p_ub <- object$p_ub
	p_lb <- object$p_lb
	p_eub <- object$p_eub
	p_elb <- object$p_elb
	p_leub <- object$p_leub
	p_lelb <- object$p_lelb
	p_int <- object$p_int

	pub <- object$pub
	plb <- object$plb
	vub <- object$vub
	vlb <- object$vlb

	vp_ub <- object$vp_ub
	vp_lb <- object$vp_lb
	vp_eub <- object$vp_eub
	vp_elb <- object$vp_elb
	vp_leub <- object$vp_leub
	vp_lelb <- object$vp_lelb
	vp_int <- object$vp_int

  cat("Probability calculator\n")
  cat("Distribution: Poisson\n")
	cat("Lambda      :", lambda, "\n")
	cat("Mean        :", lambda, "\n")
	cat("Variance    :", lambda, "\n")

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound :", {if (is.na(lb)) "" else lb}, "\n")
		cat("Upper bound :", {if (is.na(ub)) "" else ub}, "\n")

		if (!is.na(ub) || !is.na(lb)) {
		  cat("\n")

			if (!is.na(lb)) {
				cat(paste0("P(X  = ", lb,") = ", p_elb, "\n"))
				if (lb > 0) {
					cat(paste0("P(X  < ", lb,") = ", p_lb, "\n"))
				  cat(paste0("P(X <= ", lb,") = ", p_lelb, "\n"))
				}
				# if (lb < n) {
				  cat(paste0("P(X  > ", lb,") = ", round(1 - (p_lb + p_elb), dec), "\n"))
			  	cat(paste0("P(X >= ", lb,") = ", round(1 - p_lb, dec), "\n"))
				# }
			}

			if (!is.na(ub)) {
				cat(paste0("P(X  = ", ub,") = ", p_eub, "\n"))
				if (ub > 0) {
					cat(paste0("P(X  < ", ub,") = ", p_ub, "\n"))
				  cat(paste0("P(X <= ", ub,") = ", p_leub, "\n"))
				}
				# if (ub < n) {
				  cat(paste0("P(X  > ", ub,") = ", round(1 - (p_ub + p_eub), dec), "\n"))
				  cat(paste0("P(X >= ", ub,") = ", round(1 - p_ub, dec), "\n"))
				# }
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " <= X <= ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " <= X <= ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {

		cat("Lower bound :", if (is.na(plb)) "\n" else paste0(plb, " (", vlb, ")\n"))
		cat("Upper bound :", if (is.na(pub)) "\n" else paste0(pub, " (", vub, ")\n"))

		if (!is.na(pub) || !is.na(plb)) {
		  cat("\n")

			if (!is.na(plb)) {
				cat(paste0("P(X  = ", vlb,") = ", vp_elb, "\n"))
				if (vlb > 0) {
					cat(paste0("P(X  < ", vlb,") = ", vp_lb, "\n"))
				  cat(paste0("P(X <= ", vlb,") = ", vp_lelb, "\n"))
				}
				# if (vlb < n) {
				  cat(paste0("P(X  > ", vlb,") = ", round(1 - (vp_lb + vp_elb), dec), "\n"))
			  	cat(paste0("P(X >= ", vlb,") = ", round(1 - vp_lb, dec), "\n"))
				# }
			}

			if (!is.na(pub)) {
				cat(paste0("P(X  = ", vub,") = ", vp_eub, "\n"))
				if (vub > 0) {
					cat(paste0("P(X  < ", vub,") = ", vp_ub, "\n"))
				  cat(paste0("P(X <= ", vub,") = ", vp_leub, "\n"))
				}
				# if (vub < n) {
				  cat(paste0("P(X  > ", vub,") = ", round(1 - (vp_ub + vp_eub), dec), "\n"))
		      cat(paste0("P(X >= ", vub,") = ", round(1 - vp_ub, dec), "\n"))
				# }
			}

			if (!is.na(plb) && !is.na(pub)) {
				cat(paste0("P(", vlb, " <= X <= ", vub,")     = ", vp_int, "\n"))
				cat(paste0("1 - P(", vlb, " <= X <= ", vub,") = ", round(1 - vp_int, dec), "\n"))
		  }
		}
	}
}
