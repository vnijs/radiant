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

	v_ub <- qnorm(pub, mean, stdev)
	v_lb <- qnorm(plb, mean, stdev)

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
    y[x < lb | x > ub] <- NA
    y
  }

  dnorm_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dnorm(x, mean = mean, sd = stdev)
    y[x > lb] <- NA
    y
  }

  dnorm_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dnorm(x, mean = mean, sd = stdev)
    y[x < ub] <- NA
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
#' @param lb Lower bound (default is -Inf)
#' @param ub Upper bound (default is Inf)
#' @param plb Lower probability bound
#' @param pub Upper probability bound
#' @param dec Number of decimals to show
#'
#' @export
prob_tdist <- function(df,
                       lb = NA,
                       ub = NA,
                       plb = NA,
                       pub = NA,
                       dec = 3) {

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
    y[x < lb | x > ub] <- NA
    y
  }

  dt_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dt(x, df = df)
    y[x > lb] <- NA
    y
  }

  dt_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dt(x, df = df)
    y[x < ub] <- NA
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


	# df1 <- 1
	# df2 <- 2

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
    y[x < lb | x > ub] <- NA
    y
  }

  df_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- df(x, df1 = df1, df2 = df2)
    y[x > lb] <- NA
    y
  }

  df_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- df(x, df1 = df1, df2 = df2)
    y[x < ub] <- NA
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
    y[x < lb | x > ub] <- NA
    y
  }

  dchisq_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dchisq(x, df = df)
    y[x > lb] <- NA
    y
  }

  dchisq_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dchisq(x, df = df)
    y[x < ub] <- NA
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
  cat("Distribution: Chi-squared\n")
	cat("Df          :", df, "\n")
	cat("Mean        :", n, "\n")
	cat("Variance    :", 2*n, "\n")

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
	cat("n          :", n, "\n")
	cat("p          :", p, "\n")
	cat("Mean       :", n*p, "\n")
	cat("St. dev    :", sqrt(n*p*(1-p)) %>% round(dec), "\n")

	mess <- object[[paste0("mess_",type)]]
	if (!is.null(mess)) return(mess)

	if (type == "values") {
		cat("Lower bound:", {if (is.na(lb)) "" else lb}, "\n")
		cat("Upper bound:", {if (is.na(ub)) "" else ub}, "\n")

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
					# if (lb > 0)
				  	cat(paste0("P(X >= ", lb,") = ", round(1 - p_lb, dec), "\n"))
				  # else
				  	# cat(paste0("P(X >= ", lb,") = ", 1, "\n"))
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
				  # if (ub > 0)
				  	cat(paste0("P(X >= ", ub,") = ", round(1 - p_ub, dec), "\n"))
				  # else
				  	# cat(paste0("P(X >= ", ub,") = ", 1, "\n"))
				}
			}

			if (!is.na(lb) && !is.na(ub)) {
				cat(paste0("P(", lb, " <= X <= ", ub,")     = ", p_int, "\n"))
				cat(paste0("1 - P(", lb, " <= X <= ", ub,") = ", round(1 - p_int, dec), "\n"))
		  }
		}

	} else {

		cat("Lower bound:", if (is.na(plb)) "\n" else paste0(plb, " (", vlb, ")\n"))
		cat("Upper bound:", if (is.na(pub)) "\n" else paste0(pub, " (", vub, ")\n"))

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
				  # if (vlb > 0)
				  	cat(paste0("P(X >= ", vlb,") = ", round(1 - vp_lb, dec), "\n"))
				  # else
				  	# cat(paste0("P(X >= ", vlb,") = ", 1, "\n"))
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
				  # if (vub > 0)
				    cat(paste0("P(X >= ", vub,") = ", round(1 - vp_ub, dec), "\n"))
				  # else
				    # cat(paste0("P(X >= ", vub,") = ", 1, "\n"))
				}
			}

			if (!is.na(plb) && !is.na(pub)) {
				cat(paste0("P(", vlb, " <= X <= ", vub,")     = ", vp_int, "\n"))
				cat(paste0("1 - P(", vlb, " <= X <= ", vub,") = ", round(1 - vp_int, dec), "\n"))
		  }
		}
	}
}

#' Probability calculator for the uniforem distribution
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
	stdev <- (max-min)^2 / (1/12)

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
    y[x < lb | x > ub] <- NA
    y
  }

  dunif_lb <- function(x) {
  	if (is.na(lb)) return(0)
    y <- dunif(x, min = min, max = max)
    y[x > lb] <- NA
    y
  }

  dunif_ub <- function(x) {
  	if (is.na(ub)) return(0)
    y <- dunif(x, min = min, max = max)
    y[x < ub] <- NA
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

#' Summary method for the probability calculator function (normal)
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
	cat("Min        :", min, "\n")
	cat("Max        :", max, "\n")
	cat("Mean       :", mean, "\n")
	cat("St. dev    :", stdev, "\n")

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
		cat("Lower bound:", {if (is.na(lb)) min else lb}, "\n")
		cat("Upper bound:", {if (is.na(ub)) max else ub}, "\n")

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

		cat("Lower bound:", if (plb < 0) "0" else plb, "\n")
		cat("Upper bound:", if (pub > 1) "1" else pub, "\n")

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
