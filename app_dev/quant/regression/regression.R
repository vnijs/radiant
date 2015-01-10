################################################################
# Regression
################################################################

# put in a bunch of defaults here!
# if a user wants to call this from regular code they don't want to
# put in values for everything they don't use!
regression <- function(dataset, reg_var1, reg_var2,
                       reg_var3,
                       reg_intsel,
                       reg_interactions,
                       reg_predict_buttons,
                       reg_predict,
                       reg_predict_data,
                       reg_standardize,
                       reg_sumsquares,
                       reg_confint, reg_conf_level,
                       reg_rmse,
                       reg_vif,
                       reg_stepwise,
                       reg_plots,
                       reg_line,
                       reg_loess) {

	vars <- reg_var2

	# adding interaction terms as needed
	if(reg_interactions != 'none' && !is.null(reg_intsel) && length(vars) > 1) {
		vars <- c(vars,reg_intsel)
	}

	dat <- values[[dataset]]
	if(reg_standardize) dat <- mutate_each(dat,funs(reg_standardize_fun))

	formula <- paste(reg_var1, "~", paste(vars, collapse = " + "))

	if(reg_stepwise) {
		mod <- step(lm(as.formula(paste(reg_var1, "~ 1")), data = dat),
		            scope = list(upper = formula), direction = 'both')
		# not sure what the k = log ... setting was for. AIC vs BIC?
		# mod <- step(lm(as.formula(paste(reg_var1, "~ 1")), data = dat),
		#             k = log(nrow(dat)), scope = list(upper = formula),
		#             direction = 'both')
	} else {
		mod <- lm(formula, data = dat)
	}


	# Could this just be a list of inputs passed on?
	# just like for single_mean?
	# something like: args_reg <- as.list(formals(regression))
	# but then the list of stuff that was passed to the function?


	# looks like you might need match.call ... or the argg one (see in single_mean)
	# http://stackoverflow.com/questions/11885207/get-all-parameters-as-list
	# http://stackoverflow.com/questions/14397364/match-call-with-default-arguments


  #
  #
  # use one of the following like in single mean
  #
  #

  # print(as.list(match.call()))
  # as.list(environment())

	# list of inputs
	mod$reg_predict_buttons <- reg_predict_buttons
	mod$reg_predict <- reg_predict
	mod$reg_predict_data <- reg_predict_data
	mod$reg_sumsquares <- reg_sumsquares
	mod$reg_confint <- reg_confint
	mod$reg_conf_level <- reg_conf_level
	mod$reg_rmse <- reg_rmse
	mod$reg_vif <- reg_vif
	mod$reg_var1 <- reg_var1
	mod$reg_var2 <- reg_var2
	mod$reg_var3 <- reg_var3
	mod$reg_stepwise <- reg_stepwise
	mod$reg_plots <- reg_plots
	mod$reg_interactions <- reg_interactions
	mod$reg_intsel <- reg_intsel
	mod$reg_standardize <- reg_standardize
	mod$reg_line <- reg_line
	mod$reg_loess <- reg_loess
	mod$dataset <- dataset

	return(mod)
}

summary_regression <- function(result = .regression()) {

	if(class(result) != 'lm') return(result)

	# rounding to avoid scientific notation for the coefficients
  #	res <- summary(format(result, scientific = FALSE))
  #	res$coefficients <- format(res$coefficients, scientific = FALSE)
	res <- summary(result)
 	res$coefficients <- round(res$coefficients,3)
	.print.summary.lm(res, dataset = result$dataset, dv = result$reg_var1, std_c = result$reg_standardize, digits = 3)
  # print(res, digits = 3)

	# if(reg_outlier) print(outlierTest(result), digits = 3)

  if(result$reg_predict != '' || result$reg_predict_data != "none") {

    # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
    # as starting point
    if(result$reg_standardize) {
      cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the
          standardized coefficients box and try again.")
    } else {

      if(result$reg_predict_buttons == "cmd") {
     		reg_predict <- gsub("\"","\'", result$reg_predict)
        nval <- try(eval(parse(text = paste0("data.frame(",reg_predict,")"))), silent = TRUE)
      } else {
        nval <- values[[result$reg_predict_data]]
        vars <- as.character(attr(result$terms,'variables'))[-1]
        nval <- try(select_(nval, .dots = vars[-1]), silent = TRUE)
      }

      if(is(nval, 'try-error')) {
        if(result$reg_predict_buttons == "cmd") {
          cat("The expression entered does not seem to be correct. Please try again.\n")
          cat("Examples are shown in the helpfile.\n")
        } else {
          cat("The profiles to predict do not contain all variables that are in the model.\n")
          cat("Add variables to the profiles data as needed.\n\n")
          ivars <- vars[-1]
          cat("Model variables: ")
          cat(ivars,"\n")
          cat("Profile variables to be added: ")
          nval_names <- names(values[[result$reg_predict_data]])
          cat(ivars[!ivars %in% nval_names])
        }
      } else {

        dat <- ggplot2::fortify(result)
        vars <- as.character(attr(result$terms,'variables'))[-1]
        reg_var1 <- vars[1]
        reg_var2 <- vars[-1]
        dat <- dat[,reg_var2, drop = FALSE]

        isFct <- sapply(dat, is.factor)
        isNum <- sapply(dat, is.numeric)

        if(sum(isNum) + sum(isFct) < dim(dat)[2]) {
          cat("The model includes data-types that cannot be used for\nprediction at this point\n")
        } else {

          newdat <- ""
          if(sum(isNum) > 0)  newdat <- data.frame(newdat,t(colMeans(dat[,isNum, drop = FALSE])))
          # from http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
          if(sum(isFct) > 0)  newdat <- data.frame(newdat,t(apply(dat[,isFct, drop = FALSE],2,function(x) names(which.max(table(x))))))

          # if(sum(names(nval) %in% names(newdat)) < length(nval)) {
          if(sum(names(nval) %in% names(newdat)) < length(names(nval))) {
            cat("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n")
          } else {
            if(result$reg_predict_buttons == "cmd" & result$reg_predict == "") {
              pred <- try(log("a"), silent=TRUE)
            } else {
              newdat[names(nval)] <- list(NULL)
              nnd <- data.frame(newdat[-1],nval)
              pred <- try(predict(result, nnd,interval = 'prediction', level = result$reg_conf_level), silent = TRUE)
              # pred <- try(predict(result, nnd,interval = 'prediction', level = as.numeric(reg_conf_level)), silent = TRUE)
            }

            if(!is(pred, 'try-error')) {
            	if(result$reg_predict_buttons == "dataframe") {
              	cat(paste0("Predicted values for profiles from dataset: ",result$reg_predict_data,"\n"))
              } else {
              	cat("Predicted values for:\n")
              }

            	pred <- data.frame(pred,pred[,3]-pred[,1])
              cl_split <- function(x) 100*(1-x)/2
            	cl_split(result$reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
            	(100 - cl_split(result$reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high
            	colnames(pred) <- c("Prediction",cl_low,cl_high,"+/-")

            	nnd <- data.frame(nnd, pred, check.names = FALSE)

            	# putting the predictions into the clipboard
            	os_type <- .Platform$OS.type
            	if (os_type == 'windows') {
            	  write.table(nnd, "clipboard", sep="\t", row.names=FALSE)
            	} else {
            	  write.table(nnd, file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
            	}

            	nnd %>% print(., row.names = FALSE)
              cat("\n")
            } else {
              cat("The expression entered does not seem to be correct. Please try again.\nExamples are shown in the helpfile.\n")
            }
          }
        }
      }
    }
  }

  if(result$reg_rmse) {
    rmse <- sqrt(mean(res$residual^2,na.rm=TRUE))
    # rmse_df <- data.frame("RMSE" = rmse, "RMSE (95%)" = rmse *2, check.names = FALSE)
    cat("Prediction error (RMSE): ", rmse, "\n\n")
	}

  if(result$reg_sumsquares) {
	  atab <- anova(result)
	  nr_rows <- dim(atab)[1]
	  df_reg <- sum(atab$Df[-nr_rows])
	  df_err <- sum(atab$Df[nr_rows])
	  df_tot <- df_reg + df_err

	  ss_reg <- sum(atab$`Sum Sq`[-nr_rows])
	  ss_err <- sum(atab$`Sum Sq`[nr_rows])
	  ss_tot <- ss_reg + ss_err
	  ssTable <- data.frame(matrix(nrow = 3, ncol = 2))
	  rownames(ssTable) <- c("Regression","Error","Total")
	  colnames(ssTable) <- c("df","SS")
	  ssTable$df <- c(df_reg,df_err,df_tot)
	  ssTable$SS <- c(ss_reg,ss_err,ss_tot)
    cat("Sum of squares:\n")
    print(format(ssTable, scientific = FALSE))
    cat("\n")
  }

  if(result$reg_vif) {
		print(vif_regression(result), digits = 3)
    cat("\n")
	}

	if(result$reg_confint) {

    cl_split <- function(x) 100*(1-x)/2
    cl_split(result$reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
    (100 - cl_split(result$reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

    cat("Coefficient confidence intervals:\n")
    confint(result, level = result$reg_conf_level) %>%
      data.frame %>%
      magrittr::set_colnames(c("Low","High")) %>%
      cbind(Estimate = result$coefficients,.) -> dat

    dat$`+/-` <- dat$High - dat$Estimate
    dat %>%
      magrittr::set_colnames(c("Estimate",cl_low,cl_high, "+/-")) %>%
      print
    cat("\n")
	}

	if(!is.null(result$reg_var3)) {
		if(!result$reg_stepwise) {
      test_regression(result)
      # NA's shown
      # print(format(test_regression(result), scientific = FALSE))
		} else {
	  	cat("Model comparisons not conducted when Stepwise has been selected.\n")
	  }
	}
}

.print.summary.lm <- function (x, dataset = "", dv = "", std_c = FALSE, digits = max(3L, getOption("digits") - 3L), signif.stars = getOption("show.signif.stars"), ...) {

  # adapted from getAnywhere(print.summary.lm)
	# cat(paste("Dependent variable:",input$reg_var1,"\n"))
  cat(paste("Data:",dataset,"\n"))
  cat(paste("Dependent variable:",dv,"\n"))

  if(std_c == TRUE) {
    cat("Standardized coefficients:\n")
  } else {
    cat("Coefficients:\n")
  }
  coefs <- x$coefficients
  if (!is.null(aliased <- x$aliased) && any(aliased)) {
    cn <- names(aliased)
    coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, colnames(coefs)))
    coefs[!aliased, ] <- x$coefficients
  }
  printCoefmat(coefs, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
	# printCoefmat(coefs, digits = digits, signif.stars = signif.stars, na.print = "NA", scientific = FALSE, ...)

  cat("\n")
  if (nzchar(mess <- naprint(x$na.action)))
    cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("R-squared: ", formatC(x$r.squared, digits = digits))
    cat(", Adjusted R-squared: ", formatC(x$adj.r.squared, digits = digits),
        "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits), "on", x$fstatistic[2L],
        "and", x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),  digits = digits)
    )
    cat(paste0("\nNr obs: ",length(x$residuals)))
    cat("\n\n")
  }
}

# main functions called from radiant.R
r_plots <- list("None" = "", "Histograms" = "histlist", "Correlations" = "correlations", "Scatter" = "scatterlist", "Dashboard" = "dashboard",
		# "Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots", "Coefficient plot" = "coef")
		"Residual vs predictor" = "resid_vs_predictorlist", "Leverage plots" = "leverage_plots")

plots_regression <- function(result = .regression()) {

	if(class(result) != 'lm') return(result)
	mod <- ggplot2::fortify(result)

	vars <- as.character(attr(result$terms,'variables'))[-1]
	reg_var1 <- vars[1]
	reg_var2 <- vars[-1]

	dat <- mod[,vars, drop = FALSE]

	if(result$reg_plots == "")
		return(plot(x = 1, type = 'n', main="Please select a plot from the Regression plots dropdown menu.", axes = FALSE, xlab = "", ylab = ""))

	if(result$reg_plots == "histlist") {
		plots <- list()
		for(i in vars) plots[[i]] <- ggplot(dat, aes_string(x = i)) + geom_histogram()
	}

	if(result$reg_plots == "correlations") {
		datlist <- list()
		datlist$dat <- dat
		return(plots_correlation(datlist))
	}

	if(result$reg_plots == "dashboard") {

		plots <- list()
		df <- data.frame(cbind(mod$.fitted,mod[1]))
		colnames(df) <- c("x","y")
		# plots[[1]] <- ggplot(df, aes(x=x, y=y)) + geom_point() + labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual values"))
		p <- ggplot(df, aes(x=x, y=y)) + geom_point() + labs(list(title = "Actual vs Fitted", x = "Fitted values", y = "Actual values"))
    if(result$reg_line) p <- p + geom_abline(linetype = 'dotdash')
    if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
    plots[[1]] <- p

		p <- qplot(.fitted, .resid, data = mod) + labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))
    if(result$reg_line) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75, linetype = "dashed", colour = 'black')
    if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
    plots[[2]] <- p

		#	p <- qplot(y=.resid, x=seq_along(.resid), data = mod) + geom_point() +
		p <- qplot(y=.resid, x=seq_along(.resid), data = mod, geom="line") +
			labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))
    if(result$reg_line) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75, linetype = "dashed", colour = 'black')
    if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
    plots[[3]] <- p

		p <- qplot(sample =.stdresid, data = mod, stat = "qq") +
			labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))
    if(result$reg_line) p <- p + geom_abline(linetype = 'dotdash')
		# if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
    plots[[4]] <- p

	plots[[5]] <- ggplot(mod, aes(x = .resid)) + geom_histogram() +
			labs(list(title = "Histogram of residuals", x = "Residuals"))

  plots[[6]] <- ggplot(mod, aes(x=.resid)) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(mod[,'.resid']), sd = sd(mod[,'.resid'])), color = "blue") +
			labs(list(title = "Residual vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())

	}

	if(result$reg_plots == "scatterlist") {
		plots <- list()
		for(i in reg_var2) {
			if('factor' %in% class(dat[,i])) {
				plots[[i]] <- ggplot(dat, aes_string(x=i, y=reg_var1, fill=i)) + geom_boxplot(alpha = .3)
			} else {
				p <- ggplot(dat, aes_string(x=i, y=reg_var1)) + geom_point()
        if(result$reg_line) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75,
				            linetype = "dashed", colour = 'black')
        if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
        plots[[i]] <- p
			}
		}
	}

	if(result$reg_plots == "resid_vs_predictorlist") {
		plots <- list()
		residuals <- mod$.resid
		rdat <- cbind(residuals,dat[,reg_var2])
		rdat <- data.frame(rdat)
		colnames(rdat) <- c('residuals',reg_var2)
		for(i in reg_var2) {
			if('factor' %in% class(dat[,i])) {
				plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_boxplot(fill = 'blue', alpha = .3)
			} else {
				p <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_point()
        if(result$reg_line) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75,
                                                 linetype = "dashed", colour = 'black')
        if(result$reg_loess) p <- p + geom_smooth(size = .75, linetype = "dotdash")
        plots[[i]] <- p
			}
		}
	}

	if(result$reg_plots == "leverage_plots") {
		return(leveragePlots(result, main = "", ask=FALSE, id.n = 1, layout = c(ceiling(length(reg_var2)/2),2)))
	}

	# coefficient plots require the arm package (Gelman)
	# if(result$reg_plots == "coef") {
	# 	return(coefplot(result, xlab="", ylab="", main="Coefficient plot", col.pts="blue", CI=2))
	# }

	if(exists("plots"))
		suppressWarnings(suppressMessages(do.call(grid.arrange, c(plots, list(ncol = 2)))))
}

saveRegResiduals <- function(result = .regression()) {
	resid <- data.frame(result$residuals)
	changedata(resid, 'residuals')
}

# save residuals
observe({
	if(is.null(input$saveres) || input$saveres == 0) return()
	isolate({
		result <- .regression()
		if(is.character(result)) return()
		saveRegResiduals(result)
	})
})

################################################################
# Additional functions for regression
################################################################
reg_standardize_fun <- function(x) {
	if(is.numeric(x)) return(scale(x))
  x
}

reg_int_vec <- function(reg_vars, nway) {
	n <- length(reg_vars)
	iway <- c()
	for(i in 1:(n-1)) {
		for(j in (i+1):n) {
			iway <- c(iway, paste(reg_vars[i],reg_vars[j],sep=":"))
		}
	}
	if(n >= 3 && nway == '3way') {
		for(i in 1:(n-2)) {
			for(j in (i+1):(n-1)) {
				for(k in (j+1):n) {
					iway <- c(iway, paste(reg_vars[i],reg_vars[j],reg_vars[k],sep=":"))
				}
			}
		}
	}
	iway
}

vif_regression <- function(result = .regression()) {
	if(result$reg_vif) {
		if(length(result$reg_var2) > 1) {

  	 	VIF <- try(vif(result))
      if(is(VIF, 'try-error')) {
        cat("Insufficient number of independent variables selected to calculate VIF scores\n")
      } else {
	  	  cat("Variance Inflation Factors\n")
      	if(!is.null(dim(VIF))) VIF <- VIF[,'GVIF'] # needed when factors are included
        VIF <- data.frame("VIF" = VIF, "Rsq" = 1 - 1/VIF)
	  	  VIF <- VIF[order(VIF$VIF, decreasing=T),]
	  	  ifelse(nrow(VIF) < 8, return(VIF %>% t), return(VIF))
      }
		} else {
	  	cat("Insufficient number of independent variables selected to calculate VIF scores\n")
		}
	}
}

test_regression <- function(result = .regression()) {
	dat <- values[[result$dataset]]
	if(result$reg_standardize) dat <- data.frame(lapply(dat,reg_standardize))

	sub_formula <- ". ~ 1"
	vars <- result$reg_var2

  if(result$reg_interactions != 'none' && !is.null(result$reg_intsel) && length(vars) > 1) {
		vars <- c(vars,result$reg_intsel)
	}

	not_selected <- setdiff(vars,result$reg_var3)
	if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))

	reg_sub <- update(result, sub_formula)
	anova(reg_sub, result, test='F')
}


###############################
# Add coeficient plots
###############################
#
# customer.stats <- data.frame(b = lm1.c$coefficient[weekday.par.names],
#                              ci.lower=confint(lm1.c,weekday.par.names)[,1],
#                              ci.upper=confint(lm1.c,weekday.par.names)[,2],
#                              weekday=factor(weekday.names,levels=weekday.names),
#                              usertype=rep('Customer',length(weekday.names)))
#
# sub.stats <- data.frame(b = lm1.s$coefficient[weekday.par.names],
#                         ci.lower=confint(lm1.s,weekday.par.names)[,1],
#                         ci.upper=confint(lm1.s,weekday.par.names)[,2],
#                         weekday=factor(weekday.names,levels=weekday.names),
#                         usertype=rep('Subscriber',length(weekday.names)))
#
# reg.stats <- rbind(customer.stats,sub.stats)
#
# weekday.effects.plot <- ggplot(data=reg.stats) +
#   geom_pointrange(aes(x=weekday,y=b,ymin=ci.lower,ymax=ci.upper,color=usertype)) +
#   geom_line(aes(x=weekday,y=b,color=usertype,group=usertype),linetype='dotted') +
#   geom_hline(aes(yintercept=0))+
#   facet_wrap(~usertype,scales='free_y')
