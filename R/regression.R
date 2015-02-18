#' Linear regression using OLS
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param reg_dep_var The dependent variable in the regression
#' @param data_filter Expression intered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param reg_indep_var Independent variables in the regression
#' @param reg_test_var Variables to evaluate in model comparison (i.e., a competing models F-test)
#' @param reg_int_var Interaction term to include in the model
#' @param reg_interactions Should interactions be considered. Options are "", 2, and 3. None ("") is the default. To consider 2-way interactions choose 2, and for 2- and 3-way interactions choose 3.
#' @param reg_predict Choose the type of prediction input. Default is no prediction (""). To generate predictions using a data.frame choose ("data"), and to include a command to generate values to predict select ("cmd")
#' @param reg_predict_cmd Generate predictions using a command. For example, carat = seq(.5, 1.5, .1) would produce predicitions for values of carat starting at .5, increasing to 1.5 in increments of .1. Make sure to press Enter after you finish entering the command. If no results are shown the command was likely invalid. Try entering the same expression in the R(studio) console to debug the command
#' @param reg_predict_data Generate predictions by specifying the name of a dataset (e.g., "diamonds"). The dataset must have all columns used in model estimation
#' @param reg_check Optional output or estimation parameters. "rsme" to show the root mean squared error. "sumsquares" to show the sum of squares table. "vif" to show the multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates. "standardize" to use standardized coefficient estimates. "stepwise" to apply step-wise selection of variables to estimate the regression model
#' @param reg_conf_level Confidence level to use to estimate the confidence intervals (.95 is the default)
#' @param reg_plots Regression plots to produce for the specified regression model. Specify "" to avoid showing any plots (default). "hist" to show histograms of all variables in the model. "correlations" for a visual representation of the correlation matrix of all variables in the data. "scatter" to show scatter plots (or box plots for factors) for all independent variables with the dependent variable. "dashboard" a series of six plots that can be used to evaluate model fit visually. "resid_pred" to plot the independent variables against the model residuals. "coef" for a coefficient plot with adjustable confidence intervals. "leverage" to show leverage plots for each independent variable
#' @param reg_coef_int Include the intercept in the coefficient plot (TRUE, FALSE). FALSE is the default
#' @param reg_lines Optional lines to include in the select plot. "line" to include a line through a scatter plot. "loess" to include a polynomial refression fit line. To include both use c("line","loess")

#' @return A list with all variables defined in the function as an object of class regression
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#'
#' @seealso \code{\link{summary.regression}} to summarize results
#' @seealso \code{\link{plot.regression}} to plot results
#'
#' @export
regression <- function(dataset, reg_dep_var, reg_indep_var,
                       data_filter = "",
                       reg_test_var = "",
                       reg_int_var = "",
                       reg_interactions = "",
                       reg_predict = "",
                       reg_predict_cmd = "",
                       reg_predict_data = "",
                       reg_check = "",
                       reg_conf_level = .95,
                       reg_plots = "",
                       reg_coef_int = FALSE,
                       reg_lines = "") {


# dataset = "diamonds"
# reg_dep_var = "price"
# reg_indep_var = "color"
# reg_check = "standardize"

	vars <- reg_indep_var

	# adding interaction terms as needed
	if(reg_interactions != "" &&
	  reg_int_var != "" &&
	  length(vars) > 1) {
		if({reg_int_var %>% strsplit(":") %>% unlist} %in% reg_indep_var %>% all) {
			vars <- c(vars,reg_int_var)
	 	} else{
      cat("Interaction terms contain variables not selected as main effects.\nRemoving interactions from the estimation")
      reg_int_var <- ""
	 	}
	}

	dat <- getdata_exp(dataset, c(reg_dep_var, reg_indep_var), filt = data_filter)

	if("standardize" %in% reg_check) {
    isNum <- sapply(dat, is.numeric)
    if(sum(isNum > 0)) dat[,isNum] %<>% data.frame %>% mutate_each(funs(scale))
  }

	formula <- paste(reg_dep_var, "~", paste(vars, collapse = " + "))

	if("stepwise" %in% reg_check) {
    # use k = 2 for AIC, use k = log(nrow(dat)) for BIC
		model <- lm(paste(reg_dep_var, "~ 1") %>% as.formula, data = dat) %>%
      step(., k = 2, scope = list(upper = formula), direction = 'both')
	} else {
		model <- lm(formula, data = dat)
	}

  reg_coeff <- tidy(model)
  reg_coeff$` ` <- sig_stars(reg_coeff$p.value)
  reg_coeff[,c(2:5)] %<>% round(3)
  reg_coeff$p.value[reg_coeff$p.value < .001] <- "< .001"
  colnames(reg_coeff) <- c("  ","coefficient","std.error","t.value","p.value"," ")

  isFct <- sapply(select(dat,-1), is.factor)
  if(sum(isFct) > 0) {
    for(i in names(select(dat,-1)[isFct]))
      reg_coeff$`  ` %<>% gsub(i, paste0(i," > "), .)
  }

  if("confint" %in% reg_check || "coef" %in% reg_plots) {
    if(model$coeff %>% is.na %>% any) {
      reg_plots <- ""
    } else {
      cl_split <- function(x) 100*(1-x)/2
      cl_split(reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
      (100 - cl_split(reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

      confint(model, level = reg_conf_level) %>%
        data.frame %>%
        magrittr::set_colnames(c("Low","High")) %>%
        cbind(select(reg_coeff,2),.) %>%
        round(3) %>%
        set_rownames(reg_coeff$`  `) -> ci_tab
    }
  }

	# specifying plot heights
	nrVars <- length(as.character(attr(model$terms,'variables'))[-1])
	plot_height <- 500
	plot_width <- 650

	if(reg_plots == 'hist') plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
	if(reg_plots == 'dashboard') plot_height <- 1.5 * plot_height
	if(reg_plots == 'correlations') { plot_height <- 150 * nrVars; plot_width <- 150 * nrVars }
  if(reg_plots == 'coef') plot_height <- 300 + 20 * length(model$coefficients)
	if(reg_plots %in% c('scatter','leverage','resid_pred'))
		plot_height <- (plot_height/2) * ceiling((nrVars-1) / 2)

  environment() %>% as.list %>% set_class(c("regression",class(.)))
}

#' Summarize results from the regression. This is a method of class regression and can be called as summary or summary.regression
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{regression}}
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' summary(result)
#'
#' @seealso \code{\link{regression}} to generate the results
#' @seealso \code{\link{plot.regression}} to plot results
#'
#' @export
summary.regression <- function(result) {

	if(class(result$model) != 'lm') return(result)

  cat("Linear regression (OLS)\n")
  cat("Data     :", result$dataset, "\n")
  if(result$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
  cat("Dependent variable   :", result$reg_dep_var, "\n")
  cat("Independent variables:", paste0(result$reg_indep_var, collapse=", "), "\n")
  if("standardize" %in% result$reg_check)
 		cat("Standardized coefficients shown\n")
 	cat("\n")
  # cat("Null hyp.: variables x and y are not correlated\n")
  # cat("Alt. hyp.: variables x and y are correlated\n\n")
	print(result$reg_coeff, row.names=FALSE)

	reg_fit <- glance(result$model) %>% round(3)
	if(reg_fit['p.value'] < .001) reg_fit['p.value'] <- "< .001"
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("R-squared:", paste0(reg_fit$r.squared, ", "), "Adjusted R-squared:", reg_fit$adj.r.squared, "\n")
  cat("F-statistic:", reg_fit$statistic, paste0("df(", reg_fit$df, ",", reg_fit$df.residual, "), p.value"), reg_fit$p.value)
  cat(paste0("\nNr obs: ", reg_fit$df + reg_fit$df.residual))
  cat("\n\n")

  if("rmse" %in% result$reg_check) {
    mean(result$model$residual^2, na.rm=TRUE) %>% sqrt %>%
    cat("Prediction error (RMSE): ", ., "\n\n")
  }

  if("sumsquares" %in% result$reg_check) {
    atab <- anova(result$model)
    atab %>% format(scientific = FALSE)
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

  if("vif" %in% result$reg_check) {
		if(result$model$coeff %>% is.na %>% any) {
			cat("The set of independent variables exhibit perfect multi-collinearity.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
		} else {
      if(length(result$reg_indep_var) > 1) {
        cat("Variance Inflation Factors\n")
        vif(result$model) %>%
          { if(!dim(.) %>% is.null) .[,"GVIF"] else . } %>% # needed when factors are included
          data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
          round(3) %>%
          .[order(.$VIF, decreasing=T),] %>%
          { if(nrow(.) < 8) t(.) else . } %>% print
      } else {
        cat("Insufficient number of independent variables selected to calculate\nmulti-collinearity diagnostics")
      }
		}
    cat("\n")
  }

  if(result$reg_predict != "") {

    # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
    # as starting point
    if("standardize" %in% result$reg_check) {
      cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the standardized coefficients box and try again")
    } else if ( result$reg_predict == "cmd" && result$reg_predict_cmd == "") {
      cat("Please specify a command to generate predictions. For example,\ncarat = seq(.5, 1.5, .1) would produce predicitions for values of\ncarat starting at .5, increasing to 1.5 in increments of .1. \nMake sure to press Enter after you finish entering the command.\nIf no results are shown the command was likely invalid. Try entering\nthe same expression in the R(studio) console to debug the command")
    } else if ( result$reg_predict == "data" && result$reg_predict_data == "") {
      cat("Please select a dataset to generate predictions. You could create this in Excel\nand use the paste feature in Data > Manage to bring it into Radiant")
    } else {

      if(result$reg_predict == "cmd") {
     		reg_predict_cmd <- gsub("\"","\'", result$reg_predict_cmd)
        nval <- try(eval(parse(text = paste0("data.frame(", reg_predict_cmd ,")"))), silent = TRUE)
      } else {
        nval <- getdata_exp(result$reg_predict_data)
        nval_names <- names(nval)
        vars <- as.character(attr(result$model$terms,'variables'))[-1]
        nval <- try(select_(nval, .dots = vars[-1]), silent = TRUE)
      }

      if(is(nval, 'try-error')) {
        if(result$reg_predict == "cmd") {
          cat("The expression entered does not seem to be correct. Please try again.\n")
          cat("Examples are shown in the helpfile.\n")
        } else {
          cat("The profiles to predict do not contain all variables that are in the model.\n")
          cat("Add variables to the profiles data as needed.\n\n")
          ivars <- vars[-1]
          cat("Model variables: ")
          cat(ivars,"\n")
          cat("Profile variables to be added: ")
          # nval_names <- names(r_data[[result$reg_predict_data]])
          cat(ivars[!ivars %in% nval_names])
        }
      } else {

        dat <- ggplot2::fortify(result$model)
        vars <- as.character(attr(result$model$terms,'variables'))[-1]
        reg_dep_var <- vars[1]
        reg_indep_var <- vars[-1]
        dat <- dat[,reg_indep_var, drop = FALSE]

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
            newdat[names(nval)] <- list(NULL)
            nnd <- data.frame(newdat[-1],nval)
            pred <- try(predict(result$model, nnd, interval = 'prediction', level = result$reg_conf_level), silent = TRUE)

            if(!is(pred, 'try-error')) {
            	if(result$reg_predict == "data") {
              	cat(paste0("Predicted values for profiles from dataset: ",result$reg_predict_data,"\n"))
              } else {
              	cat("Predicted values for:\n")
              }

              pred %<>% data.frame %>% mutate(diff = .[,3] - .[,1])
              cl_split <- function(x) 100*(1-x)/2
            	cl_split(result$reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
            	(100 - cl_split(result$reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high
            	colnames(pred) <- c("Prediction",cl_low,cl_high,"+/-")

            	nnd <- data.frame(nnd, pred, check.names = FALSE)

            	# pushing predictions into the clipboard
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

  if("confint" %in% result$reg_check) {
    if(result$model$coeff %>% is.na %>% any) {
      cat("There is perfect multi-collineary in the set of independent variables.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
    } else {
      result$ci_tab$`+/-` <- (result$ci_tab$High - result$ci_tab$coefficient)
      result$ci_tab %>%
        magrittr::set_colnames(c("coefficient", result$cl_low, result$cl_high, "+/-")) %>%
        print
      cat("\n")
    }
  }

	# if(!result$reg_test_var %>% is_empty) {
  if(!result$reg_test_var == "") {
		if("stepwise" %in% result$reg_check) {
	  	cat("Model comparisons are not conducted when Stepwise has been selected.\n")
	  } else {

				sub_formula <- ". ~ 1"
				vars <- result$reg_indep_var

				if(result$reg_interactions != "" &&
					  result$reg_int_var != "" &&
					  length(vars) > 1) {

						if({result$reg_int_var %>% strsplit(":") %>% unlist} %in% result$reg_test_var %>% any) {
	 						cat("Interaction terms contain variables specified for testing.\nRelevant interaction terms are include in the requested test.\n\n")

							for(i in result$reg_test_var) {
								ind <- grep(i,result$reg_int_var)
								result$reg_test_var <- c(result$reg_test_var, result$reg_int_var[ind])
							}
							result$reg_test_var <- unique(result$reg_test_var)
	 					}
			 			vars <- c(vars,result$reg_int_var)
				}
				not_selected <- setdiff(vars,result$reg_test_var)
				if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))

				sub_mod <- update(result$model, sub_formula, data = result$dat) %>%
				             anova(., result$model, test='F')

				if(sub_mod[,"Pr(>F)"][2] %>% is.na) return(cat(""))
				if(sub_mod[,"Pr(>F)"][2] < .001) sub_mod[,"Pr(>F)"][2] <- "< .001"

        cat("\n")
				cat(attr(sub_mod,"heading")[2])
  				result$dat[,result$reg_dep_var] %>%
  				{ sum((. - mean(.))^2) } %>%
  				{1 - (sub_mod$RSS / .)} %>%
  				round(3) %>%
  				cat("\nR-squared, Model 1 vs 2:", .)
  			cat("\nF-statistic:", sub_mod$F[2] %>% round(3), paste0("df(", sub_mod$Res.Df[1], ",", sub_mod$Res.Df[2], "), p.value"),
            sub_mod[,"Pr(>F)"][2])
	  }
	}
}

#' Plot results from the regression function. This is a method of class regression and can be called as plot or plot.regression
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{regression}}
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"), reg_plots = "dashboard")
#' plot(result)
#'
#' @seealso \code{\link{regression}} to generate the result
#' @seealso \code{\link{summary.regression}} to summarize results
#'
#' @export
plot.regression <- function(result) {

	if(class(result$model) != 'lm') return(result)

	if({result$reg_int_var %>% strsplit(":") %>% unlist} %in% result$reg_indep_var %>% all) {
		# nothing
 	} else {
    cat("Interaction terms contain variables not selected as\nmain effects. Removing interactions from the estimation")
    reg_int_var <- ""
 	}

	model <- ggplot2::fortify(result$model)
  model$.actual <- model[,result$reg_dep_var]

	vars <- as.character(attr(result$model$terms,'variables'))[-1]
	reg_dep_var <- vars[1]
	reg_indep_var <- vars[-1]

	# dat <- model[,vars, drop = FALSE]

	if(result$reg_plots == "")
		return(plot(x = 1, type = 'n', main="Please select a plot from the Regression plots dropdown menu.", axes = FALSE, xlab = "", ylab = ""))

	if(result$reg_plots == "hist") {
		plots <- list()
		for(i in vars) plots[[i]] <- ggplot(result$dat, aes_string(x = i)) + geom_histogram()
	}

	if(result$reg_plots == "correlations") {
		return(plots_correlation(result))
	}

	if(result$reg_plots == "dashboard") {

		plots <- list()
		# df <- data.frame(cbind(model$.fitted,model[1]))
		# colnames(df) <- c("x","y")
		plots[[1]] <- ggplot(model, aes(x=.fitted, y=.actual)) + geom_point() + labs(list(title = "Actual vs Fitted values", x = "Fitted", y = "Actual"))
    if("line" %in% result$reg_lines) plots[[1]] <- plots[[1]] + geom_abline(linetype = 'dotdash')
    if("loess" %in% result$reg_lines) plots[[1]] <- plots[[1]] + geom_smooth(size = .75, linetype = "dotdash")

		plots[[2]] <- qplot(.fitted, .resid, data = model) + labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals"))
    if("line" %in% result$reg_lines) plots[[2]] <- plots[[2]] + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75, linetype = "dashed", colour = 'black')
    if("loess" %in% result$reg_lines) plots[[2]] <- plots[[2]] + geom_smooth(size = .75, linetype = "dotdash")

		plots[[3]] <- qplot(y=.resid, x=seq_along(.resid), data = model, geom="line") +
			labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))
    if("line" %in% result$reg_lines) plots[[3]] <- plots[[3]] + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75, linetype = "dashed", colour = 'black')
    if("loess" %in% result$reg_lines) plots[[3]] <- plots[[3]] + geom_smooth(size = .75, linetype = "dotdash")

		plots[[4]] <- qplot(sample =.stdresid, data = model, stat = "qq") +
			labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))
    if("line" %in% result$reg_lines) plots[[4]] <- plots[[4]] + geom_abline(linetype = 'dotdash')

  	plots[[5]] <- ggplot(model, aes(x = .resid)) + geom_histogram() +
      labs(list(title = "Histogram of residuals", x = "Residuals"))

    plots[[6]] <- ggplot(model, aes(x=.resid)) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(model[,'.resid']), sd = sd(model[,'.resid'])), color = "blue") +
  		labs(list(title = "Residual vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())

	}

	if(result$reg_plots == "scatter") {
		plots <- list()
		for(i in reg_indep_var) {
			if('factor' %in% class(result$dat[,i])) {
				plots[[i]] <- ggplot(result$dat, aes_string(x=i, y=reg_dep_var, fill=i)) + geom_boxplot(alpha = .7)
			} else {
				p <- ggplot(result$dat, aes_string(x=i, y=reg_dep_var)) + geom_point()
        if("line" %in% result$reg_lines) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75,
				            linetype = "dashed", colour = 'black')
        if("loess" %in% result$reg_lines) p <- p + geom_smooth(size = .75, linetype = "dotdash")
        plots[[i]] <- p
			}
		}
	}

	if(result$reg_plots == "resid_pred") {
		plots <- list()
		rdat <- cbind(model$.resid,result$dat[,reg_indep_var]) %>% data.frame
		colnames(rdat) <- c('residuals',reg_indep_var)
		for(i in reg_indep_var) {
			if('factor' %in% class(result$dat[,i])) {
				plots[[i]] <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_boxplot(fill = 'blue', alpha = .7)
			} else {
				p <- ggplot(rdat, aes_string(x=i, y="residuals")) + geom_point()
        if("line" %in% result$reg_lines) p <- p + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75,
                                                 linetype = "dashed", colour = 'black')
        if("loess" %in% result$reg_lines) p <- p + geom_smooth(size = .75, linetype = "dotdash")
        plots[[i]] <- p
			}
		}
	}

	if(result$reg_plots == "leverage") {
		return(leveragePlots(result$model, main = "", ask=FALSE, id.n = 1, layout = c(ceiling(length(reg_indep_var)/2),2)))
	}

  if(result$reg_plots == "coef") {
    result$ci_tab %>%  { if(!result$reg_coef_int) .[-1,] else . } %>%
      mutate(variable = rownames(.)) %>% ggplot() +
      geom_pointrange(aes(x = variable, y = coefficient, ymin = Low, ymax = High)) +
      geom_hline(yintercept = 0, linetype = 'dotdash', color = "blue") + coord_flip() -> plot
      return(plot)
	}

	if(exists("plots"))
		sshh( do.call(grid.arrange, c(plots, list(ncol = 2))) )

}

