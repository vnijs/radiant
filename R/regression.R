#' Linear regression using OLS
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param reg_dep_var The dependent variable in the regression
#' @param reg_indep_var Independent variables in the regression
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param reg_int_var Interaction terms to include in the model
#' @param reg_check "standardize" to see standardized coefficient estimates. "stepwise" to apply step-wise selection of variables in estimation
#'
#' @return A list of all variables used in regression as an object of class regression
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' result <- regression("diamonds", "price", c("carat","clarity"), reg_check = "standardize")
#'
#' @seealso \code{\link{summary.regression}} to summarize results
#' @seealso \code{\link{plot.regression}} to plot results
#' @seealso \code{\link{predict.regression}} to generate predictions
#'
#' @export
regression <- function(dataset, reg_dep_var, reg_indep_var,
                       data_filter = "",
                       reg_int_var = "",
                       reg_check = "") {

  dat <- getdata(dataset, c(reg_dep_var, reg_indep_var), filt = data_filter)

  vars <- ""
  var_check(reg_indep_var, colnames(dat)[-1], reg_int_var) %>%
    { vars <<- .$vars; reg_indep_var <<- .$indep_var; reg_int_var <<- .$int_var }

	if("standardize" %in% reg_check) {
    isNum <- sapply(dat, is.numeric)
    if(sum(isNum > 0)) dat[,isNum] %<>% data.frame %>% mutate_each(funs(scale))
  }

	formula <- paste(reg_dep_var, "~", paste(vars, collapse = " + ")) %>% as.formula

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

    rm(i, isFct)
  }

  # dat not needed elsewhere
  rm(dat)

  environment() %>% as.list %>% set_class(c("regression",class(.)))
}

#' Summary method for regression
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regression}}
#' @param reg_sum_check Optional output or estimation parameters. "rsme" to show the root mean squared error. "sumsquares" to show the sum of squares table. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates.
#' @param reg_conf_level Confidence level used to estimate confidence intervals (.95 is the default)
#' @param reg_test_var Variables to evaluate in model comparison (i.e., a competing models F-test)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' summary(result, reg_sum_check = c("rmse","sumsquares","vif","confint"), reg_test_var = "clarity")
#' result <- regression("shopping", "v1", c("v2","v3"))
#' summary(result, reg_test_var = "v2")
#'
#' @seealso \code{\link{regression}} to generate the results
#' @seealso \code{\link{plot.regression}} to plot results
#' @seealso \code{\link{predict.regression}} to generate predictions
#'
#' @importFrom car vif
#'
#' @export
summary.regression <- function(object,
                               reg_sum_check = "",
                               reg_conf_level = .95,
                               reg_test_var = "",
                               ...) {

	if(class(object$model)[1] != 'lm') return(object)

  # cat("Time",now(),"\n")
  cat("Linear regression (OLS)\n")
  cat("Data     :", object$dataset, "\n")
  if(object$data_filter %>% gsub("\\s","",.) != "")
    cat("Filter   :", gsub("\\n","", object$data_filter), "\n")
  cat("Dependent variable   :", object$reg_dep_var, "\n")
  cat("Independent variables:", paste0(object$reg_indep_var, collapse=", "), "\n")
  if("standardize" %in% object$reg_check)
 		cat("Standardized coefficients shown\n")
 	cat("\n")
  # cat("Null hyp.: variables x and y are not correlated\n")
  # cat("Alt. hyp.: variables x and y are correlated\n\n")
	print(object$reg_coeff, row.names=FALSE)

  if(nrow(object$model$model) <= (length(object$reg_indep_var) + 1))
    return("\nInsufficient observations to estimate model")

	reg_fit <- glance(object$model) %>% round(3)
	if(reg_fit['p.value'] < .001) reg_fit['p.value'] <- "< .001"
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("R-squared:", paste0(reg_fit$r.squared, ", "), "Adjusted R-squared:", reg_fit$adj.r.squared, "\n")
  cat("F-statistic:", reg_fit$statistic, paste0("df(", reg_fit$df, ",", reg_fit$df.residual, "), p.value"), reg_fit$p.value)
  cat(paste0("\nNr obs: ", reg_fit$df + reg_fit$df.residual))
  cat("\n\n")

  if("rmse" %in% reg_sum_check) {
    mean(object$model$residual^2, na.rm=TRUE) %>% sqrt %>%
    cat("Prediction error (RMSE): ", ., "\n\n")
  }

  if("sumsquares" %in% reg_sum_check) {
    atab <- anova(object$model)
    nr_rows <- dim(atab)[1]
    df_reg <- sum(atab$Df[-nr_rows])
    df_err <- sum(atab$Df[nr_rows])
    df_tot <- df_reg + df_err

    ss_reg <- sum(atab$`Sum Sq`[-nr_rows])
    ss_err <- sum(atab$`Sum Sq`[nr_rows])
    ss_tot <- ss_reg + ss_err
    ss_tab <- data.frame(matrix(nrow = 3, ncol = 2))
    rownames(ss_tab) <- c("Regression","Error","Total")
    colnames(ss_tab) <- c("df","SS")
    ss_tab$df <- c(df_reg,df_err,df_tot)
    ss_tab$SS <- c(ss_reg,ss_err,ss_tot)
    cat("Sum of squares:\n")
    format(ss_tab, scientific = FALSE) %>% print
    cat("\n")
  }

  if("vif" %in% reg_sum_check) {
    if(anyNA(object$model$coeff)) {
			cat("The set of independent variables exhibit perfect multi-collinearity.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
		} else {
      if(length(object$reg_indep_var) > 1) {
        cat("Variance Inflation Factors\n")
        vif(object$model) %>%
          { if(!dim(.) %>% is.null) .[,"GVIF"] else . } %>% # needed when factors are included
          data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
          round(3) %>%
          .[order(.$VIF, decreasing=T),] %>%
          { if(nrow(.) < 8) t(.) else . } %>%
          print
      } else {
        cat("Insufficient number of independent variables selected to calculate\nmulti-collinearity diagnostics")
      }
		}
    cat("\n")
  }

  if("confint" %in% reg_sum_check) {
    if(anyNA(object$model$coeff)) {
      cat("There is perfect multi-collineary in the set of independent variables.\nOne or more variables were dropped from the estimation. Confidence\nintervals were not calculated.\n")
    } else {

      cl_split <- function(x) 100*(1-x)/2
      cl_split(reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
      (100 - cl_split(reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

      confint(object$model, level = reg_conf_level) %>%
        as.data.frame %>%
        magrittr::set_colnames(c("Low","High")) %>%
        cbind(select(object$reg_coeff,2),.) %>%
        round(3) %>%
        set_rownames(object$reg_coeff$`  `) %T>%
        { .$`+/-` <- (.$High - .$coefficient) } %>%
        magrittr::set_colnames(c("coefficient", cl_low, cl_high, "+/-")) %>%
        print
      cat("\n")
    }
  }

  if(!is.null(reg_test_var) && reg_test_var[1] != "") {
		if("stepwise" %in% object$reg_check) {
	  	cat("Model comparisons are not conducted when Stepwise has been selected.\n")
	  } else {
			sub_formula <- ". ~ 1"

			vars <- object$reg_indep_var
      if(object$reg_int_var != "" && length(vars) > 1) {
        # updating reg_test_var if needed
        reg_test_var <- test_check(reg_test_var, object$reg_int_var)
	 	    vars <- c(vars,object$reg_int_var)
			}

			not_selected <- setdiff(vars,reg_test_var)
			if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))
      sub_mod <- update(object$model, sub_formula, data = object$model$model) %>%
                   anova(object$model, test='F')

			if(sub_mod[,"Pr(>F)"][2] %>% is.na) return(cat(""))
      p.value <- sub_mod[,"Pr(>F)"][2] %>% { if(. < .001) "< .001" else round(.,3) }

      # cat("\n")
			cat(attr(sub_mod,"heading")[2])
				object$model$model[,1] %>%
				{ sum((. - mean(.))^2) } %>%
				{1 - (sub_mod$RSS / .)} %>%
				round(3) %>%
				cat("\nR-squared, Model 1 vs 2:", .)
			cat("\nF-statistic:", sub_mod$F[2] %>% round(3), paste0("df(", sub_mod$Res.Df[1], ",", sub_mod$Res.Df[2], "), p.value ", p.value))
	  }
	}
}

#' Plot method for regression
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{regression}}
#' @param reg_plots Regression plots to produce for the specified regression model. Enter "" to avoid showing any plots (default). "hist" to show histograms of all variables in the model. "correlations" for a visual representation of the correlation matrix selected variables. "scatter" to show scatter plots (or box plots for factors) for the dependent variables with each independent variable. "dashboard" for a series of six plots that can be used to evaluate model fit visually. "resid_pred" to plot the independent variables against the model residuals. "coef" for a coefficient plot with adjustable confidence intervals. "leverage" to show leverage plots for each independent variable
#' @param reg_lines Optional lines to include in the select plot. "line" to include a line through a scatter plot. "loess" to include a polynomial regression fit line. To include both use c("line","loess")
#' @param reg_conf_level Confidence level used to estimate confidence intervals (.95 is the default)
#' @param reg_coef_int Include the intercept in the coefficient plot (TRUE, FALSE). FALSE is the default
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' plot(result, reg_plots = "dashboard")
#' plot(result, reg_plots = "dashboard", reg_lines = c("line","loess"))
#' plot(result, reg_plots = "coef", reg_coef_int = TRUE)
#' plot(result, reg_plots = "coef", reg_conf_level = .99, reg_coef_int = TRUE)
#' plot(result, reg_plots = "hist")
#' plot(result, reg_plots = "scatter", reg_lines = c("line","loess"))
#' plot(result, reg_plots = "correlations")
#' plot(result, reg_plots = "leverage")
#' plot(result, reg_plots = "resid_pred", reg_lines = "line")
#'
#' @seealso \code{\link{regression}} to generate the results
#' @seealso \code{\link{summary.regression}} to summarize results
#' @seealso \code{\link{predict.regression}} to generate predictions
#'
#' @importFrom car leveragePlots
#'
#' @export
plot.regression <- function(x,
                            reg_plots = "",
                            reg_lines = "",
                            reg_conf_level = .95,
                            reg_coef_int = FALSE,
                            ...) {

  object <- x; rm(x)

	if(class(object$model)[1] != 'lm') return(object)

  if(reg_plots[1] == "")
    return(cat("Please select a regression plot from the drop-down menu"))

  # no plots if aliased coefficients present
  if(anyNA(object$model$coeff)) reg_plots <- return("")

  # object_size(object$model, model)
	model <- ggplot2::fortify(object$model)

	reg_dep_var <- object$reg_dep_var
	reg_indep_var <- object$reg_indep_var
  vars <- c(reg_dep_var, reg_indep_var)

  plots <- list()
	if("hist" %in% reg_plots)
		for(i in vars) plots[[paste0("hist",i)]] <- ggplot(model[,vars], aes_string(x = i)) + geom_histogram()

	if("dashboard" %in% reg_plots) {

		plots[[1]] <- ggplot(model, aes_string(x=".fitted", y=reg_dep_var)) +
      labs(list(title = "Actual vs Fitted values", x = "Fitted", y = "Actual")) +
      geom_point(alpha = .5)

    plots[[2]] <- ggplot(model, aes_string(x=".fitted", y='.resid')) +
      labs(list(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")) +
      geom_point(alpha = .5)

		plots[[3]] <- ggplot(model, aes(y=.resid, x=seq_along(.resid))) + geom_line() +
			labs(list(title = "Residuals vs Row order", x = "Row order", y = "Residuals"))

    plots[[4]] <- ggplot(model, aes_string(sample=".stdresid")) + stat_qq(alpha = .5) +
      labs(list(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Standardized residuals"))

  	plots[[5]] <- ggplot(model, aes_string(x = ".resid")) + geom_histogram() +
      labs(list(title = "Histogram of residuals", x = "Residuals"))

    plots[[6]] <- ggplot(model, aes_string(x=".resid")) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(model[,'.resid']), sd = sd(model[,'.resid'])), color = "blue") +
  		labs(list(title = "Residual vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())

    if("loess" %in% reg_lines)
      for(i in 1:3) plots[[i]] <- plots[[i]] + geom_smooth(size = .75, linetype = "dotdash")

    if("line" %in% reg_lines) {
      for(i in c(1,4))
        plots[[i]] <- plots[[i]] + geom_abline(linetype = 'dotdash')
      for(i in 2:3)
        plots[[i]] <- plots[[i]] + geom_smooth(method = "lm", se = FALSE, size = .75, linetype = "dotdash", colour = 'black')
    }
	}

	if("scatter" %in% reg_plots) {
		for(i in reg_indep_var) {
			if('factor' %in% class(model[,i])) {
				plots[[paste0("scatter",i)]] <- ggplot(model, aes_string(x=i, y=reg_dep_var, fill=i)) +
                                          geom_boxplot(alpha = .7) +
                                          theme(legend.position = "none")
			} else {
				p <- ggplot(model, aes_string(x=i, y=reg_dep_var)) + geom_point()
        if("line" %in% reg_lines) p <- p + geom_smooth(method = "lm", se = FALSE, size = .75, linetype = "dotdash", colour = 'black')
        if("loess" %in% reg_lines) p <- p + geom_smooth(size = .75, linetype = "dotdash")
        plots[[paste0("scatter",i)]] <- p
			}
		}
	}

	if("resid_pred" %in% reg_plots) {
		for(i in reg_indep_var) {
			if('factor' %in% class(model[,i])) {
				plots[[i]] <- ggplot(model, aes_string(x=i, y=".resid")) +
                        geom_boxplot(fill = 'blue', alpha = .7) +
                        ylab("residuals") + theme(legend.position = "none")
			} else {
				p <- ggplot(model, aes_string(x=i, y=".resid")) + geom_point(alpha = .5) + ylab("residuals")
        if("line" %in% reg_lines)
          p <- p + geom_smooth(method = "lm", se = FALSE, size = .75, linetype = "dotdash", colour = 'black')
        if("loess" %in% reg_lines)
          p <- p + geom_smooth(size = .75, linetype = "dotdash")
        plots[[i]] <- p
			}
		}
	}

  if("coef" %in% reg_plots) {
    if(!anyNA(object$model$coeff)) {
      cl_split <- function(x) 100*(1-x)/2
      cl_split(reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
      (100 - cl_split(reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

      confint(object$model, level = reg_conf_level) %>%
        data.frame %>%
        magrittr::set_colnames(c("Low","High")) %>%
        cbind(select(object$reg_coeff,2),.) %>%
        round(3) %>%
        set_rownames(object$reg_coeff$`  `) %>%
        { if(!reg_coef_int) .[-1,] else . } %>%
        mutate(variable = rownames(.)) %>%
          ggplot() +
            geom_pointrange(aes_string(x = "variable", y = "coefficient",
                            ymin = "Low", ymax = "High")) +
            geom_hline(yintercept = 0, linetype = 'dotdash', color = "blue") + coord_flip() -> p
            return(p)
            # -> plots[["coef"]]
    }
	}

  if("correlations" %in% reg_plots)
    return(plot.correlation(object$model$model))

  if("leverage" %in% reg_plots)
    return(leveragePlots(object$model, main = "", ask=FALSE, id.n = 1, layout = c(ceiling(length(reg_indep_var)/2),2)))

	if(exists("plots"))
		sshh( do.call(grid.arrange, c(plots, list(ncol = 2))) )
}

#' Predict method for regression
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regression}}
#' @param reg_predict_cmd Command used to generate data for prediction
#' @param reg_predict_data Name of the dataset to use for prediction
#' @param reg_conf_level Confidence level used to estimate confidence intervals (.95 is the default)
#' @param reg_save_pred Save predicted values to a csv file
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' predict(result, reg_predict_cmd = "carat = 1:10")
#' predict(result, reg_predict_cmd = "clarity = levels(clarity)")
#' result <- regression("diamonds", "price", c("carat","clarity"), reg_int_var = c("carat:clarity"))
#' predict(result, reg_predict_data = "diamonds")
#'
#' @seealso \code{\link{regression}} to generate the result
#' @seealso \code{\link{summary.regression}} to summarize results
#' @seealso \code{\link{plot.regression}} to plot results
#'
#' @export
predict.regression <- function(object,
                               reg_predict_cmd = "",
                               reg_predict_data = "",
                               reg_conf_level = 0.95,
                               reg_save_pred = FALSE,
                               ...) {

  # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression as starting point
  if ("standardize" %in% object$reg_check) {
    return(cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the standardized coefficients box and try again"))
  } else if (reg_predict_cmd == "" && reg_predict_data == "") {
    return(cat("Please specify a command to generate predictions. For example,\ncarat = seq(.5, 1.5, .1) would produce predictions for values of\ncarat starting at .5, increasing to 1.5 in increments of .1. \nMake sure to press CTRL-return (CMD-return on mac) after you finish entering the command.\nIf no results are shown the command was likely invalid\nAlternatively specify a dataset to generate predictions. You could create this in Excel\nand use the paste feature in Data > Manage to bring it into Radiant"))
  }

  if (reg_predict_cmd != "" && reg_predict_data != "")
    cat("Both a command and a dataset where specified for prediciton. The command will be used.\nTo use the dataset remove the command.")

  reg_predict_type <- "cmd"
  vars <- object$reg_indep_var
  if(reg_predict_cmd != "") {
    reg_predict_cmd %<>% gsub("\"","\'", .)
    pred_df <- try(eval(parse(text = paste0("with(object$model$model, expand.grid(", reg_predict_cmd ,"))"))), silent = TRUE)
    if(is(pred_df, 'try-error')) {
      return(cat("The command entered did not generate valid data for prediction. Please try again.\nExamples are shown in the helpfile.\n"))
    }

    # adding information to the prediction data.frame
    dat_classes <- attr(object$model$term, "dataClasses")[-1]
    isFct <- dat_classes == "factor"
    isNum <- dat_classes == "numeric"
    dat <- select_(object$model$model, .dots = vars)

    # based on http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
    max_freq <- function(x) names(which.max(table(x)))

    plug_data <- data.frame(init___ = 1)
    if(sum(isNum) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(mean), vars[isNum]))
    if(sum(isFct) > 0)
      plug_data %<>% bind_cols(., summarise_each_(dat, funs(max_freq), vars[isFct]))

    if(sum(isNum) + sum(isFct) < length(vars)) {
      cat("The model includes data-types that cannot be used for\nprediction at this point\n")
    } else {
      if(sum(names(pred_df) %in% names(plug_data)) < length(names(pred_df))) {
        return(cat("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n"))
      } else {
        plug_data[names(pred_df)] <- list(NULL)
        pred_df <- data.frame(plug_data[-1],pred_df)
      }
    }
  } else {
    pred_df <- getdata(reg_predict_data)
    pred_names <- names(pred_df)
    pred_df <- try(select_(pred_df, .dots = vars), silent = TRUE)
    if(is(pred_df, 'try-error')) {
      cat("Model variables: ")
      cat(vars,"\n")
      cat("Profile variables to be added: ")
      cat(vars[!vars %in% pred_names])
      return()
    }
    reg_predict_type <- "data"
  }

  pred <- try(predict(object$model, pred_df, interval = 'prediction', level = reg_conf_level), silent = TRUE)
  if(!is(pred, 'try-error')) {
    pred %<>% data.frame %>% mutate(diff = .[,3] - .[,1])
    cl_split <- function(x) 100*(1-x)/2
    cl_split(reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
    (100 - cl_split(reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high
    colnames(pred) <- c("Prediction",cl_low,cl_high,"+/-")

    pred_df <- data.frame(pred_df, pred, check.names = FALSE)

    # return predicted values
    if(reg_save_pred) return(pred_df)

    if(reg_predict_type == "cmd") {
      cat("Predicted values for:\n")
    } else {
      cat(paste0("Predicted values for profiles from dataset: ",object$reg_predict_data,"\n"))
    }

    pred_df %>% print(., row.names = FALSE)

    # pushing predictions into the clipboard
    os_type <- Sys.info()["sysname"]
    if (os_type == 'Windows') {
      write.table(pred_df, "clipboard", sep="\t", row.names=FALSE)
    } else if (os_type == "Darwin") {
      write.table(pred_df, file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
    }
    if (os_type != "Linux")
      cat("\nPredictions were pushed to the clipboard. You can paste them in Excel or\nuse Manage > Data to paste the predictions as a new dataset.\n\n")

  } else {
    cat("The expression entered does not seem to be correct. Please try again.\nExamples are shown in the helpfile.\n")
  }
}

#' Save regression residuals
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{regression}}
#'
#' @examples
#' \dontrun{
#' result <- regression("diamonds", "price", c("carat","clarity"))
#' save_reg_resid(result)
#' }
#' @export
save_reg_resid <- function(object) {
  if(object$data_filter != "")
    return("Please deactivate data filters before trying to save residuals")
  object$model$residuals %>%
    changedata(object$dataset, vars = ., var_names = "reg_residuals")
}

#' Check if main effects for all interaction effects are included in the model
#' If ':' is used to select a range _indep_var_ is updated
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param indep_var List of independent variables provided to _regression_ or _glm_
#' @param cn Column names for all independent variables in _dat_
#' @param int_var Interaction terms specified
#'
#' @return 'vars' is a vector of right-hand side variables, possibly with interactions, 'indep_var' is the list of indepdent variables, and int_var are interaction terms
#'
#' @examples
#' var_check("a:d", c("a","b","c","d"))
#' var_check(c("a","b"), c("a","b"), "a:c")
#'
#' @export
var_check <- function(indep_var, cn, int_var = "") {

  # if : is used to select a range of variables reg_indep_var is updated
  vars <- indep_var
  if(length(vars) < length(cn)) vars <- indep_var <- cn

  if(int_var != "" && length(vars) > 1) {
    if({int_var %>% strsplit(":") %>% unlist} %in% vars %>% all) {
      vars <- c(vars, int_var)
    } else{
      cat("Interaction terms contain variables not selected as main effects.\nRemoving all interactions from the estimation")
      int_var <- ""
    }
  }

  list(vars = vars, indep_var = indep_var, int_var = int_var)
}

#' Add interaction terms to list of test variables if needed
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/regression.html} for an example in Radiant
#'
#' @param test_var List of variables to use for testing for _regression_ or _glm_
#' @param int_var Interaction terms specified
#'
#' @return 'test_var' is a vector of variables to test
#'
#' @examples
#' test_check("a", c("a:b","b:c"))
#'
#' @export
test_check <- function(test_var, int_var) {

  if({int_var %>% strsplit(":") %>% unlist} %in% test_var %>% any) {
    cat("Interaction terms contain variables specified for testing.\nRelevant interaction terms are included in the requested test.\n\n")

    for(i in test_var) {
      ind <- grep(i, int_var)
      test_var <- c(test_var, int_var[ind])
    }

    test_var <- unique(test_var)
  }
  test_var
}
