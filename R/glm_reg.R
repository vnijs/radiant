#' Generalized linear models (GLM)
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param glm_dep_var The dependent variable in the logit (probit) model
#' @param glm_indep_var Independent variables in the model
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param glm_levels The level in the dependent variable defined as _success_
#' @param glm_link Link function for _glm_ ('logit' or 'probit'). 'logit' is the default
#' @param glm_int_var Interaction term to include in the model (not implement)
#' @param glm_check Optional output or estimation parameters. "vif" to show the multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates. "odds" to show odds ratios and confidence interval estimates. "standardize" to output standardized coefficient estimates. "stepwise" to apply step-wise selection of variables
#'
#' @return A list with all variables defined in glm_reg as an object of class glm_reg
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), glm_levels = "Yes")
#'
#' @seealso \code{\link{summary.glm_reg}} to summarize the results
#' @seealso \code{\link{plot.glm_reg}} to plot the results
#' @seealso \code{\link{predict.glm_reg}} to generate predictions
#'
#' @export
glm_reg <- function(dataset, glm_dep_var, glm_indep_var,
                    data_filter = "",
                    glm_levels = "",
                    glm_link = "logit",
                    glm_int_var = "",
                    glm_check = "") {

	dat <- getdata(dataset, c(glm_dep_var, glm_indep_var), filt = data_filter)

  if(glm_levels == "")
    glm_levels <- dat[,glm_dep_var] %>% as.character %>% as.factor %>% levels(.) %>% .[1]

  # transformation
  glm_dv <- dat[,glm_dep_var]
  dat[,glm_dep_var] <- dat[,glm_dep_var] == glm_levels

  vars <- ""
  var_check(glm_indep_var, colnames(dat)[-1], glm_int_var) %>%
    { vars <<- .$vars; glm_indep_var <<- .$indep_var; glm_int_var <<- .$int_var }

	if("standardize" %in% glm_check) {
    isNum <- sapply(dat, is.numeric)
    if(sum(isNum > 0)) dat[,isNum] %<>% data.frame %>% mutate_each(funs(scale))
  }

  formula <- paste(glm_dep_var, "~", paste(vars, collapse = " + ")) %>% as.formula

	if("stepwise" %in% glm_check) {
    # use k = 2 for AIC, use k = log(nrow(dat)) for BIC
		model <- glm(paste(glm_dep_var, "~ 1") %>% as.formula,
		             family = binomial(link = glm_link), data = dat) %>%
      			 step(k = 2, scope = list(upper = formula), direction = 'both')
	} else {
		model <- glm(formula, family = binomial(link = glm_link), data = dat)
	}

  glm_coeff <- tidy(model)
  glm_coeff$` ` <- sig_stars(glm_coeff$p.value)
  glm_coeff[,c(2:5)] %<>% round(3)
  glm_coeff$p.value[glm_coeff$p.value < .001] <- "< .001"
  colnames(glm_coeff) <- c("  ","coefficient","std.error","z.value","p.value"," ")

  isFct <- sapply(select(dat,-1), is.factor)
  if(sum(isFct) > 0) {
    for(i in names(select(dat,-1)[isFct]))
      glm_coeff$`  ` %<>% gsub(i, paste0(i," > "), .)

    rm(i, isFct)
  }

  # dat not needed elsewhere
  rm(dat)

  environment() %>% as.list %>% set_class(c("glm_reg",class(.)))
}

#' Summary method for glm_reg
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{glm_reg}}
#' @param glm_sum_check Optional output or estimation parameters. "rsme" to show the root mean squared error. "sumsquares" to show the sum of squares table. "vif" to show multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates.
#' @param glm_conf_level Confidence level to use for coefficient and odds confidence intervals (.95 is the default)
#' @param glm_test_var Variables to evaluate in model comparison (i.e., a competing models Chi-squared test)
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- glm_reg("titanic", "survived", "pclass", glm_levels = "Yes")
#' summary(result, glm_test_var = "pclass")
#' res <- glm_reg("titanic", "survived", c("pclass","sex"), glm_int_var="pclass:sex", glm_levels="Yes")
#' summary(res, glm_sum_check = c("vif","confint","odds"))
#'
#' @seealso \code{\link{glm_reg}} to generate the results
#' @seealso \code{\link{plot.glm_reg}} to plot the results
#' @seealso \code{\link{predict.glm_reg}} to generate predictions
#'
#' @importFrom car vif
#'
#' @export
summary.glm_reg <- function(object,
                            glm_sum_check = "",
                            glm_conf_level = .95,
                            glm_test_var = "",
                            ...) {

	if(class(object$model)[1] != 'glm') return(object)

  cat("Generalized linear model (glm)")
  cat("\nLink function:", object$glm_link)
  cat("\nData         :", object$dataset)
  if(object$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter       :", gsub("\\n","", object$data_filter))
  cat("\nDependent variable   :", object$glm_dep_var)
  cat("\nLevel                :", object$glm_levels, "in", object$glm_dep_var)
  cat("\nIndependent variables:", paste0(object$glm_indep_var, collapse=", "))
  if("standardize" %in% object$glm_check)
 		cat("\nStandardized coefficients shown")
 	cat("\n\n")
	print(object$glm_coeff, row.names=FALSE)

  glm_fit <- glance(object$model)

	# pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_regression
	glm_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>% round(3)

	# chi-squared test of overall model fit (p-value) - http://www.ats.ucla.edu/stat/r/dae/logit.htm
	chi_pval <- with(object$model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
	chi_pval %<>% { if(. < .001) "< .001" else round(.,3) }

  cat("\nPseudo R-squared:", glm_fit$r2)
  cat(paste0("\nLog-likelihood: ", glm_fit$logLik, ", AIC: ", glm_fit$AIC, ", BIC: ", glm_fit$BIC))
  cat(paste0("\nChi-squared: ", with(glm_fit, null.deviance - deviance) %>% round(3), " df(",
         with(glm_fit, df.null - df.residual), "), p.value ", chi_pval), "\n")
  cat("Nr obs: ", glm_fit$df.null + 1, "\n\n")

  if("vif" %in% glm_sum_check) {
    if(anyNA(object$model$coeff)) {
      cat("The set of independent variables exhibit perfect multi-collinearity.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
    } else {
      if(length(object$glm_indep_var) > 1) {
        cat("Variance Inflation Factors\n")
        vif(object$model) %>%
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

  if(c("confint","odds") %in% glm_sum_check %>% any) {
    if(object$model$coeff %>% is.na %>% any) {
      cat("There is perfect multi-collineary in the set of independent variables.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
    } else {
      cl_split <- function(x) 100*(1-x)/2
      cl_split(glm_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
      (100 - cl_split(glm_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

      confint(object$model, level = glm_conf_level) %>%
        as.data.frame %>%
        magrittr::set_colnames(c("Low","High")) %>%
        cbind(select(object$glm_coeff,2),.) %>%
        set_rownames(object$glm_coeff$`  `) -> ci_tab

      if("confint" %in% glm_sum_check) {
        ci_tab %>% round(3) %T>%
        # set_rownames(object$glm_coeff$`  `) %T>%
        { .$`+/-` <- (.$High - .$coefficient) } %>%
        magrittr::set_colnames(c("coefficient", cl_low, cl_high, "+/-")) %>%
        print
        cat("\n")
      }
    }
  }

  if("odds" %in% glm_sum_check) {
    if(object$model$coeff %>% is.na %>% any) {
      cat("There is perfect multi-collinearity in the set of selected independent variables.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
    } else {
      odds_tab <- exp(ci_tab) %>% round(3)
      odds_tab$`+/-` <- (odds_tab$High - odds_tab$Low)
      odds_tab %>%
        magrittr::set_colnames(c("odds", cl_low, cl_high, "+/-")) %>%
        print
      cat("\n")
    }
  }

  if(!is.null(glm_test_var) && glm_test_var[1] != "") {
    if("stepwise" %in% object$glm_check) {
      cat("Model comparisons are not conducted when Stepwise has been selected.\n")
    } else {
      # sub_formula <- ". ~ 1"
      sub_formula <- paste(object$glm_dep_var, "~ 1")

      vars <- object$glm_indep_var
      if(object$glm_int_var != "" && length(vars) > 1) {
        # updating glm_test_var if needed
        glm_test_var <- test_check(glm_test_var, object$glm_int_var)
        vars <- c(vars,object$glm_int_var)
      }

      # glm_test_var <- "pclass"
  		not_selected <- setdiff(vars, glm_test_var)
      if(length(not_selected) > 0) sub_formula <- paste(object$glm_dep_var, "~", paste(not_selected, collapse = " + "))
      #### update with glm_sub NOT working when called from radiant - strange
  		# glm_sub <- update(object$model, sub_formula, data = object$model$model)
      glm_sub <- glm(sub_formula, family = binomial(link = object$glm_link), data = object$model$model)
  		glm_sub_fit <- glance(glm_sub)
  		glm_sub <- anova(glm_sub, object$model, test='Chi')

  		# pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_regression
  		glm_sub_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>% round(3)
  		glm_sub_pval <- glm_sub[,"Pr(>Chi)"][2] %>% { if(. < .001) "< .001" else round(.3) }
  		cat(attr(glm_sub,"heading")[2])
  		cat("\nPseudo R-squared, Model 1 vs 2:", c(glm_sub_fit$r2, glm_fit$r2))
  		cat(paste0("\nChi-statistic: ", glm_sub$Deviance[2] %>% round(3), " df(", glm_sub$Df[2], "), p.value ", glm_sub_pval))
    }
	}
}

#' Plot method for glm_reg
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param x Return value from \code{\link{glm_reg}}
#' @param glm_plots Plots to produce for the specified GLM model. Use "" to avoid showing any plots (default). "hist" shows histograms of all variables in the model. "scatter" shows scatter plots (or box plots for factors) for the dependent variable with each independent variable. "dashboard" is a series of four plots used to visually evaluate model. "coef" provides a coefficient plot
#' @param glm_conf_level Confidence level to use for coefficient and odds confidence intervals (.95 is the default)
#' @param glm_coef_int Include the intercept in the coefficient plot (TRUE or FALSE). FALSE is the default
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), glm_levels = "Yes")
#' plot(result, glm_plots = "coef")
#'
#' @seealso \code{\link{glm_reg}} to generate results
#' @seealso \code{\link{plot.glm_reg}} to plot results
#' @seealso \code{\link{predict.glm_reg}} to generate predictions
#'
#' @export
plot.glm_reg <- function(x,
                         glm_plots = "",
                         glm_conf_level = .95,
                         glm_coef_int = FALSE,
                         ...) {

  object <- x; rm(x)

	if(class(object$model)[1] != 'glm') return(object)

  if(glm_plots[1] == "")
    return(cat("Please select a glm regression plot from the drop-down menu"))

  # no plots if aliased coefficients present
  if(anyNA(object$model$coeff)) glm_plots <- return("")

	model <- ggplot2::fortify(object$model)
	model$.fitted <- predict(object$model, type = 'response')
  model$.actual <- as.numeric(object$glm_dv)
	model$.actual <- model$.actual - max(model$.actual) + 1 	# adjustment in case max > 1

  glm_dep_var <- object$glm_dep_var
  glm_indep_var <- object$glm_indep_var
  vars <- c(object$glm_dep_var, object$glm_indep_var)
	nrCol <- 2
	plots <- list()

	if("hist" %in% glm_plots)
    for(i in vars) plots[[i]] <- ggplot(model, aes_string(x = i)) + geom_histogram()

  if("coef" %in% glm_plots) {
    p <- confint(object$model, level = glm_conf_level) %>%
          data.frame %>%
          magrittr::set_colnames(c("Low","High")) %>%
          cbind(select(object$glm_coeff,2),.) %>%
          set_rownames(object$glm_coeff$`  `) %>%
          { if(!glm_coef_int) .[-1,] else . } %>%
          mutate(variable = rownames(.)) %>% ggplot() +
          geom_pointrange(aes_string(x = "variable", y = "coefficient",
                          ymin = "Low", ymax = "High")) +
          geom_hline(yintercept = 0, linetype = 'dotdash', color = "blue") + coord_flip()
          return(p)
  }

  if (glm_plots == "scatter") {
		for(i in glm_indep_var) {
      if('factor' %in% class(model[,i])) {
        plots[[i]] <- ggplot(model, aes_string(x=i, fill=glm_dep_var)) +
                        geom_bar(position = "fill", alpha=.7) +
                        labs(list(y = ""))
			} else {
        plots[[i]] <- ggplot(model, aes_string(x=glm_dep_var, y=i, fill=glm_dep_var)) +
                        geom_boxplot(alpha = .7) + theme(legend.position = "none")
			}
		}
		nrCol <- 1
	}

  if (glm_plots == "dashboard") {
		plots[[1]] <- ggplot(model, aes_string(x=".fitted", y=".actual")) + geom_point(alpha = .25) +
					 stat_smooth(method="glm", family="binomial", se=TRUE) +
					 geom_jitter(position = position_jitter(height = .05)) +
					 labs(list(title = "Actual vs Fitted values", x = "Fitted values", y = "Actual"))

		plots[[2]] <- ggplot(model, aes_string(x=".fitted", y=".resid")) + geom_point(alpha = .25) +
					 geom_hline(yintercept = 0) + geom_smooth(size = .75, linetype = "dotdash", se = TRUE) +
					 labs(list(title = "Residuals vs Fitted values", x = "Fitted", y = "Residuals"))

  	plots[[3]] <- ggplot(model, aes_string(x = ".resid")) + geom_histogram(binwidth = .5) +
      labs(list(title = "Histogram of residuals", x = "Residuals"))

    plots[[4]] <- ggplot(model, aes_string(x=".resid")) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(model[,".resid"]), sd = sd(model[,".resid"])), color = "blue") +
  		labs(list(title = "Residual vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())
	}

	if(length(plots) > 0) sshh( do.call(grid.arrange, c(plots, list(ncol = nrCol))) )
}

#' Predict method for glm
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm_reg} for an example in Radiant
#'
#' @param object Return value from \code{\link{regression}}
#' @param glm_predict_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable use a `,` (e.g., `pclass = levels(pclass), age = seq(0,100,20)`)
#' @param glm_predict_data Provide the name of a dataframe to generate predictions (e.g., "titanic"). The dataset must contain all columns used in the estimation
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), glm_levels = "Yes")
#' pred <- predict(result, glm_predict_cmd = "pclass = levels(pclass)")
#' str(pred)
#' names(pred)
#' predict(result, glm_predict_cmd = "sex = c('male','female')")
#'
#' @seealso \code{\link{glm_reg}} to generate the result
#' @seealso \code{\link{summary.glm_reg}} to summarize results
#' @seealso \code{\link{plot.glm_reg}} to plot results
#'
#' @export
predict.glm_reg <- function(object,
                            glm_predict_cmd = "",
                            glm_predict_data = "",
                            ...) {

  # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression as starting point
  if ("standardize" %in% object$glm_check) {
    return(cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the standardized coefficients box and try again"))
  } else if (glm_predict_cmd == "" && glm_predict_data == "") {
    return(cat("Please specify a command to generate predictions. For example,\n pclass = levels(pclass) would produce predictions for the different\n levels of factor pclass. To add another variable use a ,\n(e.g., pclass = levels(pclass), age = seq(0,100,20))\nMake sure to press CTRL-return (CMD-return on mac) after you finish entering the command.\nIf no results are shown the command was likely invalid\nAlternatively specify a dataset to generate predictions. You could create this in Excel\nand use the paste feature in Data > Manage to bring it into Radiant"))
  }

  if (glm_predict_cmd != "" && glm_predict_data != "")
    cat("Both a command and a dataset where specified for prediciton. The command will be used.\nTo use the dataset remove the command.")

  glm_predict_type <- "cmd"
  vars <- object$glm_indep_var
  # glm_predict_cmd <- "pclass = levels(pclass)"
  if(glm_predict_cmd != "") {
    glm_predict_cmd %<>% gsub("\"","\'", .)
    pred <- try(eval(parse(text = paste0("with(object$model$model, expand.grid(", glm_predict_cmd ,"))"))), silent = TRUE)
    if(is(pred, 'try-error')) {
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

    rm(dat)

    if(sum(isNum) + sum(isFct) < length(vars)) {
      cat("The model includes data-types that cannot be used for\nprediction at this point\n")
    } else {
      if(sum(names(pred) %in% names(plug_data)) < length(names(pred))) {
        return(cat("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n"))
      } else {
        plug_data[names(pred)] <- list(NULL)
        pred <- data.frame(plug_data[-1],pred)
      }
    }
  } else {
    pred <- getdata(glm_predict_data)
    pred_names <- names(pred)
    pred <- try(select_(pred, .dots = vars), silent = TRUE)
    if(is(pred, 'try-error')) {
      cat("Model variables: ")
      cat(vars,"\n")
      cat("Profile variables to be added: ")
      cat(vars[!vars %in% pred_names])
      return()
    }
    reg_predict_type <- "data"
  }

  pred_val <- try(predict(object$model, pred, type = 'response', se.fit = TRUE), silent = TRUE)
  if(!is(pred_val, 'try-error')) {
    pred_val %<>% data.frame %>% select(1:2)
    colnames(pred_val) <- c("Prediction","std.error")
    pred <- data.frame(pred, pred_val, check.names = FALSE)

    # return predicted values
    # if(glm_save_pred) {
    #   pred %<>% set_class(c("glm_predict",class(.)))
    #   return(pred)
    # }

    if(glm_predict_type == "cmd") {
      cat("Predicted values for:\n")
    } else {
      cat(paste0("Predicted values for profiles from dataset: ",object$glm_predict_data,"\n"))
    }

    pred %>% print(., row.names = FALSE)

    # pushing predictions into the clipboard
    # os_type <- Sys.info()["sysname"]
    # if (os_type == 'Windows') {
    #   write.table(pred, "clipboard", sep="\t", row.names=FALSE)
    # } else if (os_type == "Darwin") {
    #   write.table(pred, file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
    # }
    # if (os_type != "Linux")
    #   cat("\nPredictions were pushed to the clipboard. You can paste them in Excel or\nuse Manage > Data to paste the predictions as a new dataset.\n\n")

    # if(glm_save_pred)
    # return(pred %>% set_class(c("glm_predict",class(.))))
    return(pred %>% set_class(c("glm_predict",class(.))))
  } else {
    cat("The expression entered does not seem to be correct. Please try again.\nExamples are shown in the helpfile.\n")
  }

  return(invisible())
}

#' Plot method for glm_predict (i.e., predict.glm_reg)
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm_reg} for an example in Radiant
#'
#' @param x Return value from \code{\link{predict.glm_reg}}.
#' @param glm_xvar Variable to display along the X-axis of the plot
#' @param glm_facet_row Create vertically arranged subplots for each level of the selected factor variable
#' @param glm_facet_col Create horizontally arranged subplots for each level of the selected factor variable
#' @param glm_color Adds color to a scatter plot to generate a heat map. For a line plot one line is created for each group and each is assigned a different colour
#' @param glm_conf_level Confidence level to use for prediction intervals (.95 is the default). Note that the error bars for predicitions are approximations at this point.
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex","age"), glm_levels = "Yes")
#' pred <- predict(result, glm_predict_cmd = "pclass = levels(pclass)")
#' plot(pred, glm_xvar = "pclass")
#' pred <- predict(result, glm_predict_cmd = "age = 0:100")
#' plot(pred, glm_xvar = "age")
#' pred <- predict(result, glm_predict_cmd = "pclass = levels(pclass), sex = levels(sex)")
#' plot(pred, glm_xvar = "pclass", glm_color = "sex")
#' pred <- predict(result, glm_predict_cmd = "pclass = levels(pclass), age = seq(0,100,20)")
#' plot(pred, glm_xvar = "pclass", glm_color = "age")
#' plot(pred, glm_xvar = "age", glm_color = "pclass")
#' pred <- predict(result, glm_predict_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,20)")
#' plot(pred, glm_xvar = "age", glm_color = "sex", glm_facet_col = "pclass")
#' plot(pred, glm_xvar = "age", glm_color = "pclass", glm_facet_col = "sex")
#' pred <- predict(result, glm_predict_cmd="pclass=levels(pclass), sex=levels(sex), age=seq(0,100,5)")
#' plot(pred, glm_xvar = "age", glm_color = "sex", glm_facet_col = "pclass")
#' plot(pred, glm_xvar = "age", glm_color = "pclass", glm_facet_col = "sex")
#'
#' @seealso \code{\link{glm_reg}} to generate the result
#' @seealso \code{\link{summary.glm_reg}} to summarize results
#' @seealso \code{\link{plot.glm_reg}} to plot results
#'
#' @export
plot.glm_predict <- function(x,
                             glm_xvar = "",
                             glm_facet_row = ".",
                             glm_facet_col = ".",
                             glm_color = "none",
                             glm_conf_level = .95,
                             ...) {

  if(is.null(glm_xvar) || glm_xvar == "") return(invisible())

  object <- x; rm(x)

  object$ymin <- object$Prediction - qnorm(.5 + glm_conf_level/2)*object$std.error
  object$ymax <- object$Prediction + qnorm(.5 + glm_conf_level/2)*object$std.error

  if (glm_color == 'none') {
    p <- ggplot(object, aes_string(x=glm_xvar, y="Prediction")) +
           geom_line(aes(group=1))
  } else {
    p <- ggplot(object, aes_string(x=glm_xvar, y="Prediction", color=glm_color)) +
                geom_line(aes_string(group=glm_color))
  }

  facets <- paste(glm_facet_row, '~', glm_facet_col)
  if (facets != '. ~ .') p <- p + facet_grid(facets)

  if(length(unique(object[[glm_xvar]])) < 10)
    p <- p + geom_pointrange(aes_string(ymin = "ymin", ymax = "ymax"), size=.3)
  else
    p <- p + geom_smooth(aes_string(ymin = "ymin", ymax = "ymax"), stat="identity")

  sshhr( p )
}

#' Save glm residuals
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm_reg.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{glm_reg}}
#'
#' @examples
#' \dontrun{
#' result <- glm_reg("titanic", "survived", "pclass", glm_levels = "Yes")
#' save_glm_resid(result)
#' }
#' @export
save_glm_resid <- function(object) {
  if(object$data_filter != "")
    return("Please deactivate data filters before trying to save residuals")
  object$model$residuals %>%
    changedata(object$dataset, vars = ., var_names = "glm_residuals")
}
