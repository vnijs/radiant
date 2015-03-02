#' Generalized linear models (GLM)
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm.html} for an example in Radiant
#'
#' @param dataset Dataset name (string). This can be a dataframe in the global environment or an element in an r_data list from Radiant
#' @param glm_dep_var The dependent variable in the logit (probit) model
#' @param glm_indep_var Independent variables in the model
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param glm_levels The level in the dependent variable defined as _success_
#' @param glm_test_var Variables to evaluate in model comparison (i.e., a competing models Chi-squared test)
#' @param glm_int_var Interaction term to include in the model (not implement)
#' @param glm_interactions Should interactions be considered? Options are "", 2, and 3. None ("") is the default. To consider 2-way interactions choose 2, and for 2- and 3-way interactions choose 3 (not implemented)
#' @param glm_predict Choose the type of prediction input. Default is no prediction (""). To generate predictions using a data.frame choose ("data"), and to include a command to generate predictions select ("cmd")
#' @param glm_predict_cmd Generate predictions using a command. For example, `pclass = levels(pclass)` would produce predictions for the different levels of factor `pclass`. To add another variable use a `,` (e.g., `pclass = levels(pclass), age = seq(0,100,20)`)
#' @param glm_predict_data Provide the name of a dataframe to generate predictions (e.g., "titanic"). The dataset must contain all columns used in the estimation
#' @param glm_check Optional output or estimation parameters. "vif" to show the multicollinearity diagnostics. "confint" to show coefficient confidence interval estimates. "odds" to show odds ratios and confidence interval estimates. "standardize" to output standardized coefficient estimates. "stepwise" to apply step-wise selection of variables
#' @param glm_conf_level Confidence level to use for coefficient and odds confidence intervals (.95 is the default)
#' @param glm_plots Plots to produce for the specified GLM model. Use "" to avoid showing any plots (default). "hist" shows histograms of all variables in the model. "scatter" shows scatter plots (or box plots for factors) for the dependent variable with each independent variable. "dashboard" is a series of four plots used to visually evaluate model. "coef" provides a coefficient plot
#' @param glm_coef_int Include the intercept in the coefficient plot (TRUE or FALSE). FALSE is the default
#'
#' @return A list with all variables defined in glm_reg as an object of class glm_reg
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), glm_levels = "Yes")
#'
#' @seealso \code{\link{summary.glm_reg}} to summarize the results
#' @seealso \code{\link{plot.glm_reg}} to plot the results
#'
#' @export
glm_reg <- function(dataset, glm_dep_var, glm_indep_var,
                data_filter = "",
                glm_levels = "",
                glm_test_var = "",
                # glm_int_var = "",
                # glm_interactions = "",
                glm_link = "logit",
                glm_predict = "",
                glm_predict_cmd = "",
                glm_predict_data = "",
                glm_predict_out = "",
                glm_check = "",
                glm_conf_level = .95,
                glm_plots = "",
                glm_coef_int = FALSE) {

	vars <- glm_indep_var
	dat <- getdata(dataset, c(glm_dep_var, glm_indep_var), filt = data_filter)

  # levs <- levels(dat[,glm_dep_var])
  # if(glm_levels != "") {
  #   if(glm_levels %in% levs && levs[1] != glm_levels) {
  #     dat[,glm_dep_var] %<>% as.character %>% as.factor %>% relevel(glm_levels)
  #     levs <- levels(dat[,glm_dep_var])
  #   }
  # }
  # glm_levels <- levels(dat[,glm_dep_var])[1]

  if(glm_levels == "")
    glm_levels <- dat[,glm_dep_var] %>% as.character %>% as.factor %>% levels(.) %>% .[1]

  # transformation
  glm_dv <- dat[,glm_dep_var]
  dat[,glm_dep_var] <- dat[,glm_dep_var] == glm_levels

	# if(input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)

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

  if("confint" %in% glm_check || "odds" %in% glm_check || "coef" %in% glm_plots) {
    if(model$coeff %>% is.na %>% any) {
      glm_plots <- ""
    } else {
      cl_split <- function(x) 100*(1-x)/2
      cl_split(glm_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
      (100 - cl_split(glm_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

      suppressMessages(confint(model, level = glm_conf_level)) %>%
        data.frame %>%
        magrittr::set_colnames(c("Low","High")) %>%
        cbind(select(glm_coeff,2),.) %>%
        set_rownames(glm_coeff$`  `) -> ci_tab
    }
    if("odds" %in% glm_check) odds_tab <- exp(ci_tab) %>% round(3)
    ci_tab %<>% round(3)
  }

  nrVars <- length(as.character(attr(model$terms,'variables'))[-1])
	plot_height <- 500
	plot_width <- 650

  if(glm_plots == 'hist') plot_height <- (plot_height / 2) * ceiling(nrVars / 2)
  if(glm_plots == 'scatter') plot_height <- 300 * nrVars
  if(glm_plots == 'coef') plot_height <- 300 + 20 * length(model$coefficients)

  environment() %>% as.list %>% set_class(c("glm_reg",class(.)))
}

#' Summary method for glm_reg
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{glm_reg}}
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), glm_levels = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{glm_reg}} to generate the results
#' @seealso \code{\link{plot.glm_reg}} to plot the results
#'
#' @importFrom car vif
#'
#' @export
summary.glm_reg <- function(result, savepred = FALSE) {

	if(class(result$model)[1] != 'glm') return(result)

  cat("Generalized linear model (glm)")
  cat("\nLink function:", result$glm_link)
  cat("\nData         :", result$dataset)
  if(result$data_filter %>% gsub("\\s","",.) != "")
    cat("\nFilter       :", gsub("\\n","", result$data_filter))
  cat("\nDependent variable   :", result$glm_dep_var)
  cat("\nLevel                :", result$glm_levels, "in", result$glm_dep_var)
  cat("\nIndependent variables:", paste0(result$glm_indep_var, collapse=", "))
  if("standardize" %in% result$glm_check)
 		cat("\nStandardized coefficients shown")
 	cat("\n\n")
	print(result$glm_coeff, row.names=FALSE)

	glm_fit <- glance(result$model)

	# pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_regression
	glm_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>% round(3)

	# chi-squared test of overall model fit (p-value) - http://www.ats.ucla.edu/stat/r/dae/logit.htm
	chi_pval <- with(result$model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
	chi_pval %<>% { if(. < .001) "< .001" else round(.,3) }

  cat("\nPseudo R-squared:", glm_fit$r2)
  cat(paste0("\nLog-likelihood: ", glm_fit$logLik, ", AIC: ", glm_fit$AIC, ", BIC: ", glm_fit$BIC))
  cat(paste0("\nChi-squared: ", with(glm_fit, null.deviance - deviance) %>% round(3), " df(",
         with(glm_fit, df.null - df.residual), "), p.value ", chi_pval), "\n")
  cat("Nr obs: ", nrow(result$dat), "\n\n")

  if("confint" %in% result$glm_check) {
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

  if("odds" %in% result$glm_check) {
    if(result$model$coeff %>% is.na %>% any) {
      cat("There is perfect multi-collinearity in the set of selected independent variables.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
    } else {
    	result$odds_tab
      result$odds_tab$`+/-` <- (result$odds_tab$High - result$odds_tab$Low)
      result$odds_tab %>%
        magrittr::set_colnames(c("odds", result$cl_low, result$cl_high, "+/-")) %>%
        print
      cat("\n")
    }
  }

  if("vif" %in% result$glm_check) {
    if(result$model$coeff %>% is.na %>% any) {
      cat("The set of independent variables exhibit perfect multi-collinearity.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
    } else {
      if(length(result$glm_indep_var) > 1) {
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

	if(result$glm_test_var[1] != "") {
    sub_formula <- paste(result$glm_dep_var, "~ 1")
		vars <- result$glm_indep_var

		# if(!is.null(input$glm_intsel) && input$glm_interactions != 'none') vars <- c(vars,input$glm_intsel)

		not_selected <- setdiff(vars, result$glm_test_var)
    if(length(not_selected) > 0) sub_formula <- paste(result$glm_dep_var, "~", paste(not_selected, collapse = " + "))
    #### update with glm_sub NOT working when called from radiant - strange
		# glm_sub <- update(result$model, sub_formula, data = result$dat)
    glm_sub <- glm(sub_formula, family = binomial(link = result$glm_link), data = result$dat)
		glm_sub_fit <- glance(glm_sub)
		glm_sub <- anova(glm_sub, result$model, test='Chi')

		# pseudo R2 (likelihood ratio) - http://en.wikipedia.org/wiki/Logistic_regression
		glm_sub_fit %<>% mutate(r2 = (null.deviance - deviance) / null.deviance) %>% round(3)
		glm_sub_pval <- glm_sub[,"Pr(>Chi)"][2] %>% { if(. < .001) "< .001" else round(.3) }
		cat(attr(glm_sub,"heading")[2])
		cat("\nPseudo R-squared, Model 1 vs 2:", c(glm_sub_fit$r2, glm_fit$r2))
		cat(paste0("\nChi-statistic: ", glm_sub$Deviance[2] %>% round(3), " df(", glm_sub$Df[2], "), p.value ", glm_sub_pval))
	}

  if(result$glm_predict != "") {

    # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
    # as starting point
    if("standardize" %in% result$glm_check) {
      cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the standardized coefficients box and try again")
    } else if (result$glm_predict == "cmd" && result$glm_predict_cmd == "") {
      cat("Please specify a command to generate predictions. For example,\npclass = levels(pclass) would produce predictions for the different levels of factor pclass. \nMake sure to press Enter after you finish entering the command.\nIf no results are shown the command was invalid.")
    } else if (result$glm_predict == "data" && result$glm_predict_data == "") {
      cat("Please select a dataset to generate predictions. You could create this in Excel\nand use the paste feature in Data > Manage to bring it into Radiant")
    } else {

      if(result$glm_predict == "cmd") {
        glm_predict_cmd <- gsub("\"","\'", result$glm_predict_cmd)
        nval <- try(eval(parse(text = paste0("with(result$dat, expand.grid(", glm_predict_cmd ,"))"))), silent = TRUE)
      } else {
        nval <- getdata(result$glm_predict_data)
        nval_names <- names(nval)
        vars <- as.character(attr(result$model$terms,'variables'))[-1]
        nval <- try(select_(nval, .dots = vars[-1]), silent = TRUE)
      }

      if(is(nval, 'try-error')) {
        if(result$glm_predict == "cmd") {
          cat("The expression entered does not seem to be correct. Please try again.\n")
          cat("Examples are shown in the help file.\n")
        } else {
          cat("The profiles to predict do not contain all variables that are in the model.\n")
          cat("Add variables to the profiles data as needed.\n\n")
          ivars <- vars[-1]
          cat("Model variables: ")
          cat(ivars,"\n")
          cat("Profile variables to be added: ")
          cat(ivars[!ivars %in% nval_names])
        }
      } else {

        dat <- ggplot2::fortify(result$model)
        vars <- as.character(attr(result$model$terms,'variables'))[-1]
        glm_dep_var <- vars[1]
        glm_indep_var <- vars[-1]
        dat <- dat[,glm_indep_var, drop = FALSE]

        isFct <- sapply(dat, is.factor)
        isNum <- sapply(dat, is.numeric)

        if(sum(isNum) + sum(isFct) < dim(dat)[2]) {
          cat("The model includes data-types that cannot be used for\nprediction at this point\n")
        } else {
          newdat <- ""
          if(sum(isNum) > 0)  newdat <- data.frame(newdat,t(colMeans(dat[,isNum, drop = FALSE])))
          # from http://stackoverflow.com/questions/19982938/how-to-find-the-most-frequent-values-across-several-columns-containing-factors
          if(sum(isFct) > 0)  newdat <- data.frame(newdat,t(apply(dat[,isFct, drop = FALSE],2,function(x) names(which.max(table(x))))))

          if(sum(names(nval) %in% names(newdat)) < length(names(nval))) {
            cat("The expression entered contains variable names that are not in the model.\nPlease try again.\n\n")
          } else {
            newdat[names(nval)] <- list(NULL)
            nnd <- data.frame(newdat[-1],nval)
            # pred <- try(predict(result$model, nnd, type = 'response', se.fit = TRUE), silent = TRUE)
            pred <- try(predict(result$model, nnd, type = 'response'), silent = TRUE)

            if(!is(pred, 'try-error')) {
              if(result$glm_predict == "data") {
                cat(paste0("Predicted values for profiles from dataset: ",result$glm_predict_data,"\n"))
              } else {
                cat("Predicted values for:\n")
              }

              nnd <- data.frame(nnd, pred, check.names = FALSE)
              if(savepred) return(nnd)

              # pushing predictions into the clipboard
              if(running_local) {
                os_type <- Sys.info()["sysname"]
                if (os_type == 'Windows') {
                  write.table(nnd, "clipboard", sep="\t", row.names=FALSE)
                } else if (os_type == 'Debian') {
                  write.table(nnd, file = pipe("pbcopy"), row.names = FALSE, sep = '\t')
                }
                if (os_type != 'Linux')
                  cat("\nPredictions were pushed to the clipboard. You can paste them in Excel or\nuse Manage > Data to paste the predictions as a new dataset.\n\n")
              }

              # if(result$glm_predict_out != "") {
              #   r_data[[result$glm_predict_out]] <- nnd
              #   # r_data[[paste0(result$glm_predict_out,"_descr")]] <- ""
              #   r_data[['datasetlist']] %<>%
              #     c(result$glm_predict_out,.) %>%
              #     unique
              #   r_data[['glm_pred_list']] %<>%
              #     c(result$glm_predict_out,.) %>%
              #     unique
              # }

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
}


#' Plot method for glm_reg
#'
#' @details See \url{http://mostly-harmless.github.io/radiant/quant/glm.html} for an example in Radiant
#'
#' @param result Return value from \code{\link{glm_reg}}
#'
#' @examples
#' result <- glm_reg("titanic", "survived", c("pclass","sex"), glm_levels = "Yes", glm_plots = "coef")
#' plot(result)
#'
#' @seealso \code{\link{glm_reg}} to generate results
#' @seealso \code{\link{plot.glm_reg}} to plot results
#'
#' @export
plot.glm_reg <- function(result) {

	if(class(result$model)[1] != 'glm') return(result)

	# if({result$reg_int_var %>% strsplit(":") %>% unlist} %in% result$reg_indep_var %>% all) {
	#	# nothing
  #	} else {
  #    cat("Interaction terms contain variables not selected as\nmain effects. Removing interactions from the estimation")
  #   reg_int_var <- ""
  #	}

	model <- ggplot2::fortify(result$model)
	model$.fitted <- predict(result$model, type = 'response')
  model$.actual <- as.numeric(result$glm_dv)
	model$.actual <- model$.actual - max(model$.actual) + 1 	# adjustment in case max > 1

  glm_dep_var <- result$glm_dep_var
  glm_indep_var <- result$glm_indep_var
  result$dat[,glm_dep_var] <- result$glm_dv
  vars <- c(result$glm_dep_var, result$glm_indep_var)
	nrCol <- 2
	plots <- list()

	if(result$glm_plots == "hist") {

		for(i in vars) plots[[i]] <- ggplot(result$dat, aes_string(x = i)) + geom_histogram()

	} else if(result$glm_plots == "coef") {

		p <- result$ci_tab %>%  { if(!result$glm_coef_int) .[-1,] else . } %>%
    	mutate(variable = rownames(.)) %>% ggplot() +
	    	geom_pointrange(aes(x = variable, y = coefficient, ymin = Low, ymax = High)) +
	      geom_hline(yintercept = 0, linetype = 'dotdash', color = "blue") + coord_flip()
	      return(p)

  } else if (result$glm_plots == "scatter") {
		for(i in glm_indep_var) {
			if('factor' %in% class(result$dat[,i])) {
        plots[[i]] <- ggplot(result$dat, aes_string(x=i, fill=glm_dep_var)) + geom_bar(position = "fill", alpha=.7) +
              labs(list(y = ""))
			} else {
        plots[[i]] <- ggplot(result$dat, aes_string(x=glm_dep_var, y=i, fill=glm_dep_var)) + geom_boxplot(alpha = .7)
			}
		}
		nrCol <- 1
	} else if (result$glm_plots == "dashboard") {
		plots[[1]] <- ggplot(model, aes(x=.fitted, y=.actual)) + geom_point(alpha = .25) +
					 stat_smooth(method="glm", family="binomial", se=TRUE) +
					 geom_jitter(position = position_jitter(height = .05)) +
					 labs(list(title = "Actual vs Fitted values", x = "Fitted values", y = "Actual"))

		plots[[2]] <- ggplot(model, aes(x=.fitted, y=.resid)) + geom_point(alpha = .25) +
					 geom_hline(yintercept = 0) + geom_smooth(size = .75, linetype = "dotdash", se = TRUE) +
					 labs(list(title = "Residuals vs Fitted values", x = "Fitted", y = "Residuals"))

  	plots[[3]] <- ggplot(model, aes(x = .resid)) + geom_histogram(binwidth = .5) +
      labs(list(title = "Histogram of residuals", x = "Residuals"))

    plots[[4]] <- ggplot(model, aes(x=.resid)) + geom_density(alpha=.3, fill = "green") +
      stat_function(fun = dnorm, args = list(mean = mean(model[,'.resid']), sd = sd(model[,'.resid'])), color = "blue") +
  		labs(list(title = "Residual vs Normal density", x = "Residuals", y = "")) + theme(axis.text.y = element_blank())
	}

	if(length(plots) > 0) sshh( do.call(grid.arrange, c(plots, list(ncol = nrCol))) )
}
