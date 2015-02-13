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

################################################################
# Regression
################################################################
# regression <- function(dataset, reg_var1, reg_var2, reg_var3, reg_intsel, reg_interactions,
#                        reg_predict, reg_predict_cmd, reg_predict_data, reg_standardize, reg_sumsquares,
#                        reg_confint, reg_conf_level, reg_rmse, reg_vif, reg_stepwise, reg_plots, reg_line,
#                        reg_loess) {

# reg_var1 = reg_dep_var
# reg_var2 = reg_indep_var
# reg_var3 = reg_test_var
# reg_interactions = reg_intsel
# reg_int_var = reg_interactions
regression <- function(dataset, reg_var1, reg_var2,
                       data_filter = "",
                       show_filter = FALSE, 		# remove when recode is complete
                       reg_var3 = "",
                       reg_intsel = "",
                       reg_interactions = "none",
                       reg_predict = "none",
                       reg_predict_cmd = "",
                       reg_predict_data = "",
                       reg_check = "",
                       reg_conf_level = .95,
                       reg_plots = "",
                       reg_line = FALSE,
                       reg_loess = FALSE) {

	# dataset = "mtcars"
	# reg_var1 = "mpg"
	# reg_var2 = c("cyl","disp","cyl")
	# reg_var3 = "cyl"
	# reg_interactions = 2
	# reg_intsel = c("cyl:disp","blue:gold")
	# reg_intsel = c("cyl:disp")
	# dat = mtcars
	# data_filter = ""

	vars <- reg_var2

	# adding interaction terms as needed
	# if(reg_interactions != 'none' && !is.null(reg_intsel) && length(vars) > 1) {
	if(reg_interactions != 'none' &&
	  reg_intsel != "" &&
	  length(vars) > 1) {
		if({reg_intsel %>% strsplit(":") %>% unlist} %in% reg_var2 %>% all) {
			vars <- c(vars,reg_intsel)
	 	} else{
	 		return(cat("Interaction terms contain variables not selected as main effects"))
	 	}
	}

	dat <- getdata_exp(dataset, c(reg_var1, reg_var2), filt = data_filter)

	if("standardize" %in% reg_check)
    dat[,sapply(dat, is.numeric)] %<>% mutate_each(funs(scale))

	formula <- paste(reg_var1, "~", paste(vars, collapse = " + "))

	if("stepwise" %in% reg_check) {
    # use k = 2 for AIC, use k = log(nrow(dat)) for BIC
		mod <- lm(paste(reg_var1, "~ 1") %>% as.formula, data = dat) %>%
      step(., k = 2, scope = list(upper = formula), direction = 'both')
	} else {
		mod <- lm(formula, data = dat)
	}

	# specifying plot heights
	nrVars <- length(as.character(attr(mod$terms,'variables'))[-1])

	plotHeight <- 650
	plotWidth <- 650

	if(reg_plots == 'histlist') plotHeight <- 325 * ceiling(nrVars / 2)

	if(reg_plots == 'dashboard') plotHeight <- 630 + 375

	if(reg_plots == 'correlations') {
		plotHeight <- 150 * nrVars
		plotWidth <- 150 * nrVars
	}

	if(reg_plots %in% c('scatterlist','leverage_plots','resid_vs_predictorlist'))
		plotHeight <- 325 * ceiling((nrVars-1) / 2)

  environment() %>% as.list %>% set_class(c("regression",class(.)))
}

# x1 <- rnorm( 100 )
# x2 <- 2 * x1
# x3 <- as.factor(c(rep(1,50),rep(0,50)))
# x4 <- as.factor(c(rep(1,50),rep(0,50)))
# y <- rnorm( 100 )

# reg <- lm(y ~ x2 + x3 + x4)
# reg
# alias(reg)
# reg$coeff %>% is.na %>% any

# vif( lm( y ~ x1 + x2 ) )
# x <- alias( lm( y ~ x1 + x2 ) )
# str(x)



# list of function arguments
# install.packages('dplyr')
# devtools::install_github('hadley/purrr')
# install.packages('broom')
# install.packages('gridExtra')
# library(dplyr)
# library(magrittr)
# library(ggplot2)
# library(broom)
# library(devtools)
# library(gridExtra)
# library(car)
# ?vif
# source("/Users/vnijs/gh/radiant_dev/R/radiant.R")
# source("/Users/vnijs/gh/radiant_dev/inst/base/radiant.R")
# reg_args <- list()
# reg_args$dataset <- "diamonds"
# reg_args$reg_var1 <- "price"
# reg_args$reg_var2 <- c("carat","color")
# reg_args$reg_var3 <- "color"
# reg_args$reg_plots <- "dashboard"
# result <- do.call(regression, reg_args)
# summary_regression(result)
# plots_regression(result)
# str(result)


# summary_regression <- reactive({ .summary_regression() })
summary_regression <- function(result = .regression()) {

	if(class(result$mod) != 'lm') return(result)

  cat("Linear regression (OLS)\n")
  cat("Data     :", result$dataset, "\n")
  if(result$show_filter) {
    if(result$data_filter %>% gsub("\\s","",.) != "")
      cat("Filter   :", gsub("\\n","", result$data_filter), "\n")
  }
  cat("Dependent variable   :", result$reg_var1, "\n")
  cat("Independent variables:", paste0(result$reg_var2, collapse=", "), "\n")
  if("standardize" %in% result$reg_check)
 		cat("Standardized coefficients shown\n")
 	cat("\n")
  # cat("Null hyp.: variables x and y are not correlated\n")
  # cat("Alt. hyp.: variables x and y are correlated\n\n")

	reg_coeff <- tidy(result$mod)
	reg_coeff$` ` <- sig_stars(reg_coeff$p.value)
	reg_coeff[,c(2:5)] %<>% round(3)
	reg_coeff$p.value[reg_coeff$p.value < .001] <- "< .001"
	colnames(reg_coeff) <- c("  ","coefficient","std.error","t.value","p.value"," ")
	print(reg_coeff, row.names=FALSE)

	reg_fit <- glance(result$mod) %>% round(3)
	if(reg_fit['p.value'] < .001) reg_fit['p.value'] <- "< .001"
  cat("\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("R-squared:", paste0(reg_fit$r.squared, ", "), "Adjusted R-squared:", reg_fit$adj.r.squared, "\n")
  cat("F-statistic:", reg_fit$statistic, paste0("df(", reg_fit$df, ",", reg_fit$df.residual, "), p.value"), reg_fit$p.value)
  cat(paste0("\nNr obs: ", reg_fit$df + reg_fit$df.residual))
  cat("\n\n")


  if("rmse" %in% result$reg_check) {
    mean(result$mod$residual^2, na.rm=TRUE) %>% sqrt %>%
    cat("Prediction error (RMSE): ", ., "\n\n")
  }

  if("sumsquares" %in% result$reg_check) {
    atab <- anova(result$mod)
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
		if(result$mod$coeff %>% is.na %>% any) {
			cat("There is perfect multi-collineary in the set of independent variables.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
		} else {
    	vif_regression(result) %>% print
		}
    cat("\n")
  }

  # if(result$reg_predict != '' || result$reg_predict_data != "none") {
  if(result$reg_predict != "none") {

    # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
    # as starting point
    if("standardize" %in% result$reg_check) {
      cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the
          standardized coefficients box and try again.")
    } else {

      if(result$reg_predict == "cmd") {
     		reg_predict <- gsub("\"","\'", result$reg_predict)
        nval <- try(eval(parse(text = paste0("data.frame(",reg_predict,")"))), silent = TRUE)
      } else {
        nval <- r_data[[result$reg_predict_data]]
        vars <- as.character(attr(result$mod$terms,'variables'))[-1]
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
          nval_names <- names(r_data[[result$reg_predict_data]])
          cat(ivars[!ivars %in% nval_names])
        }
      } else {

        dat <- ggplot2::fortify(result$mod)
        vars <- as.character(attr(result$mod$terms,'variables'))[-1]
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
            if(result$reg_predict == "cmd" & result$reg_predict_cmd == "") {
              pred <- try(log("a"), silent=TRUE)
            } else {
              newdat[names(nval)] <- list(NULL)
              nnd <- data.frame(newdat[-1],nval)
              pred <- try(predict(result, nnd,interval = 'prediction', level = result$reg_conf_level), silent = TRUE)
              # pred <- try(predict(result, nnd,interval = 'prediction', level = as.numeric(reg_conf_level)), silent = TRUE)
            }

            if(!is(pred, 'try-error')) {
            	if(result$reg_predict == "data") {
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



  if("confint" %in% result$reg_check) {

		if(result$mod$coeff %>% is.na %>% any) {
			cat("There is perfect multi-collineary in the set of independent variables.\nOne or more variables were dropped from the estimation.\nMulti-collinearity diagnostics were not calculated.\n")
		} else {
	    cl_split <- function(x) 100*(1-x)/2
	    cl_split(result$reg_conf_level) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_low
	    (100 - cl_split(result$reg_conf_level)) %>% round(1) %>% as.character %>% paste0(.,"%") -> cl_high

	    cat("Coefficient confidence intervals:\n")
	    confint(result$mod, level = result$reg_conf_level) %>%
	      data.frame %>%
	      magrittr::set_colnames(c("Low","High")) %>%
	      cbind(select(reg_coeff,2),.) %>%
	      round(3) -> ci_tab

	    ci_tab$`+/-` <- (ci_tab$High - ci_tab$coefficient)
	    ci_tab %>%
	      magrittr::set_colnames(c("coefficient",cl_low,cl_high, "+/-")) %>%
	      print
	    cat("\n")
  	}
	}

	if(!result$reg_var3 %>% is_empty) {
		if("stepwise" %in% result$reg_check) {
	  	cat("Model comparisons not conducted when Stepwise has been selected.\n")
	  } else {

				sub_formula <- ". ~ 1"
				vars <- result$reg_var2

				if(result$reg_interactions != 'none' &&
					  result$reg_intsel != "" &&
					  length(vars) > 1) {

						if({result$reg_intsel %>% strsplit(":") %>% unlist} %in% result$reg_var3 %>% any) {
	 						cat("Interaction terms contain variables specified for testing.\nRelevant interaction terms are include in the requested test.\n\n")

# vars <- c("carat", "clarity", "cut")
# result$reg_intsel <-  c("carat:clarity", "carat:cut", "clarity:cut")
# result$reg_var3 <-  c("carat")

							for(i in result$reg_var3) {
								ind <- grep(i,result$reg_intsel)
								result$reg_var3 <- c(result$reg_var3, result$reg_intsel[ind])
							}
							result$reg_var3 <- unique(result$reg_var3)
	 					}
			 			vars <- c(vars,result$reg_intsel)
				}
				not_selected <- setdiff(vars,result$reg_var3)
				if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))

				sub_mod <- update(result$mod, sub_formula, data = result$dat) %>%
				anova(., result$mod, test='F')

				if(sub_mod[,"Pr(>F)"][2] %>% is.na) return(cat(""))

				if(sub_mod[,"Pr(>F)"][2] < .001) sub_mod[,"Pr(>F)"][2] <- "< .001"

				cat(attr(sub_mod,"heading")[2])
				result$dat[,result$reg_var1] %>%
				{ sum((. - mean(.))^2) } %>%
				{1 - (sub_mod$RSS / .)} %>%
				round(3) %>%
				cat("\nR-squared, Model 1 vs 2:", .)
  			cat("\nF-statistic:", sub_mod$F[2] %>% round(3), paste0("df(", sub_mod$Res.Df[1], ",", sub_mod$Res.Df[2], "), p.value"), sub_mod[,"Pr(>F)"][2])
	  }
	}
}

plots_regression <- function(result = .regression()) {

	if(class(result$mod) != 'lm') return(result)

	if({result$reg_intsel %>% strsplit(":") %>% unlist} %in% result$reg_var2 %>% all) {
			# nothing
 	} else {
	 		return(cat("Interaction terms contain variables not selected as main effects"))
 	}

	mod <- ggplot2::fortify(result$mod)
	# mod[1:10,]
  # mod <- augment(result$mod)
  # mod[1:10,]

	vars <- as.character(attr(result$mod$terms,'variables'))[-1]
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
		sshh( do.call(grid.arrange, c(plots, list(ncol = 2))) )
}

################################################################
# Additional functions for regression
################################################################
saveRegResiduals <- function(result = .regression()) {
	resid <- data.frame(result$residuals)
	changedata(resid, 'residuals')
}

reg_int_vec <- function(reg_vars, nway) {
  int_vec <- c()
  for(i in 2:nway) {
    int_vec %<>% {c(., combn(reg_vars, i) %>% apply( 2, paste, collapse = ":" ))}
  }
  int_vec
}

# test
# reg_vars <- letter[1:5]
# reg_int_vec(reg_vars,2)
# reg_int_vec(reg_vars,3)

vif_regression <- function(result = .regression()) {
	if(length(result$reg_var2) > 1) {
	  cat("Variance Inflation Factors\n")
    vif(result$mod) %>%
      { if(!dim(.) %>% is.null) .[,"GVIF"] else . } %>% # needed when factors are included
      data.frame("VIF" = ., "Rsq" = 1 - 1/.) %>%
      round(3) %>%
      .[order(.$VIF, decreasing=T),] %>%
      { if(nrow(.) < 8) t(.) else . }
	} else {
  	cat("Insufficient number of independent variables selected to calculate\nmulti-collinearity diagnostics\n")
  	return("VIF")
	}
}


# test_regression <- function(result = .regression()) {
# 	# dat <- r_data[[result$dataset]]
# 	if(result$reg_standardize) dat <- data.frame(lapply(dat,reg_standardize_fun))

# 	sub_formula <- ". ~ 1"
# 	vars <- result$reg_var2

#   if(result$reg_interactions != 'none' && !is.null(result$reg_intsel) && length(vars) > 1) {
# 		vars <- c(vars,result$reg_intsel)
# 	}

# 	not_selected <- setdiff(vars,result$reg_var3)
# 	if(length(not_selected) > 0) sub_formula <- paste(". ~", paste(not_selected, collapse = " + "))

# 	reg_sub <- update(result, sub_formula)
# 	anova(reg_sub, result, test='F')
# }
