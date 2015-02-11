summary_regression <- function(result = .regression()) {

	if(class(result$mod) != 'lm') return(result)

	# sig_stars <- function(pval) {
	#   sapply(pval, function(x) x < c(.001,.01, .05, .1)) %>%
	#     colSums %>% add(1) %>%
	#     c("",".","*", "**", "***")[.]
	# }

	reg_coeff <- tidy(result$mod)
	reg_coeff$` ` <- sig_stars(reg_coeff$p.value)
	reg_coeff[,c(2:5)] %<>% round(3)
	reg_coeff$p.value[reg_coeff$p.value < .001] <- "< .001"
	colnames(reg_coeff) <- c("  ","coefficient","std.error","t.value","p.value"," ")
	print(reg_coeff, row.names=FALSE)

	reg_fit <- glance(result$mod) %>% round(3)
	if(reg_fit['p.value'] < .001) reg_fit['p.value'] <- "< .001"
	# reg_fit

	reg_aug <- augment(result$mod)
	# reg_aug[1:10,]

  # cat(paste("Data:",dataset,"\n"))
  # cat(paste("Dependent variable:",dv,"\n"))

  # if(std_c == TRUE) {
  #   cat("Standardized coefficients:\n")
  # } else {
  #   cat("Coefficients:\n")
  # }

  # if (!is.null(x$fstatistic)) {
  #   cat("R-squared: ", formatC(x$r.squared, digits = digits))
  #   cat(", Adjusted R-squared: ", formatC(x$adj.r.squared, digits = digits),
  #       "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits), "on", x$fstatistic[2L],
  #       "and", x$fstatistic[3L], "DF,  p-value:", format.pval(pf(x$fstatistic[1L], x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),  digits = digits)
  #   )
  #   cat(paste0("\nNr obs: ",length(x$residuals)))
  #   cat("\n\n")
  # }



	# if(reg_outlier) print(outlierTest(result), digits = 3)

  if(result$reg_predict_cmd != '' || result$reg_predict_data != "none") {

    # used http://www.r-tutor.com/elementary-statistics/simple-linear-regression/prediction-interval-linear-regression
    # as starting point
    if(result$reg_standardize) {
      cat("Currently you cannot use standardized coefficients for prediction.\nPlease uncheck the
          standardized coefficients box and try again.")
    } else {

      if(result$reg_predict == "cmd") {
     		reg_predict_cmd <- gsub("\"","\'", result$reg_predict_cmd)
        nval <- try(eval(parse(text = paste0("data.frame(",reg_predict_cmd,")"))), silent = TRUE)
      } else {
        nval <- r_data[[result$reg_predict_data]]
        vars <- as.character(attr(result$terms,'variables'))[-1]
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
            if(result$reg_predict == "cmd" & result$reg_predict_cmd == "") {
              pred <- try(log("a"), silent=TRUE)
            } else {
              newdat[names(nval)] <- list(NULL)
              nnd <- data.frame(newdat[-1],nval)
              pred <- try(predict(result, nnd,interval = 'prediction', level = result$reg_conf_level), silent = TRUE)
              # pred <- try(predict(result, nnd,interval = 'prediction', level = as.numeric(reg_conf_level)), silent = TRUE)
            }

            if(!is(pred, 'try-error')) {
            	if(result$reg_predict == "dataframe") {
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
