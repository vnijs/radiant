adf.cv <- function (n, type = "none", prob = "0.95")  #{{{
{
	# sample size to use
    if(n <= 25) {
      rsel <- "25"
	} else if(n <= 50) {
      rsel <- "50"
	} else if(n <= 100) {
      rsel <- "100"
	} else if(n <= 250) {
      rsel <- "250"
	} else if(n <= 500) {
      rsel <- "500"
    } else if(n > 500) {
      rsel <- "Inf."
	}

	if(type == "trend") {

		# test for presence of a unitroot
		tau.t <- rbind(c(-4.38, -3.60, -3.24),
					   c(-4.15, -3.50, -3.18),
					   c(-4.04, -3.45, -3.15),
					   c(-3.99, -3.43, -3.13),
					   c(-3.98, -3.42, -3.13),
					   c(-3.96, -3.41, -3.12))

		# t-test of significance of the deterministic components
		tau.a.t <- rbind(c(4.05, 3.20, 2.77),
						 c(3.87, 3.14, 2.75),
						 c(3.78, 3.11, 2.73),
						 c(3.74, 3.09, 2.73),
						 c(3.72, 3.08, 2.72),
						 c(3.71, 3.08, 2.72))

		tau.b.t <- rbind(c(3.74, 2.85, 2.39),
						 c(3.60, 2.81, 2.38),
						 c(3.53, 2.79, 2.38),
						 c(3.49, 2.79, 2.38),
						 c(3.48, 2.78, 2.38),
						 c(3.46, 2.78, 2.38))

		# F-test of joint significance
		phi2 <- rbind(c(8.21, 5.68, 4.67),
					  c(7.02, 5.13, 4.31),
					  c(6.50, 4.88, 4.16),
					  c(6.22, 4.75, 4.07),
					  c(6.15, 4.71, 4.05),
					  c(6.09, 4.68, 4.03))

		phi3 <- rbind(c(10.61, 7.24, 5.91),
					  c( 9.31, 6.73, 5.61),
					  c( 8.73, 6.49, 5.47),
					  c( 8.43, 6.49, 5.47),
					  c( 8.34, 6.30, 5.36),
					  c( 8.27, 6.25, 5.34))  

		colnames(tau.t) <- colnames(tau.a.t) <- colnames(tau.b.t) <- colnames(phi2) <- colnames(phi3) <- c("0.99","0.95","0.90")
		rownames(tau.t) <- rownames(tau.a.t) <- rownames(tau.b.t) <- rownames(phi2) <- rownames(phi3) <- c("25","50","100","250","500","Inf.")

		return(c(tau.t[rsel,prob], tau.a.t[rsel,prob], tau.b.t[rsel,prob], phi2[rsel,prob], phi3[rsel,prob]))

	} else if(type == "drift") {
		# test for presence of a unitroot
		tau.m <- rbind(c(-3.75, -3.00, -2.62),
				       c(-3.58, -2.93, -2.60),
					   c(-3.51, -2.89, -2.58),
					   c(-3.46, -2.88, -2.57),
					   c(-3.44, -2.87, -2.57),
					   c(-3.43, -2.86, -2.57))

		# t-test of significance of the deterministic components
		tau.a.m <- rbind(c(3.41, 2.61, 2.20),
	  		             c(3.28, 2.56, 2.18),
						 c(3.22, 2.54, 2.17),
				         c(3.19, 2.53, 2.16),
				         c(3.18, 2.52, 2.16),
				         c(3.18, 2.52, 2.16))

		# F-test of joint significance
		phi1 <- rbind(c(7.88, 5.18, 4.12),
				      c(7.06, 4.86, 3.94),
					  c(6.70, 4.71, 3.86),
					  c(6.52, 4.63, 3.81),
					  c(6.47, 4.61, 3.79),
					  c(6.43, 4.59, 3.78))

		colnames(tau.m) <- colnames(tau.a.m) <- colnames(phi1) <- c("0.99","0.95","0.00")
		rownames(tau.m) <- rownames(tau.a.m) <- rownames(phi1) <- c("25","50","100","250","500","Inf.")

		return(c(tau.m[rsel,prob], tau.a.m[rsel,prob], phi1[rsel,prob]))

	} else if(type == "none") {

		# test for presence of a unitroot
		tau	<- rbind(c(-2.66, -1.95, -1.60),
			   	     c(-2.62, -1.95, -1.61),
				     c(-2.60, -1.95, -1.61),
				     c(-2.58, -1.95, -1.62),
					 c(-2.58, -1.95, -1.62),
					 c(-2.58, -1.95, -1.62))
		colnames(tau) <- c("0.99","0.95","0.90")
		rownames(tau) <- c("25","50","100","250","500","Inf.")

		return(tau[rsel,prob])
	}
}

# }}}
adf.test <- function (y, exo. = 0, type = "drift", lags = NULL, prob = .95, print = TRUE)  #{{{
{
	# setting the max.lag variable if not provided
	if(is.null(lags)) {
		lags = round(10*log10(length(y)))  
	}

	# setting up the data for the ADF test
	lags <- lags + 1
    mlags <- lags
    z.diff <- diff(y)
    n <- length(z.diff)
    z.lag.1 <- y[mlags:n]
    trend <- mlags:n

	f <- "z.diff ~ z.lag.1"
    if (mlags > 1) {
		z.diff.lag <- data.frame(embed(z.diff, mlags)[,2:mlags])
		names(z.diff.lag) <- paste("z.diff.lag",1:(lags-1),sep = "")
		attach(z.diff.lag)
		f.lags <- paste(names(z.diff.lag), collapse = " + ")
		f <- paste(f,f.lags, sep = " + ")
	}

    if (length(exo.) > 1) {
		if(is.matrix(exo.)) {
			exo. <- exo.[mlags:n,]
		} else {
			stop("\nThe exogenous variables have not been put into a matrix.\n")
		}

		exo.varnm <- colnames(exo.)
		exo. <- data.frame(exo.)
		names(exo.) <- paste("exo.",exo.varnm, sep = "")
		attach(exo.)
		f.exo <- paste(names(exo.), collapse = " + ")
		f <- paste(f,f.exo, sep = " + ")
	}

	z.diff <- z.diff[mlags:n]
    n <- length(z.diff)

    if (type == "none") {
		f <- paste(f,"1", sep = " - ")
		adf.reg <- lm(as.formula(f))
		### adf.reg.step <- step(adf.reg, scope = list(lower = ~ z.lag.1), direction = "both", k = log(n), trace = FALSE)
		adf.reg.step <- step(adf.reg, scope = list(lower = ~ z.lag.1), direction = "both", trace = FALSE)
		adf.reg.sum <- summary(adf.reg.step)
		ret <- matrix(rep(NA,2))
		ret[1,] <- coef(adf.reg.sum)['z.lag.1','t value']
		colnames(ret) <- "tau"
	} else if (type == "drift") {
		adf.reg <- lm(as.formula(f))
		### adf.reg.step <- step(adf.reg, scope = list(lower = ~ 1 + z.lag.1), direction = "both", k = log(n), trace = FALSE)
		adf.reg.step <- step(adf.reg, scope = list(lower = ~ 1 + z.lag.1), direction = "both", trace = FALSE)
		adf.reg.sum <- summary(adf.reg.step)
		coef <- coef(adf.reg.sum)
		tau.m <- coef['z.lag.1','t value']
		tau.a.m <- coef['(Intercept)','t value']
		phi1 <- anova(update(adf.reg, . ~ . - z.lag.1 - 1), adf.reg)$F[2]
		ret <- matrix(rep(NA,6),nrow = 2)
		ret[1,] <- c(tau.m, tau.a.m, phi1)
		colnames(ret) <- c("tau.m","tau.a.m","phi1")
	} else if (type == "trend") {
		f <- paste(f,"trend", sep = " + ")
		adf.reg <- lm(as.formula(f))
		### adf.reg.step <- step(adf.reg, scope = list(lower = ~ 1 + trend + z.lag.1), direction = "both", k = log(n), trace = FALSE)
		adf.reg.step <- step(adf.reg, scope = list(lower = ~ 1 + trend + z.lag.1), direction = "both", trace = FALSE)
		adf.reg.sum <- summary(adf.reg.step)
		coef <- coef(adf.reg.sum)
		tau.t <- coef['z.lag.1','t value']
		tau.a.t <- coef['(Intercept)','t value']
		tau.b.t <- coef['trend','t value']
		phi2 <- anova(update(adf.reg, . ~ . - z.lag.1 - trend - 1), adf.reg)$F[2]
		phi3 <- anova(update(adf.reg, . ~ . - z.lag.1 - trend), adf.reg)$F[2]
		ret <- matrix(rep(NA,10),nrow = 2)
		ret[1,] <- c(tau.t, tau.a.t, tau.b.t, phi2, phi3)
		colnames(ret) <- c("tau.t","tau.a.t","tau.b.t","phi2","phi3")
	}

	if(print == TRUE) {
		print(adf.reg.sum)
	}

	# getting the degrees of freedom
	df <- c(adf.reg$df.residual,NA)
	rownames(ret) <- c("stat","cv")
	ret[2,] <- adf.cv(n, type = type, prob = as.character(prob))
	ret <- cbind(ret,df)

	# detaching data
	detach(exo.)
	detach(z.diff.lag)

	return(ret)
   
} #}}}
adf.loop <- function (y, exo = 0, slags = 'aic', lags = NULL, prob = .95, err = FALSE, print = TRUE)  #{{{
{
	# using a prepackaged routine to find the best set of augmented terms
	### library(uroot)
	### library(tseries)

	################################################################
	################################################################
	## TODO: Add (dis) confirmation using the PHI stats
	################################################################
	################################################################

	# setting up a matrix that contains the output information for the series
	unr.adf <- matrix(rep(NA,3), nrow = 1)
	colnames(unr.adf) <- c("unitroot","drift","trend")
	
	# evaluate the test statistics generated for the ADF test with trend
	unr <- adf.test(y, exo. = exo, type = "trend",  lags = lags, prob = prob, print = print)
	df <- unr['stat','df']

	if(err == TRUE) {
		print(unr)
	}

	if(unr['stat','tau.t'] < unr['cv','tau.t']) {

		# there is no unitroot but still check if the trend and intercept are significant
		unr.adf[,'unitroot'] <- "No" 

		if(pt(abs(unr['stat','tau.b.t']), df, lower.tail = FALSE) < ((1 - prob) / 2)) {		# testing if the trend is significant when there is no unitroot (2-sided)
			unr.adf[,'trend'] = "Yes" 
		} else { 
			unr.adf[,'trend'] = "No" 
		}
		if(pt(abs(unr['stat','tau.a.t']), df, lower.tail = FALSE) < ((1 - prob) / 2)) {		# testing if the drift is significant when there is no unitroot (2-sided)
			unr.adf[,'drift'] = "Yes" 
		} else { 
			unr.adf[,'drift'] = "No" 
		}
	
	} else {

		# there may be a unitroot, test if the trend is significant
		if(abs(unr['stat','tau.b.t']) > unr['cv','tau.b.t']) {
			unr.adf[,'trend'] = "Yes" 

			if(pnorm(unr['stat','tau.t'], lower.tail = FALSE) < (1-prob)) {						# testing if their is a unitroot using the normal distribution
				unr.adf[,'unitroot'] = "No" 

				if(pt(abs(unr['stat','tau.a.t']), df, lower.tail = FALSE) < ((1 - prob) / 2) ) {		# testing if the drift is significant when there is no unitroot
					unr.adf[,'drift'] = "Yes" 
				} else { 
					unr.adf[,'drift'] = "No" 
				}
			} else {
				unr.adf[,'unitroot'] = "Yes" 

				if(abs(unr['stat','tau.a.t']) > unr['cv','tau.a.t']) { 
					unr.adf[,'drift'] = "Yes" 
				} else { 
					unr.adf[,'drift'] = "No" 
				}
			}
		} else {
			unr.adf[,'trend'] = "No" 

			# evaluate the test statistics generated for the ADF test without trend but with drift
			unr <- adf.test(y, exo. = exo, type = "drift", lags = lags, prob = prob, print = print)
			df <- unr['stat','df']

			if(err == TRUE) {
				print(unr)
			}

			if(unr['stat','tau.m'] < unr['cv','tau.m']) { 

				# there is no unitroot
				unr.adf[,'unitroot'] = "No" 

				if(pt(abs(unr['stat','tau.a.m']), df, lower.tail = FALSE) < ((1 - prob) / 2) ) {		# testing if the drift is significant when there is no unitroot
					unr.adf[,'drift'] = "Yes" 
				} else { 
					unr.adf[,'drift'] = "No" 
				}
			} else {

				if(abs(unr['stat','tau.a.m']) > unr['cv','tau.a.m']) { 
					unr.adf[,'drift'] = "Yes" 

					if(pnorm(unr['stat','tau.m'], lower.tail = FALSE) < (1-prob)) {						# testing if their is a unitroot using the normal distribution
						unr.adf[,'unitroot'] = "No" 
					} else {
						unr.adf[,'unitroot'] = "Yes" 
					}

				} else { 
					unr.adf[,'drift'] = "No" 

					# evaluate the test statistics generated for the ADF test without trend but with drift
					unr <- adf.test(y, exo. = exo, type = "none", lags = lags, prob = prob, print = print)

					if(err == TRUE) {
						print(unr)
					}

					if(unr['stat','tau'] < unr['cv','tau']) { 
						unr.adf[,'unitroot'] = "No" 
					} else {
						unr.adf[,'unitroot'] = "Yes" 
					}
				}
			}
		}
	}

	return(unr.adf)

} #}}}

za.test <- function(y, exo. = 0, model = "both", lags = NULL, brk = "", print = TRUE) # Zivot-Andrews structural-break unitroot test {{{
{
	# setting up the data for the SB test
	lags <- lags + 1
    mlags <- max(lags)
    z.diff <- diff(y)
    n <- length(z.diff)
    z.lag.1 <- y[mlags:n]
    trend <- mlags:n

	f <- "z ~ z.lag.1 + trend"
    if (mlags > 1) {
		z.diff.lag <- embed(z.diff, mlags)[,lags]
		if(length(lags) > 1) {
			colnames(z.diff.lag) <- as.character(lags - 1)
		}
		f <- paste(f,"z.diff.lag", sep = " + ")
	}
    if (length(exo.) > 1) {
		if(is.matrix(exo.)) {
			exo. <- exo.[mlags:n,]
		} else {
			stop("\nThe exogenous variables have not been put into a matrix.\n")
		}
		f <- paste(f,"exo.", sep = " + ")
	}

	z <- y[(mlags+1):(n+1)]
	n <- length(z)

	roll.stat <- cbind(1:n,rep(NA,n))

	if(brk == "") {
		idx <- mlags:(n-mlags)
	} else {
		idx <- brk - mlags
	}

	if(model=="intercept"){
		roll <- function(brk){
			du <- c(rep(0, brk), rep(1, (n-brk)))
			formula <- as.formula(paste(f,"du", sep = " + "))
			roll.reg <- coef(summary(lm(formula)))
			return((roll.reg['z.lag.1','Estimate']-1)/roll.reg['z.lag.1','Std. Error'])			# -1 because H0 is == 1
		}

		roll.stat[idx,2] <- sapply(idx, roll)
		cv <- c(-5.34, -4.8, -4.58, -2.99, -2.77, -2.32)		# Zivot and Andrews, JBES 1992, table 2A, p. 256
		bpoint <- which.min(roll.stat[,2])
		du <- c(rep(0, bpoint), rep(1, (n-bpoint)))
		formula <- as.formula(paste(f,"du", sep = " + "))
		za.reg <- summary(lm(formula))

	} else if(model=="trend"){

		roll <- function(brk){
			dt <- c(rep(0, brk), 1:(n-brk))
			formula <- as.formula(paste(f,"dt", sep = " + "))
			roll.reg <- coef(summary(lm(formula)))
			return((roll.reg['z.lag.1','Estimate']-1)/roll.reg['z.lag.1','Std. Error'])
		}

		roll.stat[idx,2] <- sapply(idx, roll)
		cv <- c(-4.93, -4.42, -4.11, -2.48, -2.31, -1.97)	# Zivot and Andrews, JBES 1992, table 3A, p. 256
		bpoint <- which.min(roll.stat[,2])
		dt <- c(rep(0, bpoint), 1:(n-bpoint))
		formula <- as.formula(paste(f,"dt", sep = " + "))
		za.reg <- summary(lm(formula))

	} else if(model=="both"){

		roll <- function(brk){
			du <- c(rep(0, brk), rep(1, (n-brk)))
			dt <- c(rep(0, brk), 1:(n-brk))
			formula <- as.formula(paste(f,paste(c("du","dt"), collapse = " + "), sep = " + "))
			roll.reg <- coef(summary(lm(formula)))
			return((roll.reg['z.lag.1','Estimate']-1)/roll.reg['z.lag.1','Std. Error'])
		}

		roll.stat[idx,2] <- sapply(idx, roll)
		cv <- c(-5.57, -5.08, -4.82, -3.25, -3.06, -2.72)	# Zivot and Andrews, JBES 1992, table 4A, p. 257
		bpoint <- which.min(roll.stat[,2])
		du <- c(rep(0, bpoint), rep(1, (n-bpoint)))
		dt <- c(rep(0, bpoint), 1:(n-bpoint))

		formula <- as.formula(paste(f,paste(c("du","dt"), collapse = " + "), sep = " + "))
		za.reg <- summary(lm(formula))
	}

	# setting up a matrix that contains the output information for the series
	unr.za <- matrix(c(roll.stat[bpoint,2],bpoint+mlags,cv), nrow = 1)
	colnames(unr.za) <- c("t.alpha","break","0.01","0.05","0.10","0.90","0.95","0.99")

	if(print == TRUE) {
		print(za.reg)
		print(unr.za)
	}

	return(unr.za)
} #}}}
reg.loop.za <- function (y, exo. = 0, slags = 'aic', Pmax = "") # {{{
{
	library(nlme)

	if(Pmax == "") {
		Pmax = round(10*log10(n))
	}

	ret <- cbind(rep(NA,Pmax+1),0:Pmax)
	colnames(ret) <- c("IC","lags")

	# setting up the data
	z.diff <- diff(y)
	n <- length(z.diff)
	z.lag.1 <- y[(Pmax+1):n]
	trend <- (Pmax+1):n

	f <- "z ~ z.lag.1 + trend"
	if (Pmax > 0) {
		z.diff.lag.Pmax <- embed(z.diff, Pmax + 1)[,-1]

		if(Pmax > 1) {
			colnames(z.diff.lag.Pmax) <- as.character(1:Pmax)
		}
	}

	if (length(exo.) > 1) {
		if(is.matrix(exo.)) {
			exo. <- exo.[(Pmax+1):n,]
		} else {
			stop("\nThe exogenous variables have not been put into a matrix.\n")
		}
		f <- paste(f,"exo.", sep = " + ")
	}

	z <- y[(Pmax+2):(n+1)]

	for(lags in 0:Pmax) {

		if (lags > 0) {
			z.diff.lag <- z.diff.lag.Pmax[,1:lags]
			formula <- paste(f,"z.diff.lag", sep = " + ")
		} else {
			formula <- f
		}

		reg.loop <- lm(as.formula(formula))

		if(slags == 'bic') {
			ret[lags+1,"IC"] <- BIC(logLik(reg.loop))
		} else if(slags == 'aic') {
			ret[lags+1,"IC"] <- AIC(logLik(reg.loop))
		} else {
			stop("Model selection criterium not available. Please use aic or bic")
		}
	}

	return(ret[order(ret[,1]),])

} # }}}
za.loop <- function (y, exo = 0, model = "both", slags = 'bic', Pmax = "", prob = .95, err = FALSE, print = FALSE)  # {{{
{
	# using a prepackaged routine to find the best set of augmented terms
	### library(uroot)
	### library(tseries)

	# setting up a matrix that contains the output information for the series
	unr.za <- matrix(c(NA,NA), nrow = 1)
	colnames(unr.za) <- c("SB","SB-date")

	n <- length(y)

	if(Pmax == "") {
		Pmax = round(10*log10(n))
	}

	idx <- Pmax:(n-Pmax)
	roll.lags <- cbind(1:n,rep(NA,n))

	if(model=="intercept"){

		roll <- function(brk, roll.exo = NULL){

			du <- c(rep(0, brk), rep(1, (n-brk)))

			if(is.null(roll.exo)) {
				 roll.exo = matrix(du)
			} else {
				roll.exo = cbind(roll.exo,du)
			}

			return(reg.loop.za(y, exo. = roll.exo, slags = slags, Pmax = min(brk, Pmax))[1,'lags'])
		}

		roll.lags[idx,2] <- sapply(idx, roll, roll.exo = exo)

	} else if(model=="trend"){

		roll <- function(brk, roll.exo = NULL){
			dt <- c(rep(0, brk), 1:(n-brk))

			if(is.null(roll.exo)) {
				 roll.exo = matrix(dt)
			} else {
				roll.exo = cbind(roll.exo,du)
			}

			return(reg.loop.za(y, exo. = roll.exo, slags = slags, Pmax = min(brk, Pmax))[1,'lags'])
		}

		roll.lags[idx,2] <- sapply(idx, roll, roll.exo = exo)

	} else if(model=="both"){

		roll <- function(brk, roll.exo = NULL){

			du <- c(rep(0, brk), rep(1, (n-brk)))
			dt <- c(rep(0, brk), 1:(n-brk))

			if(is.null(roll.exo)) {
				 roll.exo = cbind(du,dt)
			} else {
				roll.exo = cbind(roll.exo,du)
			}

			return(reg.loop.za(y, exo. = roll.exo, slags = slags, Pmax = min(brk, Pmax))[1,'lags'])
		}

		roll.lags[idx,2] <- sapply(idx, roll, roll.exo = exo)
	}

	roll.za.test <- function(brk, lags = ""){

		roll.reg <- za.test(y, exo. = exo, model = model, brk = brk, lags = 1:lags[brk], print = FALSE)
		return(roll.reg[1])
	}

	roll.lags.no.na <- roll.lags[!is.na(roll.lags[,2]),]
	roll.stat <- sapply(roll.lags.no.na[,1], roll.za.test, lags = roll.lags[,2])
	ind <- which.min(roll.stat)
	roll.stat <- za.test(y, exo. = exo, model = model, brk = roll.lags.no.na[ind,1], lags = 1:roll.lags.no.na[ind,2], print = print)

	if(roll.stat[,'t.alpha'] > roll.stat[,as.character(prob)] || roll.stat[,'t.alpha'] < roll.stat[,as.character(1 - prob)]) {
		unr.za[,'SB'] = 'Yes'
		unr.za[,'SB-date'] = roll.stat[,'break']
	} 
	else {
		unr.za[,'SB'] = 'No'
	}

	return(unr.za)
} # }}}

IRFcalc <- function(var.par.mat, lags, irf.length, nr.eq, shock.vector) #{{{
{

	irf <- matrix(rep(0,(irf.length)*nr.eq),ncol = irf.length)

	irf[,lags] <- shock.vector

	for(i in lags:(irf.length-1)) {
		irf[,i+1] <- c(irf[,i:(i-lags+1)]) %*% var.par.mat
	}

	return(t(irf[,lags:irf.length])) 
} #}}}
IRF.co.calc <- function(var.par.mat, lags, irf.length, nr.eq, shock.vector, ecc.pars, co.pars) #{{{
{

	irf <- matrix(rep(0,(irf.length)*nr.eq),ncol = irf.length)
	irf[,lags] <- shock.vector
	diff.irf <- irf

	for(i in lags:(irf.length-1)) {
        cirf  <- (co.pars[c(-1,-length(co.pars))] %*% irf[,i]) %*% t(ecc.pars)
		irf[,i+1] <- c(diff.irf[,i:(i-lags+1)]) %*% var.par.mat
		irf[,i+1] <-  irf[,i+1] + cirf
		diff.irf[,i+1] <-  irf[,i+1]
		irf[,i+1] <- irf[,i+1] + irf[,i]
	}

	return(t(irf[,lags:irf.length])) 
} #}}}
IRF <- function(var.sys, t = 10, shock = NULL, ord = NULL) #{{{
{
	endo.varnm <- var.sys$endo.varnm
	lags <- var.sys$lags
	nr.eq <- length(endo.varnm)
	var.par.mat <- var.sys$var.par.mat

	vcm <- var.sys$var$residCov
	if(!is.null(ord)) {
		P <- chol(vcm[ord,ord])[endo.varnm,endo.varnm]
	} else {
		P <- chol(vcm)
	}

	if(is.null(shock)) {
		shock.vector <- P[1,]
	} else {
		shock.vector <- P[shock,]
	}

	irf.length <- lags+t-1
	if(length(var.sys$co) > 1) {
		irf <- IRF.co.calc(var.par.mat, lags, irf.length, nr.eq, shock.vector, var.sys$ecc.pars, var.sys$co$co.pars)

	} else {
		irf <- IRFcalc(var.par.mat, lags, irf.length, nr.eq, shock.vector)

		# we only need to accumulate for series without cointegration. when cointegration is present accumlation is automatic
		unr.adf = var.sys$unr.adf
		if(!is.null(unr.adf)) {
			if(sum(unr.adf[,1] == "Yes") > 0) {		# are there any non-stationary series
				for(i in 1:nr.eq) {
					if(unr.adf[i,1] == "Yes") {
						irf[,i] <- cumsum(irf[,i])
					}
				}
			}
		}
	}
	colnames(irf) <- endo.varnm

	return(irf)
} #}}}
IRF.se <- function(var.sys, t = 10, nr.sim = 100, shock = NULL, ord = NULL) #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	nr.eq <- length(endo.varnm)

	# receptical for irfs
	catch.irf <- array(rep(NA,nr.eq*t*nr.sim),dim = c(t,nr.eq,nr.sim), dimnames = list(NULL,endo.varnm,paste('sim',1:nr.sim,sep = '')))

	for(i in 1:nr.sim) {
		catch.irf[,,i] <- IRF(make.sim.data(var.sys), t = t, shock = shock, ord = ord)
	}

	irf.se = matrix(rep(NA,nr.eq*t),ncol = nr.eq)
	for(i in 1:t) {
		for(j in 1:nr.eq) {
			irf.se[i,j] <- sd(catch.irf[i,j,])
		}
	}

	colnames(irf.se) <- endo.varnm

	return(irf.se)
} #}}}

GIRF <- function(var.sys, t = 10, shock = NULL) #{{{
{
	endo.varnm <- var.sys$endo.varnm
	lags <- var.sys$lags
	nr.eq <- length(endo.varnm)
	var.par.mat <- var.sys$var.par.mat

	vcm <- var.sys$var$residCov
	vcm <- vcm / sqrt(diag(vcm))
	nr.eq <- length(endo.varnm)

	if(is.null(shock)) {
		shock.vector <- vcm[1,]
	} else {
		shock.vector <- vcm[shock,]
	}

	irf.length <- lags+t-1
	if(length(var.sys$co) > 1) {
		irf <- IRF.co.calc(var.par.mat, lags, irf.length, nr.eq, shock.vector, var.sys$ecc.pars, var.sys$co$co.pars)
	} else {
		irf <- IRFcalc(var.par.mat, lags, irf.length, nr.eq, shock.vector)

		# we only need to accumulate for series without cointegration. when cointegration is present accumlation is automatic
		unr.adf = var.sys$unr.adf
		if(!is.null(unr.adf)) {
			if(sum(unr.adf[,1] == "Yes") > 0) {		# are there any non-stationary series
				for(i in 1:nr.eq) {
					if(unr.adf[i,1] == "Yes") {
						irf[,i] <- cumsum(irf[,i])
					}
				}
			}
		}
	}
	colnames(irf) <- endo.varnm

	return(irf)
} #}}}
GIRF.se <- function(var.sys, t = 10, nr.sim = 100, shock = NULL) #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	nr.eq <- length(endo.varnm)

	# receptical for irfs
	catch.irf <- array(rep(NA,nr.eq*t*nr.sim),dim = c(t,nr.eq,nr.sim), dimnames = list(NULL,endo.varnm,paste('sim',1:nr.sim,sep = '')))

	for(i in 1:nr.sim) {
		catch.irf[,,i] <- GIRF(make.sim.data(var.sys), t = t, shock = shock)
	}

	girf.se = matrix(rep(NA,nr.eq*t),ncol = nr.eq)
	for(i in 1:t) {
		for(j in 1:nr.eq) {
			girf.se[i,j] <- sd(catch.irf[i,j,])
		}
	}

	colnames(girf.se) <- endo.varnm

	return(girf.se)
} #}}}
XIRF <- function(var.sys, shock, t = 10, file = '') #{{{
{
	endo.varnm <- var.sys$endo.varnm
	lags <- var.sys$lags
	nr.eq <- length(endo.varnm)
	var.par.mat <- var.sys$var.par.mat
	exo.pars <- var.sys$exo.pars
	nr.shock <- length(shock)

	xirf <- array(rep(NA,nr.eq*t*nr.shock),dim = c(t,nr.eq,nr.shock), dimnames = list(NULL,endo.varnm,shock))

	for(sh in shock) {
		shock.vector <- exo.pars[,sh] 

		irf.length <- lags+t-1
		if(length(var.sys$co) > 1) {
			irf <- IRF.co.calc(var.par.mat, lags, irf.length, nr.eq, shock.vector, var.sys$ecc.pars, var.sys$co$co.pars)
		} else {
			irf <- IRFcalc(var.par.mat, lags, irf.length, nr.eq, shock.vector)

			# only need to accumulate for series without cointegration. when cointegration is present accumlation is automatic
			unr.adf = var.sys$unr.adf
			if(!is.null(unr.adf)) {
				if(sum(unr.adf[,1] == "Yes") > 0) {		# are there any non-stationary series
					for(i in 1:nr.eq) {
						if(unr.adf[i,1] == "Yes") {
							irf[,i] <- cumsum(irf[,i])
						}
					}
				}
			}
		}
		xirf[,,sh] <- irf
	}

	# saving output
	if(!file == '') {
		save(xirf,file = file)
	}

	return(xirf)
} #}}}
XIRF.se <- function(var.sys, shock, t = 10, nr.sim = 100, file = '') #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	nr.eq <- length(endo.varnm)
	nr.shock <- length(shock)

	# receptical for irfs
	catch.irf <- array(rep(NA,nr.eq*t*nr.sim*nr.shock),dim = c(t,nr.eq,nr.sim,nr.shock), dimnames = list(NULL,endo.varnm,paste('sim',1:nr.sim,sep = ''),shock))

	for(i in 1:nr.sim) {
		for(sh in shock) {
			catch.irf[,,i,sh] <- XIRF(make.sim.data(var.sys), sh, t = t)
		}
		# not sure the above is efficient, 1 sim to be used for all shocks would be faster
	}

	if(!file == '') {
		save(catch.irf,file = file)
	}

	xirf.se <- array(rep(NA,nr.eq*t*nr.shock),dim = c(t,nr.eq,nr.shock), dimnames = list(NULL,endo.varnm,shock))
	for(sh in shock) {
		for(i in 1:t) {
			for(j in 1:nr.eq) {
				xirf.se[i,j,sh] <- sd(catch.irf[i,j,,sh])
			}
		}
	}

	return(xirf.se)
} #}}}

XIRF.sku <- function(var.sys, shocks = NULL, shock.mat = NULL, t = 10, file = '') #{{{
{
	endo.varnm <- var.sys$endo.varnm
	lags <- var.sys$lags
	nr.eq <- length(endo.varnm)
	var.par.mat <- var.sys$var.par.mat
	exo.pars <- var.sys$exo.pars
	nr.shock <- dim(shock.mat)[2]

	xirf <- array(rep(NA,nr.eq*t*nr.shock),dim = c(t,nr.eq,nr.shock), dimnames = list(NULL,endo.varnm,shocks))

	for(sh in 1:nr.shock) {
		shock.vector <- rep(NA,nr.eq)
		names(shock.vector) <- endo.varnm

		for(xn in endo.varnm) {
			shock.vector[xn] <- exo.pars[xn,shock.mat[xn,sh]] 
		}

		irf.length <- lags+t-1
		if(length(var.sys$co) > 1) {
			irf <- IRF.co.calc(var.par.mat, lags, irf.length, nr.eq, shock.vector, var.sys$ecc.pars, var.sys$co$co.pars)
		} else {
			irf <- IRFcalc(var.par.mat, lags, irf.length, nr.eq, shock.vector)

			# only need to accumulate for series without cointegration. when cointegration is present accumlation is automatic
			unr.adf = var.sys$unr.adf
			if(!is.null(unr.adf)) {
				if(sum(unr.adf[,1] == "Yes") > 0) {		# are there any non-stationary series
					for(i in 1:nr.eq) {
						if(unr.adf[i,1] == "Yes") {
							irf[,i] <- cumsum(irf[,i])
						}
					}
				}
			}
		}
		xirf[,,sh] <- irf
	}

	# saving output
	if(!file == '') {
		save(xirf,file = file)
	}

	return(xirf)
} #}}}
XIRF.se.sku <- function(var.sys, shocks = NULL, shock.mat = NULL, t = 10, nr.sim = 100, file = '') #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	nr.eq <- length(endo.varnm)
	nr.shock <- dim(shock.mat)[2]

	# receptical for irfs
	catch.irf <- array(rep(NA,nr.eq*t*nr.sim*nr.shock),dim = c(t,nr.eq,nr.sim,nr.shock), dimnames = list(NULL,endo.varnm,paste('sim',1:nr.sim,sep = ''),shocks))

	for(i in 1:nr.sim) {
		catch.irf[,,i,] <- XIRF.sku(make.sim.data(var.sys), shocks = shocks, shock.mat = shock.mat, t = t)
	}

	if(!file == '') {
		save(catch.irf,file = file)
		return()
	}

	xirf.se <- array(rep(NA,nr.eq*t*nr.shock),dim = c(t,nr.eq,nr.shock), dimnames = list(NULL,endo.varnm,shocks))
	for(sh in shocks) {
		for(i in 1:t) {
			for(j in 1:nr.eq) {
				xirf.se[i,j,sh] <- sd(catch.irf[i,j,,sh])
			}
		}
	}

	return(xirf.se)
} #}}}

FEVD <- function(var.sys, t = 10, shocked = NULL, ord = NULL) #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	nr.eq <- length(endo.varnm)

	# receptical for irfs
	irfs <- matrix(rep(NA,nr.eq*t),ncol = nr.eq)
	colnames(irfs) <- endo.varnm

	for(s in endo.varnm) {
		irf <- IRF(var.sys,t = t, shock = s, ord = ord)[,shocked]
		irfs[,s] <- cumsum(irf^2)
	}

	for(i in 1:dim(irfs)[1]) {
		irfs[i,] <- irfs[i,] / sum(irfs[i,])
	}

	return(irfs)
} #}}}
FEVD.se <- function(var.sys, t = 10, nr.sim = 100, shocked = NULL, ord = NULL) #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	nr.eq <- length(endo.varnm)

	# receptical for irfs
	catch.fevd <- array(rep(NA,nr.eq*t*nr.sim),dim = c(t,nr.eq,nr.sim), dimnames = list(NULL,endo.varnm,paste('sim',1:nr.sim,sep = '')))

	for(i in 1:nr.sim) {
		catch.fevd[,,i] <- FEVD(make.sim.data(var.sys), t = t, shocked = shocked, ord = ord)
	}

	fevd.se = matrix(rep(NA,nr.eq*t),ncol = nr.eq)
	for(i in 1:t) {
		for(j in 1:nr.eq) {
			fevd.se[i,j] <- sd(catch.fevd[i,j,])
		}
	}

	colnames(fevd.se) <- endo.varnm

	return(fevd.se)
} #}}}
GFEVD <- function(var.sys, t = 10, shocked = NULL) #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	nr.eq <- length(endo.varnm)

	# receptical for irfs
	irfs <- matrix(rep(NA,nr.eq*t),ncol = nr.eq)
	colnames(irfs) <- endo.varnm

	for(s in endo.varnm) {
		irf <- GIRF(var.sys,t = t, shock = s)[,shocked]
		irfs[,s] <- cumsum(irf^2)
	}

	for(i in 1:dim(irfs)[1]) {
		irfs[i,] <- irfs[i,] / sum(irfs[i,])
	}

	return(irfs)
} #}}}
GFEVD.se <- function(var.sys, t = 10, nr.sim = 100, shocked = NULL) #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	nr.eq <- length(endo.varnm)

	# receptical for irfs
	catch.fevd <- array(rep(NA,nr.eq*t*nr.sim),dim = c(t,nr.eq,nr.sim), dimnames = list(NULL,endo.varnm,paste('sim',1:nr.sim,sep = '')))

	for(i in 1:nr.sim) {
		catch.fevd[,,i] <- GFEVD(make.sim.data(var.sys), t = t, shocked = shocked)
	}

	fevd.se = matrix(rep(NA,nr.eq*t),ncol = nr.eq)
	for(i in 1:t) {
		for(j in 1:nr.eq) {
			fevd.se[i,j] <- sd(catch.fevd[i,j,])
		}
	}

	colnames(fevd.se) <- endo.varnm

	return(fevd.se)
} #}}}

coint <- function(endo, exo = 0, lags = NULL, unr.adf = NULL, print = FALSE)  # {{{
{

	library(urca)

	dim.endo <- dim(endo)
	if(is.null(lags)) {
		diff.endo <- diff(endo)
		dim.diff.endo <- dim(diff.endo)
		max.lags <- round(10*log10(dim.diff.endo[1]) / dim.diff.endo[2])  
		lags <- ar(diff.endo, aic = TRUE, order.max = max.lags)$order
	}

	if(length(exo) > 1) {
		co <- ca.jo(endo, ecdet = 'trend', type = 'trace', K = lags + 1, dumvar = exo)
	} else {
		co <- ca.jo(endo, ecdet = 'trend', type = 'trace', K = lags + 1)
	}
	data <- cbind(co@x,seq(1,dim(co@x)[1]))

	ecc <- data %*% co@V[,1]		# this is correct - checked the ox code and eviews

	# lagging ecc
	ecc <- ecc[(lags+1):(length(ecc)-1)]

	co.int <- -mean(ecc)

	ecc <- matrix(ecc + co.int)
	colnames(ecc) <- "ecc"

	cv <- co@cval['r = 0  |','5pct']
	teststat <- co@teststat[dim.endo[2]]

	if(teststat > cv) {
		coint = "Yes"
	} else {
		coint = "No"
	}

	if(print == TRUE) {
		print(summary(co))
		print(paste("Cointegration present: ", coint))
	}
	co <- list(ecc = ecc, co.pars = co@V[,1])

	# updating the labels for the co.pars
	V.names <- names(co$co.pars)
	for(i in 1:length(V.names)) {
		V.names[i] <- gsub("(.l[0-9]+$)","",V.names[i])
	}
	co$co.pars <- c(co.int,co$co.pars)
	names(co$co.pars) <- c("(Intercept)",V.names)

	return(list(coint = coint, co = co))
} # }}}
quick.systemfit <- function(system, labels, data)  # {{{
{
	fit <- vector("list",2)
	names(fit) <- c("eq","residCov")
	nr.eq <- length(system)
	nr.obs <- dim(data)[1]
	fit$eq <- vector("list",nr.eq)
	eps <- matrix(rep(NA,nr.eq*nr.obs),ncol = nr.eq)
	for(i in 1:nr.eq) {
		reg <- lm(system[[i]], data = data)
		fit$eq[[i]] <- reg
		eps[,i] <- reg$residuals
	}

	nr.coef <- length(reg$coefficients)
	fit$residCov <- crossprod(eps) / (nr.obs - nr.coef)
	colnames(fit$residCov) <- labels
	rownames(fit$residCov) <- labels

	return(fit)
} #}}}

make.sim.data <- function(var.sys) #{{{
{
	# set base variables
	endo.varnm <- var.sys$endo.varnm
	lags <- var.sys$lags
	nr.eq <- length(endo.varnm)
	var.par.mat <- var.sys$var.par.mat
	intercept.par.mat <- var.sys$intercept.par.mat
	exo.pars <- var.sys$exo.pars
	nr.obs <- dim(var.sys$var$eq[[1]]$model)[1]
	endo <- var.sys$endo
	endo.org <- endo
	exo <- var.sys$exo
	to.diff <- FALSE
	system <- var.sys$system

	# residual matrix
	ceps = matrix(rep(NA,nr.eq*nr.obs),ncol = nr.eq)
	for(i in 1:nr.eq) {
		res = var.sys$var$eq[[i]]$residual
		ceps[,i] = res - mean(res) 
	}
	colnames(ceps) <- endo.varnm

	# adjusting data as appropriate based on unit root test information
	unr.adf = var.sys$unr.adf
	if(!is.null(unr.adf)) {
		if(sum(unr.adf[,'unitroot'] == 'Yes') > 0) {
			to.diff <- TRUE
			endo[2:dim(endo)[1],unr.adf[,'unitroot'] == 'Yes'] <- diff(endo[,unr.adf[,'unitroot'] == 'Yes'])
			endo <- endo[2:nr.obs,]
		}
	}

	# running the simulation
	init <- matrix(rep(0,nr.eq*lags),ncol = nr.eq) 
	t.endo = t(rbind(endo[1:lags,],matrix(rep(0,nr.eq*nr.obs),ncol = nr.eq)))

	# adding information from exogenous variables
	innov.exo <- 0
    if(length(exo) > 1) {
		innov.exo <- exo %*% t(exo.pars)
		if(to.diff == TRUE) {
			innov.exo <- innov.exo[-1,]
		}
	}

	# selecting shocks
	### innov = rbind(init,ceps)			# use this if you want to get the original data back
	innov <- rbind(init,ceps[sample(1:nr.obs,replace = TRUE),])

	# adding information from the exogenous variables
	innov <- innov + innov.exo
	sim <- t.endo

	if(length(var.sys$co) < 2) {

		for(j in lags:(nr.obs+lags-1)) {
			sim[,j+1] <- (c(sim[,j:(j-lags+1)]) %*% var.par.mat) + intercept.par.mat + innov[j+1,]
		}
		unr.adf = var.sys$unr.adf
		if(!is.null(unr.adf)) {
			if(sum(unr.adf[,'unitroot'] == 'Yes') > 0) {		# are there any non-stationary series
				sim <- cbind(endo.org[1,],sim)
				for(k in 1:nr.eq) {
					if(unr.adf[k,'unitroot'] == 'Yes') {
						sim[k,] <- cumsum(sim[k,])
					}
				}
			}
		}
		sim <- t(sim)
	} else {

		# controlling for cointegration in simulating the data
		ecc.pars <- var.sys$ecc.pars
		co.pars <- var.sys$co$co.pars

		diff.sim <- sim
		sim[,1:lags] <- t(endo.org[1:lags,]) + sim[,1:lags]
		sim <- cbind(endo.org[1,],sim)

		for(j in (lags+1):(nr.obs+lags)) {
			c.sim  <- (co.pars %*% c(1,sim[,j],j)) %*% t(ecc.pars) 
			sim[,j+1] <- (c(diff.sim[,(j-1):(j-lags)]) %*% var.par.mat) + intercept.par.mat + innov[j,]
			sim[,j+1] <-  sim[,j+1] + c.sim
			diff.sim[,j] <-  sim[,j+1]
			sim[,j+1] <- sim[,j+1] + sim[,j]
		}
		sim <- t(sim)
	}

	if(length(var.sys$co) > 1) {
		#################################################################################
		### try to find a way to bring in the exogenous variables in a parsimonious way
		#################################################################################
		### co.sim <- coint(sim, exo = exo, lags = lags, unr.adf = unr.adf)
		co.sim <- coint(sim, lags = lags, unr.adf = unr.adf)
		if(var.sys$subset == TRUE) {
			return(var.sys.subset(sim, system = system, exo = exo, co = co.sim$co, unr.adf = unr.adf, lags = lags, sim = TRUE))
		} else {
			return(var.sys(sim, exo = exo, co = co.sim$co, unr.adf = unr.adf, lags = lags, sim = TRUE))
		}
	} else {
		if(var.sys$subset == TRUE) {
			return(var.sys.subset(sim, system = system, exo = exo, unr.adf = unr.adf, lags = lags, sim = TRUE))
		} else {
			return(var.sys(sim, exo = exo, unr.adf = unr.adf, lags = lags, sim = TRUE))
		}
	}
} #}}}
var.sys <- function(endo, exo = 0, co = 0, lags = 1, unr.adf = NULL, sim = FALSE)  # {{{
{

	# saving the original endogenous data
	endo.org = endo

	# getting the names of the endogenous variables
	dep.varnm <- colnames(endo)

	# getting the data dimensions
    edim <- dim(endo)

	# adjusting data as appropriate based on unit root test information
	if(!is.null(unr.adf)) {
		if(sum(unr.adf[,1] == "Yes") > 0) {
			endo[2:edim[1],unr.adf[,1] == "Yes"] <- diff(endo[,unr.adf[,1] == "Yes"])
			endo <- endo[2:edim[1],]
			edim <- dim(endo)
		}
	}

	# adding exogenous variables
    if (length(exo) > 1) {
		if(!is.matrix(exo)) {
			stop("\nThe exogenous variables have not been put into a matrix.\n")		
		}
		exo.varnm <- paste("exo.", colnames(exo),sep = "")

		# making the lagged data with variable names
		if(!is.null(unr.adf) && (sum(unr.adf[,1] == "Yes") > 0)) {
				var.data <- data.frame(embed(endo,lags + 1), exo[-1:-(lags+1),])
		} else {
			var.data <- data.frame(embed(endo,lags + 1), exo[-1:-lags,])
		}

		endo.varnm <- paste(paste(rep(paste("endo.",dep.varnm, sep = ""),lags),".lag",sep = ""),rep(1:lags, each = edim[2]), sep = "")
		colnames(var.data) <- c(dep.varnm, endo.varnm, exo.varnm)

	} else {
		# making the lagged data with variable names
		var.data <- data.frame(embed(endo,lags + 1))
		endo.varnm <- paste(paste(rep(paste("endo.",dep.varnm, sep = ""),lags),".lag",sep = ""),rep(1:lags, each = edim[2]), sep = "")
		colnames(var.data) <- c(dep.varnm, endo.varnm)
	}

	# adding error correction terms
    if (length(co) > 1) {
		ecc <- co$ecc
		if(!is.matrix(ecc)) {
			stop("\nThe error correction terms have not been put into a matrix.\n")		
		}
		ecc.varnm <- paste("ecc.", colnames(ecc),sep = "")

		# making the lagged data with variable names
		update.colnames <- c(colnames(var.data),ecc.varnm)
		var.data <- data.frame(var.data,ecc)
		colnames(var.data) <- update.colnames

	} else {
		ecc <- 0
	}


	# creating a list of equation as input to systemfit
	system <- vector("list",edim[2])
	names(system) <- dep.varnm
	for(i in 1:edim[2]) { 
		if(length(exo) > 1) {
			if(length(ecc) > 1) {
				system[[i]] <- as.formula(paste(paste(dep.varnm[i]," ~ "), paste(endo.varnm, collapse = " + ")," + ", paste(exo.varnm, collapse = " + ")," + ", paste(ecc.varnm, collapse = " + ")))
			} else {
				system[[i]] <- as.formula(paste(paste(dep.varnm[i]," ~ "), paste(endo.varnm, collapse = " + ")," + ", paste(exo.varnm, collapse = " + ")))
			}
		} else {
			if(length(ecc) > 1) {
				system[[i]] <- as.formula(paste(paste(dep.varnm[i]," ~ "), paste(endo.varnm, collapse = " + ")," + ", paste(ecc.varnm, collapse = " + ")))
			} else {
				system[[i]] <- as.formula(paste(paste(dep.varnm[i]," ~ "), paste(endo.varnm, collapse = " + ")))
			}
		}
	}

	# estimating the system
	if(sim == FALSE) {
		library(systemfit)
		fit <- systemfit(system, method = "OLS", data = var.data)
	} else {
		fit <- quick.systemfit(system, labels = dep.varnm, data = var.data)
	}

	# creating the varpars matrix
	var.par.mat <- matrix(rep(0,length(endo.varnm)*edim[2]),ncol = edim[2])
	rownames(var.par.mat) <- endo.varnm
	colnames(var.par.mat) <- dep.varnm
	intercept.par.mat <- matrix(rep(0,length(dep.varnm)),ncol = length(dep.varnm))
	colnames(intercept.par.mat) <- dep.varnm
	for(i in 1:edim[2]) {
		coef <- fit$eq[[i]]$coefficients
		coef.varnm <- names(coef)[grep("^endo.",names(coef))]
		var.par.mat[coef.varnm,i] <- coef[coef.varnm]
		intercept.par.mat[i] <- coef['(Intercept)']
	}

	# creating the exo matrix if needed 
	if(length(exo) > 1) {
		nr.exo <- dim(exo)[2]
		exo.pars <- matrix(rep(0,length(dep.varnm)*nr.exo),ncol = nr.exo)
		colnames(exo.pars) <- exo.varnm
		rownames(exo.pars) <- dep.varnm
		for(i in 1:length(dep.varnm)) {
			coef <- fit$eq[[i]]$coefficients
			coef.varnm <- names(coef)[grep("^exo.",names(coef))]
			exo.pars[i,coef.varnm] <- coef[coef.varnm]
		}
	} else {
		exo.pars <- 0
	}

	# creating the ecc matrix if needed 
	if(length(co) > 1) {
		ecc.pars <- matrix(rep(0,length(dep.varnm)),ncol = 1)
		colnames(ecc.pars) <- "ecc"
		rownames(ecc.pars) <- dep.varnm
		for(i in 1:length(dep.varnm)) {
			coef <- fit$eq[[i]]$coefficients
			coef.varnm <- names(coef)[grep("^ecc.",names(coef))]
			ecc.pars[i] <- coef[coef.varnm]
		}
	} else {
		ecc.pars <- 0
	}

	return(list(var = fit, subset = FALSE, system = system, endo = endo.org, exo = exo, endo.varnm = dep.varnm, var.pars = endo.varnm, var.par.mat = var.par.mat, intercept.par.mat = intercept.par.mat, exo.pars = exo.pars, lags = lags, unr.adf = unr.adf, co = co, ecc.pars = ecc.pars))

} #}}}
var.sys.subset <- function(endo, system = NULL, lower = ~ 1, omit = "", direction = 'both', exo = 0, co = 0, lags = 1, max.lag = NULL, unr.adf = NULL, sim = FALSE, var.subset.file = '', load.subset = FALSE)  # {{{
{
	# saving the original endogenous data
	endo.org = endo

	# getting the names of the endogenous variables
	dep.varnm <- colnames(endo)

	# getting the data dimensions
    edim <- dim(endo)

	# adjusting data as appropriate based on unit root test information
	if(!is.null(unr.adf)) {
		if(sum(unr.adf[,1] == "Yes") > 0) {
			endo[2:edim[1],unr.adf[,1] == "Yes"] <- diff(endo[,unr.adf[,1] == "Yes"])
			endo <- endo[2:edim[1],]
			edim <- dim(endo)
		}
	}

	# setting the max.lag variable if not provided
	if(is.null(max.lag)) {
		max.lag = round(10*log10(edim[1]) / edim[2])  
	}

	# adding exogenous variables
    if (length(exo) > 1) {
		if(!is.matrix(exo)) {
			stop("\nThe exogenous variables have not been put into a matrix.\n")		
		}
		exo.varnm <- paste("exo.", colnames(exo),sep = "")

		# making the lagged data with variable names
		if(!is.null(unr.adf) && (sum(unr.adf[,1] == "Yes") > 0)) {
			var.data <- data.frame(embed(endo,lags + 1), exo[-1:-(lags+1),])
		} else {
			var.data <- data.frame(embed(endo,lags + 1), exo[-1:-lags,])
		}

		endo.varnm <- paste(paste(rep(paste("endo.",dep.varnm, sep = ""),lags),".lag",sep = ""),rep(1:lags, each = edim[2]), sep = "")
		colnames(var.data) <- c(dep.varnm, endo.varnm, exo.varnm)

	} else {
		# making the lagged data with variable names
		var.data <- data.frame(embed(endo,lags + 1))
		endo.varnm <- paste(paste(rep(paste("endo.",dep.varnm, sep = ""),lags),".lag",sep = ""),rep(1:lags, each = edim[2]), sep = "")
		colnames(var.data) <- c(dep.varnm, endo.varnm)
	}

	# adding error correction terms
    if (length(co) > 1) {
		ecc <- co$ecc
		if(!is.matrix(ecc)) {
			stop("\nThe error correction terms have not been put into a matrix.\n")		
		}
		ecc.varnm <- paste("ecc.", colnames(ecc),sep = "")

		# making the lagged data with variable names
		update.colnames <- c(colnames(var.data),ecc.varnm)
		rows.ecc <- dim(ecc)[1]
		rows.vardata <- dim(var.data)[1]
		diff.rows <- rows.ecc - rows.vardata
		if(diff.rows > 0) {
			var.data <- data.frame(var.data,ecc[-1:-diff.rows,])
		} else {
			var.data <- data.frame(var.data,ecc)
		}
		colnames(var.data) <- update.colnames

	} else {
		ecc <- 0
	}

	# storage for autocorrelation test
	ac.test <- vector("list",edim[2])
	names(ac.test) <- dep.varnm
	ac.test.fail = FALSE

	if(sim == FALSE) {

		# creating a list of equation as input to systemfit
		system <- vector("list",edim[2])
		names(system) <- dep.varnm
		for(i in 1:edim[2]) { 
			if(length(exo) > 1) {
				if(length(ecc) > 1) {
					system[[i]] <- paste(paste(dep.varnm[i]," ~ "), paste(endo.varnm, collapse = " + ")," + ", paste(exo.varnm, collapse = " + ")," + ", paste(ecc.varnm, collapse = " + "))
				} else {
					system[[i]] <- paste(paste(dep.varnm[i]," ~ "), paste(endo.varnm, collapse = " + ")," + ", paste(exo.varnm, collapse = " + "))
				}
			} else {
				if(length(ecc) > 1) {
					system[[i]] <- paste(paste(dep.varnm[i]," ~ "), paste(endo.varnm, collapse = " + ")," + ", paste(ecc.varnm, collapse = " + "))
				} else {
					system[[i]] <- paste(paste(dep.varnm[i]," ~ "), paste(endo.varnm, collapse = " + "))
				}
			}

			if(length(omit) == 1) {
				if(!omit == "") {
					system[[i]] <- paste(system[[i]],omit)
				}
			} else {
				if(!omit[[i]] == "") {
					system[[i]] <- paste(system[[i]],omit[[i]])
				}
			}

			system[[i]] <- as.formula(system[[i]])
		}

		# finding the most appropriate subset models
		subset <- vector("list",edim[2])
		names(subset) <- dep.varnm

		# creating a list of 'lower' equations
		if(!is.list(lower)) {
			lower.single <- lower
			lower <- vector("list",edim[2])
			for(i in 1:edim[2]) {
				lower[[i]] <- lower.single
			}
		}

		if(load.subset == FALSE) {

			for(i in 1:edim[2]) { 

				fit <- lm(system[[i]], data = var.data)
				sys.step <- step(fit, scope = list(lower = lower[[i]]), direction = direction, trace = FALSE)

				# testing for residual autocorrelation
				ac.test[[i]] <- Box.test(sys.step$residuals, lag = 4, type = "Ljung-Box")$p.value

				# if you couldn't calculate the ac.test there is probably a problem
				if(is.na(ac.test[[i]])) { ac.test[[i]] = 0 }

				# if there is residual autocorrelation call the function again but allow for longer lags
				if(ac.test[[i]] < .05) {
					if(lags < max.lag) {
						cat("--- Upping the number of lags to ",lags+1," - maxlag is ",max.lag,"---\n")
						# calling back the function with more lags to try and get rid of AC
						### return(var.sys.subset(endo.org, exo = exo, co = co, lower = lower, lags = lags + 1, max.lag = max.lag, unr.adf = unr.adf, sim = FALSE))
						return(NULL)
					} else {
						cat("--- At lag", max.lag, " there is still residual correlation in the model for ",dep.varnm[i]," ---\n")
						ac.test.fail = TRUE
					}
				}

				# getting the subset formula
				cat("--- Fitted model for ",dep.varnm[i]," ---\n")
				subset[[i]] <- formula(sys.step$call[2])
			}
		} else {
			cat(paste("--- Loading var-subset system from file ---\n"))
			load(var.subset.file)
			subset = sub$subset
		}
	} else {
		subset = system
	}

	# estimating the system
	if(sim == FALSE) {
		if(load.subset == FALSE) {
			if(!var.subset.file == '') {
				sub <- list('vector')
				sub$subset <- subset
				sub$var.data <- var.data
				save(sub,file = var.subset.file)
			}
		}
		library(systemfit)
		est.method = "SUR"
		for(try.sur in 1:10) {
			# looping through several itterations in case the SUR method crashes
			cat("--- Using SUR for systemfit, attempt ",try.sur,"---\n")
			fit <- try(systemfit(subset, method = est.method, data = var.data))
			if(!class(fit) == "try-error") {
				break
			}
		}
		if(class(fit) == "try-error") {
			est.method = "OLS"
			cat("--- Moving to OLS estimation for systemfit ---\n")
			fit <- systemfit(subset, method = est.method, data = var.data)
		}
	} else {
		est.method = "OLS.SIM"
		fit <- quick.systemfit(subset, labels = dep.varnm, data = var.data)
	}

	# creating the varpars matrix
	var.par.mat <- matrix(rep(0,length(endo.varnm)*edim[2]),ncol = edim[2])
	rownames(var.par.mat) <- endo.varnm
	colnames(var.par.mat) <- dep.varnm
	intercept.par.mat <- matrix(rep(0,length(dep.varnm)),ncol = length(dep.varnm))
	colnames(intercept.par.mat) <- dep.varnm

	for(i in 1:edim[2]) {
		coef <- fit$eq[[i]]$coefficients
		coef.varnm <- names(coef)[grep("^endo.",names(coef))]
		var.par.mat[coef.varnm,i] <- coef[coef.varnm]
		intercept.par.mat[i] <- coef['(Intercept)']
	}

	# creating the exo matrix if needed 
	if(length(exo) > 1) {
		nr.exo <- dim(exo)[2]
		exo.pars <- matrix(rep(0,length(dep.varnm)*nr.exo),ncol = nr.exo)
		colnames(exo.pars) <- exo.varnm
		rownames(exo.pars) <- dep.varnm
		for(i in 1:length(dep.varnm)) {
			coef <- fit$eq[[i]]$coefficients
			coef.varnm <- names(coef)[grep("^exo.",names(coef))]
			if(length(coef.varnm) > 0) {
				exo.pars[i,coef.varnm] <- coef[coef.varnm]
			}
		}
	} else {
		exo.pars <- 0
	}

	# creating the ecc matrix if needed 
	if(length(co) > 1) {
		ecc.pars <- matrix(rep(0,length(dep.varnm)),ncol = 1)
		colnames(ecc.pars) <- "ecc"
		rownames(ecc.pars) <- dep.varnm
		for(i in 1:length(dep.varnm)) {
			coef <- fit$eq[[i]]$coefficients
			coef.varnm <- names(coef)[grep("^ecc.",names(coef))]
			if(length(coef.varnm) > 0) {
				ecc.pars[i] <- coef[coef.varnm]
			}
		}
	} else {
		ecc.pars <- 0
	}

	return(list(var = fit, subset = TRUE, system = subset, endo = endo.org, exo = exo, endo.varnm = dep.varnm, var.pars = endo.varnm, var.par.mat = var.par.mat, intercept.par.mat = intercept.par.mat, exo.pars = exo.pars, lags = lags, unr.adf = unr.adf, co = co, ecc.pars = ecc.pars, ac.test = ac.test, est.method = est.method))

} #}}}
