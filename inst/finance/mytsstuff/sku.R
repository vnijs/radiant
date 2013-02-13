setwd("~/op/skus/code/")
### setwd("/Volumes/129.105.198.77/op/skus/code/")

# sourcing the base code
source('ts_base.R')

filenames <- c("ana_headache","ana_arth","ana_child","ana_fem","ana_pm","cer_cereal","cer_muesli","cso_broth","cso_soup","did_auto","did_detergent","fsf_sheets","oat_hotcereal","ptw_papertowel","rfj_drink","rfj_juice","tbr_toothbrush","tna_tuna","tti_toilettissue")
### filenames <- c("tti_toilettissue")

# setting model specs
imp.nr <- "52"
nrstores <- 85
lags <- 6
max.maxlag <- 13 
mid.maxlag <- 11 
min.maxlag <- 9 
xtra.endo.lags <- 3
t <- 26
nr.sim <- 250
nr.obs <- 150			# minimum number of obs to use
endo.varnm <- c("traffic","vol1","vol2","vol3","vol4","price.vol1","price.vol2","price.vol3","wp.vol1","wp.vol2","wp.vol3")
out.dir <- 'output2012'

skipOrNot <- function(data,nr.obs)  #{{{
{
	ok <- complete.cases(data)
	obs <- 1:dim(data)[1]
	obs <- obs[ok]
	if(length(obs) == 0) {
		# in case data is all missing
		start <- end <- 0
	} else {
		start <- min(obs)
		end <- max(obs)
	}

	if(length(obs) < nr.obs) {
		skip.string = "Skipping this data set since there is an insufficient number of observations"
	} else if(end - start > (length(obs)-1)) {
		skip.string = "Skipping this data set since there are missing values not at the beginning/end of the dataseries"
	} else {
		skip.string = ""
	}
	return(data.frame(start,end,skip.string))
} #}}}

cvar <- function(data)  #{{{
{
	m <- mean(data)
	s <- sd(data)
	if(s > 0) {
		return(m/s)
	} else {
		return(0)
	}
} #}}}

nr.nonskip <- 0
nr.skip <- 0

for(fn in filenames) { # {{{
	cat.nonskip <- 0
	cat.skip <- 0

	for(j in 1:nrstores) {
		# if multiple instances of this program are running at the same time {{{
		fid <- sprintf("./%s/pgfile/pgfile_%s_%s.txt",out.dir,fn,j)
		if(file.exists(fid) == TRUE) {
			next
		} else {    
			message <- sprintf("Running --- %s",date(),'\n')
			write(message,file = fid)
		}

		# keeping track of progress
		cat("=================================================\n")
		cat("Working on category",fn,",store",j,"\n")
		cat("Date ---", date(),'\n')
		cat("=================================================\n")
		#}}}
		# getting data {{{
		cat("=== Getting the data ===\n")
		file <- sprintf("./rdadata%s/%s_data_%s.rda",imp.nr,fn,j)
		load(file)

		# assigning data chunks
		endo <- data[,endo.varnm]
		exo <- data[,14:dim(data)[2]]
		exo.varnm <- colnames(exo)
		exo <- matrix(exo,ncol = dim(exo)[2])
		colnames(exo) <- exo.varnm

		# skip or not
		skip <- skipOrNot(data,nr.obs)
		if(skip['skip.string'] != "") {
			fid <- sprintf("./%s/pgfile/pgfile_%s_%s.txt",out.dir,fn,j)
			message <- sprintf("%s\nFinished --- %s\n",as.character(skip[['skip.string']]),date())
			write(message,file = fid, append = TRUE)
			cat("---",as.character(skip[['skip.string']]),"---\n")
			cat.skip <- cat.skip + 1
			next
		} else {
			cat.nonskip <- cat.nonskip + 1
			start <- skip[['start']]
			end <- skip[['end']]
		}

		# updating start and ending points for the data
		endo <- endo[start:end,]
		exo <- exo[start:end,]

		# adding lags of the promo variables
		promo.lag.varnm <- c('promo11','promo12','promo13','promo21','promo22','promo23')
		promo.lag <- rbind(rep(0,6),embed(exo[,promo.lag.varnm],2)[,-c(1:6)])
		promo.lag.varnm <- paste(promo.lag.varnm,'.lag',sep = '')
		colnames(promo.lag) <- promo.lag.varnm
		exo <- cbind(exo[,1:6],promo.lag,exo[,7:dim(exo)[2]])
		exo.varnm <- colnames(exo)

		# adjusting the maxlag for the number of observations available
		if(dim(endo)[1] < 250) {
			maxlag <- min.maxlag
		} else if(dim(endo)[1] > 350) {
			maxlag <- max.maxlag
		} else {
			maxlag <- mid.maxlag
		}

		# all exo shocks
		exos <- exo[,35:dim(exo)[2]]
		### sel <- apply(exos[(maxlag+1):dim(exos)[1],],2,cvar) > 0.01
		sel <- apply(exos[(maxlag+2):dim(exos)[1],1:(dim(exos)[2]/2)],2,cvar) > 0.01		# look only at the pulse dummies. if they are not ok the step dummies should also be removed
		exos <- exos[,c(sel,sel)]
		exos.varnm <- colnames(exos)

		# main exogenous variables
		exo <- exo[,1:34]
		exo.varnm <- exo.varnm[1:34]

		# npi and rem identifiers
		exos.varnm.npi.s.p <- exos.varnm[regexpr("^npi\\d{1,4}s.p",exos.varnm, perl = TRUE) == 1]
		exos.varnm.rem.s.p <- exos.varnm[regexpr("^rem\\d{1,4}s.p",exos.varnm, perl = TRUE) == 1]
		exos.varnm.npi.s.s <- exos.varnm[regexpr("^npi\\d{1,4}s.s",exos.varnm, perl = TRUE) == 1]
		exos.varnm.rem.s.s <- exos.varnm[regexpr("^rem\\d{1,4}s.s",exos.varnm, perl = TRUE) == 1]
		exos.varnm.npi.d.p <- exos.varnm[regexpr("^npi\\d{1,4}d.p",exos.varnm, perl = TRUE) == 1]
		exos.varnm.rem.d.p <- exos.varnm[regexpr("^rem\\d{1,4}d.p",exos.varnm, perl = TRUE) == 1]
		exos.varnm.npi.d.s <- exos.varnm[regexpr("^npi\\d{1,4}d.s",exos.varnm, perl = TRUE) == 1]
		exos.varnm.rem.d.s <- exos.varnm[regexpr("^rem\\d{1,4}d.s",exos.varnm, perl = TRUE) == 1]
		#}}}
		# saving means for the endogenous variables, used in calculation of elasticities {{{
		# only run if all endo variables are included, i.e., don't run if this is a test
		if(length(endo.varnm) == 11) {
			mean.endo <- sapply(data.frame(endo),mean)
			save(mean.endo,file = sprintf("./%s/elasticity_convert/means_%s_%s.rda",out.dir,fn,j))

			# saving measures for premiums
			p1_prem <- mean(endo[,'price.vol1']-endo[,'price.vol3'])
			p2_prem <- mean(endo[,'price.vol2']-endo[,'price.vol3'])
			vol1_prem <- mean(endo[,'vol1']-endo[,'vol3'])
			vol2_prem <- mean(endo[,'vol2']-endo[,'vol3'])
			marg1_prem <- mean((endo[,'price.vol1']-endo[,'wp.vol1']) - (endo[,'price.vol3']-endo[,'wp.vol3']))
			marg2_prem <- mean((endo[,'price.vol2']-endo[,'wp.vol2']) - (endo[,'price.vol3']-endo[,'wp.vol3']))
			rev1_prem <- mean((endo[,'price.vol1']*endo[,'vol1']) - (endo[,'price.vol3']*endo[,'vol3']))
			rev2_prem <- mean((endo[,'price.vol2']*endo[,'vol2']) - (endo[,'price.vol3']*endo[,'vol3']))
			prof1_prem <- mean(((endo[,'price.vol1']-endo[,'wp.vol1'])*endo[,'vol1']) - ((endo[,'price.vol3']-endo[,'wp.vol3'])*endo[,'vol3']))
			prof2_prem <- mean(((endo[,'price.vol2']-endo[,'wp.vol2'])*endo[,'vol2']) - ((endo[,'price.vol3']-endo[,'wp.vol3'])*endo[,'vol3']))
			mean.premiums <- data.frame(p1_prem,p2_prem,vol1_prem,vol2_prem,marg1_prem,marg2_prem,rev1_prem,rev2_prem,prof1_prem,prof2_prem)
			save(mean.premiums,file = sprintf("./%s/elasticity_convert/prem_%s_%s.rda",out.dir,fn,j))
		}

		#}}}
		# running unitroot tests {{{
		cat("=== Running the unit root tests ===\n")

		# adding the npi/rem exos to the adf test
		unr.exos.varnm <- c(exos.varnm.npi.s.p,exos.varnm.rem.s.p,exos.varnm.npi.d.p,exos.varnm.rem.d.p)
		unr.exo <- cbind(exo,exos[,unr.exos.varnm])
		unr.exo.varnm <- c(colnames(exo),unr.exos.varnm)
		colnames(unr.exo) <- unr.exo.varnm

		unr.adf.file = sprintf("./%s/unr.adf/unr.adf_%s_%s.rda",out.dir,fn,j)
		if(file.exists(unr.adf.file) == FALSE) {
			unr.adf <- matrix(rep('',5*length(endo.varnm)), ncol = 5)
			colnames(unr.adf) <- c('unitroot','drift','trend','SB','SB-date')
			rownames(unr.adf) <- endo.varnm

			for(nm in endo.varnm) {
				cat(paste("--- Unit-root test for series ",nm,'---\n'))
				unr.adf[nm,1:3] <- adf.loop(ts(endo[,nm]), exo = unr.exo, print = FALSE)
			}

			for(nm in endo.varnm) {
				if(unr.adf[nm,'unitroot'] == "Yes") {
					cat(paste("--- Structural-Break Unit-root test for series ",nm,'---\n'))
					# you can't put the npi/rem dummies into the SB test since the dummies would clash with the SB-dummies
					unr.adf[nm,c('SB','SB-date')] <- za.loop(ts(endo[,nm]), exo = exo, print = FALSE)
					if(unr.adf[nm,'SB'] == "Yes") {
						# resetting to no-unitroot if the za test shows a structural break
						unr.adf[nm,'unitroot'] = "No"
					}
				} else {
					unr.adf[nm,c('SB','SB-date')] <- c("",NA)
				}
			 }

			save(unr.adf,file = unr.adf.file)
		} else {
			cat(paste("--- Loading unit-root test information from file ---\n"))
			load(unr.adf.file)
		}

		# adding structural break dummies as needed
		for(sb in rownames(unr.adf)) {
			if(!is.na(unr.adf[sb,'SB-date'])) {
				sbreak <- rep(0,dim(exo)[1])
				sbreak[as.integer(unr.adf[sb,'SB-date']):dim(exo)[1]] <- 1

				# avoiding adding break that we don't need (i.e., already in exos)
				mc <- summary(lm(sbreak ~ exos + exo))
				if(mc['r.squared'] < .5) {
					exo <- cbind(exo,sbreak)
					exo.varnm <- colnames(exo)
					exo.varnm[length(exo.varnm)] <- paste('sb.',sb,sep='')
					colnames(exo) <- exo.varnm
				}
			}
		}

		# adding a trend as needed
		if(sum(unr.adf[,'trend'] == 'Yes') > 0) {
			trend <- start:end
			exo <- cbind(exo,trend)
			exo.varnm <- exo.varnm
		}

		#}}}
		# checking for cointegration {{{
		cat("=== Checking for cointegration ===\n")
		co.test = 0
		if(sum(unr.adf[,'unitroot'] == "Yes") == length(endo.varnm)) {
			cat("--- Test for cointegration required ---\n")
			co <- coint(endo, exo = exo, lags = lags, unr.adf = unr.adf, print = FALSE)
			if(co$coint == "Yes") {
				co.test <- co$co
			}
		} else {
			cat("--- No test for cointegration required ---\n")
		}
		# }}}
		# Estimating the VARX/VECMX model {{{
		cat("=== Estimating the VARX/VECMX model ===\n")
		var.sys.file = sprintf("./%s/var.sys/var.sys_%s_%s.rda",out.dir,fn,j)
		var.subset.file = sprintf("./%s/subset/subset_%s_%s.rda",out.dir,fn,j)

		# making the list of 'lower' equations
		lower <- vector("list",length(endo.varnm)) 
		names(lower) <- endo.varnm
		omit <- vector("list",length(endo.varnm)) 
		names(omit) <- endo.varnm

		# matrix to store the exogenous shocks to step and/or pulse dummies
		shock.mat.cols <- length(c(exos.varnm.npi.s.p,exos.varnm.rem.s.p))
		shock.mat <- matrix(rep(NA,length(endo.varnm)*shock.mat.cols),ncol = shock.mat.cols)
		rownames(shock.mat) <- endo.varnm

		for(xn in endo.varnm) {

			if(unr.adf[xn,1] == 'Yes') {
				lower.add <- c(exos.varnm.npi.s.p,exos.varnm.rem.s.p)
				omit.add <- c(exos.varnm.npi.s.s,exos.varnm.rem.s.s,exos.varnm.npi.d.s,exos.varnm.rem.d.s)
			} else {
				lower.add <- c(exos.varnm.npi.s.s,exos.varnm.rem.s.s)
				omit.add <- c(exos.varnm.npi.s.p,exos.varnm.rem.s.p,exos.varnm.npi.d.p,exos.varnm.rem.d.p)
			}
			shock.mat[xn,] <- paste("exo.",lower.add,sep = "")

			if(length(lower.add) > 0) {
				lower.formula <- paste(" ~ 1 + ",paste(paste("exo.",lower.add,sep = ""), collapse = " + "))
			}
			if(class(co.test) == 'list') {
				lower.formula <- paste(lower.formula, "+ ecc.ecc")
			}
			if(length(omit.add) > 0) {
				omit[[xn]] <- paste("-",paste(paste("exo.",omit.add,sep = ""), collapse = " - "))
			} else {
				omit[[xn]] = ""
			}

			# removing some endo lags so we can have more lags for a specific endogenous variable in its 'own' equation
			omit.endo.varnm <- endo.varnm[!endo.varnm == xn]

			omit[[xn]] = paste(omit[[xn]],paste("-",paste(paste(paste("endo.",omit.endo.varnm,sep = ""),".lag",sep = ''), rep((1+lags-xtra.endo.lags):lags, each = length(omit.endo.varnm)),sep = ''), collapse = " "), collapse = " ")

			lower[[xn]] <- as.formula(lower.formula)
		}

		if(file.exists(var.sys.file) == FALSE) {

			# adding the exos variables to the exo matrix
			exo <- cbind(exo,exos)
			ret <- NULL
			ret.lags <- lags

			# expanding on the number of lags if necessary
			while(is.null(ret)) {
				if(file.exists(var.subset.file) == FALSE) {
					ret <- var.sys.subset(endo, lower = lower, omit = omit, direction = "both", co = co.test, exo = exo, lags = ret.lags, max.lag = maxlag, unr.adf = unr.adf, var.subset.file = var.subset.file, load.subset = FALSE)
				} else {
					ret <- var.sys.subset(endo, lower = lower, omit = omit, direction = "both", co = co.test, exo = exo, lags = ret.lags, max.lag = maxlag, unr.adf = unr.adf, var.subset.file = var.subset.file, load.subset = TRUE)
				}
				ret.lags <- ret.lags + 1
			}
			save(ret,file = var.sys.file)
		} else {
			load(var.sys.file)
		}

		#}}}
		# Estimating the XIRF with standard errors {{{
		cat("=== Estimating the XIRF with standard errors ===\n")

		shocks = shock.mat[1,]
		for(i in 1:shock.mat.cols) {
			es <- sub('.s$','',shocks[i])
			es <- sub('.p$','',es)
			es <- sub('^exo.','',es)
			shocks[i] <- es
		}

		xirf.file = sprintf("./%s/xirf/xirf_%s_%s.rda",out.dir,fn,j)
		XIRF.sku(ret, shocks = shocks, shock.mat = shock.mat, t = t, file = xirf.file)

		xirf.se.file = sprintf("./%s/xirf.se/xirf.se_%s_%s.rda",out.dir,fn,j)
		XIRF.se.sku(ret, shocks = shocks, shock.mat = shock.mat, t = t, nr.sim = nr.sim, file = xirf.se.file)
		#}}}
		# writting information about finish time to file #{{{
		fid <- sprintf("./%s/pgfile/pgfile_%s_%s.txt",out.dir,fn,j)
		message <- sprintf("Finished --- %s",date(),'\n')
		write(message,file = fid, append = TRUE)
		#}}}
	}

	nr.nonskip <- nr.nonskip + cat.nonskip
	nr.skip <- nr.skip + cat.skip

	cat("=================================================\n")
	cat("Skipping info for category",fn,"\n")
	cat("Skipped ---", cat.skip,'\n')
	cat("Not skipped ---", cat.nonskip,'\n')
	cat("Date ---", date(),'\n')
	cat("=================================================\n")
	

} #}}}

nr.total <- nr.skip + nr.nonskip
cat("=================================================\n")
cat("Skipping info across all categories\n")
cat("Skipped ---", nr.skip,'(',nr.skip/nr.total,')\n')
cat("Not skipped ---", nr.nonskip,'(',nr.nonskip/nr.total,')\n')
cat("Date ---", date(),'\n')
cat("=================================================\n")
