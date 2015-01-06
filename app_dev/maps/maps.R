setwd("~/Desktop/GitHub/radiant_dev/app_dev/maps")
dat <- read.csv("car_survey.csv")

nr.dim <- 2

	dis <- dat$Distance
	id1 <- as.character(dat$ID1)
	id2 <- as.character(dat$ID2)

	lab <- unique(c(id1,id2))
	nr.lev <- length(lab)

	lower <- (nr.lev * (nr.lev -1)) / 2
	nr.obs <- length(dis)

	co.dist <- diag(length(lab))
	if(lower == nr.obs) {
		co.dist[lower.tri(co.dist, diag = FALSE)] <- dis
	} else if((lower + nr.lev) == nr.obs) {
		co.dist[lower.tri(co.dist, diag = TRUE)] <- dis
	} else {
		return("Number of observations and unique id's does not match.")
	}

	rownames(co.dist) <- lab
	colnames(co.dist) <- lab
	co.dist.mat <- as.dist(co.dist)
	co.dist.mat

	set.seed(1234)

	###############################################
	# Try metaMDS
	###############################################
	# co.mds <- suppressWarnings(metaMDS(co.dist.mat, k = nr.dim, trymax = 500))
	# if(co.mds$converged == FALSE) return("The MDS algorithm did not converge. Please try again.")

	co.mds <- MASS::isoMDS(co.dist.mat, k = nr.dim, trace = FALSE)

	co.mds$points <- cmdscale(co.dist.mat, k = nr.dim)
	# co.mds$points <- co.mds

	out <- list()
	out$nr.dim <- nr.dim
	out$data <- co.dist.mat
	out$points <- co.mds$points
	out$labels <- lab
	out$nr.levels <- nr.lev
	out$lim <- max(abs(out$points))
	out$mds_rev_dim <- as.numeric(mds_rev_dim)
	out$mds_fontsz <- mds_fontsz

	nr.plots <- (nr.dim * (nr.dim - 1)) / 2
	plotHeight <- 650 * nr.plots
	plotWidth <- 650

	return(list('co.mds' = co.mds, 'co.dist.mat' = co.dist.mat, 'out' = out, 'plotHeight' = plotHeight,
		'plotWidth' = plotWidth))
}

summary_mds <- function(result = .mds()) {

	cat("Distance data:\n")
	print(result$co.dist.mat, digits = 3)
	cat("\nCoordinates:\n")
	co.mds <- result$co.mds
	coor <- co.mds$points
	colnames(coor) <- paste("Dim ", 1:ncol(coor))
	print(coor, digits = 3)

	cat("\nFinal stress measure equal to", sprintf("%.3f", co.mds$stress/100))
}

plots_mds <- function(result = .mds()) {

	out <- result$out

	if(out$nr.dim == 3) {
		op <- par(mfrow=c(3,1))
	} else {
		op <- par(mfrow=c(1,1))
	}

	if(!is.null(out$mds_rev_dim)) {
		dim2rev <- out$mds_rev_dim
		out$points[,dim2rev] <- -1 * out$points[,dim2rev]
	}

	for(i in 1:(out$nr.dim-1)) {
		for(j in (i+1):out$nr.dim) {
			plot(c(-out$lim,out$lim),type = "n",xlab='', ylab='', axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-out$lim, out$lim), xlim=c(-out$lim,out$lim))
			title(paste("Dimension",i,"vs Dimension",j), cex.main = out$mds_fontsz)
			points(out$points[,i], out$points[,j], pch = 16, cex = .6)

			# text(out$points[,i], out$points[,j], out$labels, col=rainbow(out$nr.lev,start=.6,end=.1), adj = c(0.4,-.4))
			textplot(out$points[,i], out$points[,j]+(.04*out$lim), out$labels, col=rainbow(out$nr.lev,start=.6,end=.1),
				cex = out$mds_fontsz, new = FALSE)
			abline(v=0, h=0)
		}
	}

	par(op)

}

###############################################
# move this over to an interactive viz lib
###############################################
