# MDS {{{
.mdsCheckFunction <- function(state){
	if(length(state$id1) < 1)
		return("Please select a first ID variable (i.e., ID 1)")
	if(length(state$id2) < 1)
		return("Please select a second ID variable (i.e., ID 2)")
	if("Plot in 3D" %in% state$opt && state$nr.dim != 3)
		return("3 dimensions must be specified to construct a 3D plot")
	if("Reverse D3" %in% state$opt && state$nr.dim != 3)
		return("Cannot reverse a 3rd dimension when only 2 are specified. Either select 3 dimensions or uncheck the 'Reverse D3' box.")
	return("")
}

.mdsRunFunction <- function(state) {

	nr.dim <- as.numeric(state$nr.dim)
	pbf <- as.numeric(state$pbf)
	fsize <- as.numeric(state$fsize)

	cmd <- paste("dis <- ", state$data,"$",state$dis,"\n", sep = "")
	cmd <- paste(cmd, "id1 <- as.character(", state$data,"$",state$id1,")\n", sep = "")
	cmd <- paste(cmd, "id2 <- as.character(", state$data,"$",state$id2,")\n",sep = "")
	eval(parse(text = cmd))

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
		print("Number of observations and unique id's does not match."); stop() 
	}

	rownames(co.dist) <- lab
	colnames(co.dist) <- lab
	co.dist.mat <- as.dist(co.dist)

	# todo: create a check to see if the labels line up with the distance information
	### co.mds <- suppressWarnings(metaMDS(co.dist.mat, k = nr.dim, trymax = 500))
	### if(co.mds$converged == FALSE) {
		### # print("The MDS algorithm did not converge. Please try again.")
		### # stop()
		### # co.mds <- cmdscale(co.dist.mat, k = nr.dim)
	### }

	set.seed(1234)
	co.mds <- isoMDS(co.dist.mat, k = nr.dim)
	co.mds.org <- co.mds

	if("Reverse D1" %in% state$opt)
		co.mds$points[,1] <- -1 * co.mds.org$points[,1]
	if("Reverse D2" %in% state$opt)
		co.mds$points[,2] <- -1 * co.mds.org$points[,2]
	if("Reverse D3" %in% state$opt && nr.dim == 3)
		co.mds$points[,3] <- -1 * co.mds.org$points[,3]

	out <- list()
	out$nr.dim <- nr.dim
	out$data <- co.dist.mat
	out$points <- co.mds$points
	out$labels <- lab
	out$nr.levels <- nr.lev
	out$lim <- max(abs(out$points)) * pbf
	out$fsize <- fsize
	RM.MDS.out <<- out

	if("Plot in 3D" %in% state$opt) {

		### library(rgl)
		cmd <- paste("r3dDefaults$windowRect <- c(0,0, 800, 800)\n")
		cmd <- paste(cmd, "plot3d(RM.MDS.out$points, type = 'n', box = FALSE, axes = FALSE, xlab = '', ylab = '', zlab = '')\n")
		cmd <- paste(cmd, "aspect3d('iso')\n")
		cmd <- paste(cmd, "lines3d(rbind(c(-RM.MDS.out$lim,0,0),c(RM.MDS.out$lim,0,0)), lwd = 1, col = 1)\n")
		cmd <- paste(cmd, "lines3d(rbind(c(0,-RM.MDS.out$lim,0),c(0,RM.MDS.out$lim,0)), lwd = 1, col = 1)\n")
		cmd <- paste(cmd, "lines3d(rbind(c(0,0,-RM.MDS.out$lim),c(0,0,RM.MDS.out$lim)), lwd = 1, col = 1)\n")
		cmd <- paste(cmd, "points3d(RM.MDS.out$points, size = 8, col = 'darkgreen')\n")
		cmd <- paste(cmd, paste("text3d(RM.MDS.out$points, cex = RM.MDS.out$fsize, adj = c(-.1,-.1), text = RM.MDS.out$labels, color = topo.colors(RM.MDS.out$nr.levels))\n"))
		### eval(parse(text = cmd))
		execute(cmd)
	} 

	if("Plot in 2D" %in% state$opt) {

		if(nr.dim == 3) {
			JavaGD(name="3D", width=1380, height=460, ps=12)
			op <- par(mfrow=c(1,3))
		} else {
			JavaGD(name="2D", width=800, height=800, ps=12)
			op <- par(mfrow=c(1,1))
		}

		for(i in 1:(out$nr.dim-1)) {
			for(j in (i+1):out$nr.dim) {
				plot(c(-out$lim,out$lim),type = "n",xlab=paste("Dimension",i), ylab=paste("Dimension",j), axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-out$lim, out$lim), xlim=c(-out$lim,out$lim))
				title(paste("Perceptual Map for",state$data,"data\nDimension",i,"vs Dimsension",j))
				points(out$points[,i], out$points[,j], col="darkgreen", pch = 16)
				text(out$points[,i], out$points[,j], lab, col=rainbow(nr.lev,start=.6,end=.1), cex = fsize, adj = c(0.4,-.4))
				abline(v=0, h=0)
			}
		}
		par(op)
	}

	# print output
	cat("\n\n===================================\n======== Output from MDS ==========\n===================================\n")
	print(co.dist.mat, digits = 3)
	cat("\n")
	print(co.mds, digits = 3)
}

makeRadyMDSDialog <- function() {

	# make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(550L, 400L)
	dialog$setTitle("Multi-dimension scaling")

	# add variable selector
	variableSelector <-new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	variableList <- new(VariableListWidget, variableSelector)
	addComponent(dialog, variableSelector,1,450,850,10)	

	id1 <- new(SingleVariableWidget, "ID 1", variableSelector)
	id1$setTitle("id1")
	addComponent(dialog, id1, 100, 1000, 250, 500)

	id2 <- new(SingleVariableWidget, "ID 2", variableSelector)
	id2$setTitle("id2")
	addComponent(dialog, id2, 260, 1000, 410, 500)

	dis <- new(SingleVariableWidget, "Dis-similarity data", variableSelector)
	dis$setTitle("dis")
	addComponent(dialog, dis, 420, 1000, 560, 500)

	nr.dim <- new(ComboBoxWidget, "# of dimensions", c("2","3"))
	nr.dim$setTitle("nr.dim")
	nr.dim$setDefaultModel(c("2"))
	addComponent(dialog, nr.dim, 580, 900, 720, 690)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Adjust plot")
	addComponent(dialog,button,750,900,850,690)

	#make Options Dialog
	subDialog <- new(SimpleRSubDialog,dialog,"Adjust plot")
	setSize(subDialog,200,300)

	pbf <- new(ComboBoxWidget, "Plot edge buffer", as.character(seq(.5,2,.1)))
	pbf$setTitle("pbf")
	pbf$setDefaultModel(c("1"))
	addComponent(subDialog, pbf, 10, 950, 220, 50)

	fsize <- new(ComboBoxWidget, "Font size", as.character(seq(.5,2.5,.2)))
	fsize$setTitle("fsize")
	fsize$setDefaultModel(c("1.5"))
	addComponent(subDialog, fsize, 230, 950, 440, 50)

	### inst <- suppressWarnings(require('rgl', quietly = TRUE, character.only = TRUE))
	### if(inst == FALSE) {
		### opt <- new(CheckBoxesWidget,"Options",c("Plot in 2D","Reverse D1","Reverse D2","Reverse D3"))
	### } else {
		### opt <- new(CheckBoxesWidget,"Options",c("Plot in 2D","Plot in 3D","Reverse D1","Reverse D2","Reverse D3"))
	### }

	opt <- new(CheckBoxesWidget,"Options",c("Plot in 2D","Reverse D1","Reverse D2","Reverse D3"))

	opt$setTitle("opt")
	opt$setDefaultModel(c("Plot in 2D"))
	addComponent(subDialog, opt,450,950,830, 50)

	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	actionFunction <- function(cmd,ActionEvent){
		subDialog$setLocationRelativeTo(button)
		subDialog$run()
	}
	listener <- new(ActionListener)
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)
	
	dialog$setCheckFunction(toJava(.mdsCheckFunction))
	dialog$setRunFunction(toJava(.mdsRunFunction))

	### dialog$run()
	return(dialog)
}
#}}}

# Perceptual map {{{
.perceptualMapCheckFunction <- function(state){

	if(length(state$brand.attr) < 2)
		return("Please select at least two variables")
	if("Plot preferences" %in% state$opt && is.null(state$pref))
		return("To plot preferences please select at least one preference variable")
	if("Plot in 3D" %in% state$opt && state$nr.dim != 3)
		return("3 dimension must be specified to construct a 3D plot")
	return("")
}

.perceptualMapRunFunction <- function(state) {

	# the following used to produce an error in JGR
	### r3dDefaults$windowRect <- c(0,0, 800, 800)
	### plot3d(cbind(1:10,1:10,1:10))
	# the following used to produce an error in JGR
	### cmd <- "r3dDefaults$windowRect <- c(0,0, 800, 800)\n"
	### cmd <- paste(cmd, "plot3d(cbind(1:10,1:10,1:10))")
	### eval(parse(text = cmd))

	nr.dim <- as.numeric(state$nr.dim)
	nr.attr <- length(state$brand.attr)
	sf <- as.numeric(state$sf)
	pbf <- as.numeric(state$pbf)
	lbf <- as.numeric(state$lbf)

	plot.brands <- FALSE
	plot.attr <- FALSE
	plot.pref <- FALSE
	if("Plot brands" %in% state$opt)
		plot.brands <- TRUE
	if("Plot attributes" %in% state$opt)
		plot.attr <- TRUE
	if("Plot preferences" %in% state$opt)
		plot.pref <- TRUE

	cmd <- paste("f.data <-", state$data, "[,", paste("c(\"",paste(state$brand.attr,sep = "", collapse = "\",\""),"\")", sep = ""),"]\n")
	cmd <- paste(cmd, "brand.names <- ", state$data,"$",state$brand,"\n", sep = "")
	if(!is.null(state$pref)) {
		cmd <- paste(cmd, "pref.names <- ", state$data,"$",state$pref,"\n", sep = "")
		cmd <- paste(cmd, "pref.scores <-", state$data, "[,", paste("c(\"",paste(state$pref,sep = "", collapse = "\",\""),"\")", sep = ""),"]\n")
	}
	eval(parse(text = cmd))

	f.res <- suppressWarnings(principal(f.data, nfactors=nr.dim, rotate='varimax', scores=FALSE, oblique.scores=FALSE))
	df <- as.data.frame(f.res$loadings[,colnames(f.res$loadings)])
	m <- as.matrix(df)
	mPm <- (t(m) %*% m)
	smPm <- solve(mPm)
	cscm <- m %*% smPm
	mcar <- as.matrix(f.data)
	scores <- scale(mcar, center = TRUE, scale = TRUE) %*% cscm
	rownames(scores) <- brand.names

	pc <- std.pc <- 0
	if(!is.null(state$pref)) {
		pc <- cor(pref.scores,scores)
		rownames(pc) <- state$pref
		std.pc <- sf * pc
	}

	std.m <- sf * m
	std.scores <- scores
	max.max <- max(abs(std.m),abs(std.scores),abs(std.pc)) * pbf	# adding a buffer so the labels don't move off the screen

	rotate3D <- function() { #{{{
		sm <- par3d('userMatrix')
		rgl.viewpoint(theta = 2, phi = 1)
		rm <- par3d('userMatrix')
		par3d(userMatrix = sm)

		start <- proc.time()[3] 
		while ((i <- 36*(proc.time()[3]-start)) < 1000)
			par3d(userMatrix = rm%*%sm); sm <- par3d('userMatrix')
	} #}}}

	out <- list()
	out$scores <- scores
	out$loadings <- m
	out$pref.cor <- pc 
	out$scaled.loadings <- std.m
	out$scaled.pref.cor <- std.pc
	out$lim <- max.max
	out$brand.names <- brand.names
	out$brand.attr <- state$brand.attr
	if(!is.null(state$pref))
		out$pref.names <- state$pref
	
	out$rotate3D <- rotate3D
	RM.PM.out <<- out
	rm(f.data)

	if("Plot in 3D" %in% state$opt) {

		### library(rgl)
		cmd <- paste("r3dDefaults$windowRect <- c(0,0, 800, 800)\n")
		cmd <- paste(cmd, "plot3d(RM.PM.out$scores, type = 'n', box = FALSE, axes = FALSE, xlab = '', ylab = '', zlab = '')\n")
		cmd <- paste(cmd, "rgl.viewpoint(theta = 0, phi = 0)\n")
		cmd <- paste(cmd,"lines3d(rbind(c(-RM.PM.out$lim,0,0),c(RM.PM.out$lim,0,0)), lwd = 1, col = 1)\n")
		cmd <- paste(cmd, "lines3d(rbind(c(0,-RM.PM.out$lim,0),c(0,RM.PM.out$lim,0)), lwd = 1, col = 1)\n")
		cmd <- paste(cmd, "lines3d(rbind(c(0,0,-RM.PM.out$lim),c(0,0,RM.PM.out$lim)), lwd = 1, col = 1)\n")
		cmd <- paste(cmd, "aspect3d('iso')\n")

		if(plot.brands) {
			cmd <- paste(cmd, "points3d(RM.PM.out$scores, size = 8, col = 'darkgreen')\n")
			cmd <- paste(cmd, "text3d(RM.PM.out$scores, cex = 1.2, adj = c(-.1,-.1), text = RM.PM.out$brand.names, color = 'darkgreen')\n")
		}

		if(plot.attr) {
			cmd <- paste(cmd, "text3d(RM.PM.out$scaled.loadings, cex = 1.2, adj = c(.2,.03), text = RM.PM.out$brand.attr, color = 'darkblue')\n")
			for (i in state$brand.attr) 
				cmd <- paste(cmd,paste("lines3d(rbind(c(0,0,0), c(",paste(round(std.m[i,],4),collapse = ","),")), lwd = .1, col = 'orange')", sep = ""),"\n")
		}

		if(plot.pref) {
			cmd <- paste(cmd, "text3d(RM.PM.out$scaled.pref.cor, cex = 1.2, adj = c(.2,.03), text = RM.PM.out$pref.names, color = 'darkred')\n")
			for (i in state$pref) 
				cmd <- paste(cmd,paste("lines3d(rbind(c(0,0,0), c(",paste(round(std.pc[i,],4),collapse = ","),")), lwd = .4, col = 'red')", sep = ""),"\n")
		}

		execute(cmd)
		# rotate around axis
		### rotate3D()
	} else {

		if(nr.dim == 3) {
			JavaGD(name="3D", width=1380, height=460, ps=12)
			op <- par(mfrow=c(1,3))
		} else {
			JavaGD(name="2D", width=800, height=800, ps=12)
			op <- par(mfrow=c(1,1))
		}

		for(i in 1:(nr.dim-1)) {
			for(j in (i+1):nr.dim) {
				plot(c(-max.max,max.max),type = "n",xlab=paste("Dimension",i), ylab=paste("Dimension",j), axes = F, asp = 1, yaxt = 'n', xaxt = 'n', ylim=c(-max.max, max.max), xlim=c(-max.max,max.max))
				title(paste("Perceptual Map for",state$data,"data\nDimension",i,"vs Dimsension",j))

				if(plot.brands) {
					points(std.scores[,i], std.scores[,j], col="darkgreen", pch = 16)
					text(std.scores[,i], std.scores[,j], brand.names, col="darkgreen", cex = 1.2, adj = c(-.1,-.2))
				}
				abline(v=0, h=0)
			
				if(plot.attr) {
					text(lbf*std.m[,i], std.m[,j], state$brand.attr, col="darkblue", cex = 1.2, adj=c(0.5,-.3))
					# add arrows
					for (k in state$brand.attr) 
						arrows(0,0, x1=std.m[k,i], y1=std.m[k,j], col="orange", cex=1, length=.1)
				}

				if(plot.pref) {
					text(lbf*std.pc[,i], std.pc[,j], state$pref, col="darkred", cex = 1.2,adj=c(0.5,-.3))
					for (l in state$pref) 
						arrows(0,0, x1=std.pc[l,i], y1=std.pc[l,j], col="red", length=.1)
				}

			}
		}
		# resetting the ploting preferences
		par(op)
	}

	# print output
	cat("\n\n===================================\n=== Output from factor analysis ===\n===================================\n")
	communalities <- as.data.frame(f.res$communality)
	colnames(communalities) <- "communalities"
	cat("\nAttribute communalities:\n")
	print(communalities, digits = 3)

	cat("\nAttribute - Factor loadings:\n")
	print(f.res$loadings, cutoff = 0)
	### print(f.res$loadings, cutoff = .5)

	cat("\nBrand - Factor scores:\n")
	print(scores, digits = 3)

	if(!is.null(state$pref)) {
		cat("\nPreference correlations:\n")
		print(pc, digits = 3)
	}
}

### .perceptualMapRunFunction(list(
### 'data'="retailers",
### 'data'="ret",
### 'brand'=c("Retailer"),
### 'brand.attr'=c("Good value","Quality prod.","Service","Convenience","Assortment","Sophisticated","Cluttered"),
### 'pref'=c("Segment1","Segment2"),
### 'opt'=c("Plot brands","Plot attributes"),
### 'opt'=c("Plot brands","Plot attributes","Plot in 3D"),
### 'nr.dim'="2",
### 'sf'="2.4",
### 'pbf'="1.4",
### 'lbf'="1.2"))

makeRadyPerceptualMapDialog <-function() { #{{{ 

	# make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(600L, 700L)
	dialog$setTitle("Perceptual Map")

	# add variable selector
	variableSelector <-new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	variableList <- new(VariableListWidget, variableSelector)
	addComponent(dialog, variableSelector,1,500,850,10)	

	# add list for the brand variable
	brand <- new(SingleVariableWidget, "Brand", variableSelector)
	brand$setTitle("brand")
	addComponent(dialog, brand, 50 ,1000, 150, 550)

	# add list for the attribute variables
	brand.attr <- new(VariableListWidget, "Attributes", variableSelector)
	brand.attr$setTitle("brand.attr")
	addComponent(dialog, brand.attr, 150 ,1000, 350, 550)

	# add list for the preference scores
	pref <- new(VariableListWidget, "Preference scores", variableSelector)
	pref$setTitle("pref")
	addComponent(dialog, pref, 350 ,1000, 550, 550)

	#options for transforming the variables
	### inst <- suppressWarnings(require('rgl', quietly = TRUE, character.only = TRUE))
	### if(inst == FALSE) {
		### opt <- new(CheckBoxesWidget,"Options",c("Plot brands","Plot attributes","Plot preferences"))
	### } else {
		### opt <- new(CheckBoxesWidget,"Options",c("Plot brands","Plot attributes","Plot preferences","Plot in 3D"))
	### }
	opt <- new(CheckBoxesWidget,"Options",c("Plot brands","Plot attributes","Plot preferences"))
	
	opt$setTitle("opt")
	opt$setDefaultModel(c("Plot brands"))
	addComponent(dialog, opt,550,950,690, 700)

	nr.dim <- new(ComboBoxWidget, "# of dimensions", c("2","3"))
	nr.dim$setTitle("nr.dim")
	nr.dim$setDefaultModel(c("2"))
	addComponent(dialog, nr.dim, 700, 950, 800, 700)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Adjust plot")
	addComponent(dialog,button,810,950,880,700)

	#make Options Dialog
	subDialog <- new(SimpleRSubDialog,dialog,"Adjust plot")
	setSize(subDialog,200,300)

	sf <- new(ComboBoxWidget, "Arrow scaling factor", as.character(seq(1,3,.2)))
	sf$setTitle("sf")
	sf$setDefaultModel(c("2.4"))
	addComponent(subDialog, sf, 10, 950, 260, 50)

	pbf <- new(ComboBoxWidget, "Plot edge buffer", as.character(seq(1.2,2.2,.1)))
	pbf$setTitle("pbf")
	pbf$setDefaultModel(c("1.4"))
	addComponent(subDialog, pbf, 270, 950, 520, 50)

	lbf <- new(ComboBoxWidget, "Arrow label buffer", as.character(seq(1,1.8,.1)))
	lbf$setTitle("lbf")
	lbf$setDefaultModel(c("1.2"))
	addComponent(subDialog, lbf, 530, 950, 780, 50)

	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	actionFunction <- function(cmd,ActionEvent){
		subDialog$setLocationRelativeTo(button)
		subDialog$run()
	}
	listener <- new(ActionListener)
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)
	
	dialog$setCheckFunction(toJava(.perceptualMapCheckFunction))
	dialog$setRunFunction(toJava(.perceptualMapRunFunction))

	### dialog$run()
	return(dialog)
} #}}}
#}}}
