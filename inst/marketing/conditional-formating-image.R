# John Colby's response in ...
# http://stackoverflow.com/questions/7828256/conditional-formatting-making-cells-colorful/7890885#7890885

# this will mean the factor loadings will be displayed in plots
# diagnostics etc. can go in summary

loc1 <- c("Aa", "Aa", "aa", "Aa")
loc2 <- c("aa", "aa", "aa", "AA")
loc3 <- c("aa", "Aa", "aa", "aa")
gen <- data.frame(loc1, loc2, loc3)

loc1g <- c(0.01, 0.5, 1, 0.75)
loc2g <- c(0.2, 0.1, 0.2, 0.6)
loc3g <- c(0.8, 0.8, 0.55, 1)
pval <- data.frame(loc1g, loc2g, loc3g)
x = 1:ncol(pval)
y = 1:nrow(pval)

# Colored backgrounds
dev.new(width=4, height=4)
image(x, y, t(as.matrix(pval)), col = c('red', 'yellow', 'red'),
      breaks = c(0, 0.3, 0.7, 1),
      xaxt='n', 
      yaxt='n', 
      ylim=c(max(y)+0.5, min(y)-0.5), 
      xlab='', 
      ylab='')
centers = expand.grid(y, x)
text(centers[,2], centers[,1], unlist(gen))