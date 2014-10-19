### R code from vignette source 'permutations.Rnw'

###################################################
### code chunk number 1: preliminary
###################################################
options("prompt" = "R> ", "continue" = "+  ")
options(useFancyQuotes="UTF-8")


###################################################
### code chunk number 2: load_jackal
###################################################
require(permute)
data(jackal)
jackal


###################################################
### code chunk number 3: ttest_jackal
###################################################
jack.t <- t.test(Length ~ Sex, data = jackal, var.equal = TRUE,
                 alternative = "greater")
jack.t


###################################################
### code chunk number 4: ftest_jackal
###################################################
var.test(Length ~ Sex, data = jackal)
fligner.test(Length ~ Sex, data = jackal)


###################################################
### code chunk number 5: meanFun
###################################################
meanDif <- function(x, grp) {
 mean(x[grp == "Male"]) - mean(x[grp == "Female"])
}


###################################################
### code chunk number 6: randJackal
###################################################
Djackal <- numeric(length = 5000)
N <- nrow(jackal)
set.seed(42)
for(i in seq_len(length(Djackal) - 1)) {
    perm <- shuffle(N)
    Djackal[i] <- with(jackal, meanDif(Length, Sex[perm]))
}
Djackal[5000] <- with(jackal, meanDif(Length, Sex))


###################################################
### code chunk number 7: hist_jackal (eval = FALSE)
###################################################
## hist(Djackal, main = "",
##      xlab = expression("Mean difference (Male - Female) in mm"))
## rug(Djackal[5000], col = "red", lwd = 2)


###################################################
### code chunk number 8: permutations.Rnw:125-126
###################################################
(Dbig <- sum(Djackal >= Djackal[5000]))


###################################################
### code chunk number 9: permutations.Rnw:129-130
###################################################
Dbig / length(Djackal)


###################################################
### code chunk number 10: draw_hist_jackal
###################################################
hist(Djackal, main = "",
     xlab = expression("Mean difference (Male - Female) in mm"))
rug(Djackal[5000], col = "red", lwd = 2)


###################################################
### code chunk number 11: permutations.Rnw:142-143
###################################################
choose(20, 10)


###################################################
### code chunk number 12: show_args
###################################################
args(shuffle)


###################################################
### code chunk number 13: show_str
###################################################
str(how())


###################################################
### code chunk number 14: compare_shuffle_sample
###################################################
set.seed(2)
(r1 <- shuffle(10))
set.seed(2)
(r2 <- sample(1:10, 10, replace = FALSE))
all.equal(r1, r2)


###################################################
### code chunk number 15: series1
###################################################
set.seed(4)
x <- 1:10
CTRL <- how(within = Within(type = "series"))
perm <- shuffle(10, control = CTRL)
perm
x[perm] ## equivalent


###################################################
### code chunk number 16: grid1
###################################################
set.seed(4)
plt <- gl(3, 9)
CTRL <- how(within = Within(type = "grid", ncol = 3, nrow = 3),
            plots = Plots(strata = plt))
perm <- shuffle(length(plt), control = CTRL)
perm


###################################################
### code chunk number 17: vis_grid1
###################################################
## Original
lapply(split(seq_along(plt), plt), matrix, ncol = 3)
## Shuffled
lapply(split(perm, plt), matrix, ncol = 3)


###################################################
### code chunk number 18: grid_2
###################################################
set.seed(4)
CTRL <- how(within = Within(type = "grid", ncol = 3, nrow = 3,
                            constant = TRUE),
            plots = Plots(strata = plt))
perm2 <- shuffle(length(plt), control = CTRL)
lapply(split(perm2, plt), matrix, ncol = 3)


###################################################
### code chunk number 19: series_2
###################################################
how(nperm = 10, within = Within(type = "series"))


###################################################
### code chunk number 20: shuffleSet_1
###################################################
set.seed(4)
CTRL <- how(within = Within(type = "series"))
pset <- shuffleSet(10, nset = 5, control = CTRL)
pset


###################################################
### code chunk number 21: permutations.Rnw:256-257
###################################################
how(nperm = 999)


###################################################
### code chunk number 22: withinArgs
###################################################
args(Within)


###################################################
### code chunk number 23: change-continuation
###################################################
options("prompt" = " ", "continue" = " ")


###################################################
### code chunk number 24: ptest-fun
###################################################
pt.test <- function(x, group, nperm = 199) {
    ## mean difference function
    meanDif <- function(i, x, grp) {
        grp <- grp[i]
        mean(x[grp == "Male"]) - mean(x[grp == "Female"])
    }
    ## check x and group are of same length
    stopifnot(all.equal(length(x), length(group)))
    ## number of observations
    N <- nobs(x)
    ## generate the required set of permutations
    pset <- shuffleSet(N, nset = nperm)
    ## iterate over the set of permutations applying meanDif
    D <- apply(pset, 1, meanDif, x = x, grp = group)
    ## add on the observed mean difference
    D <- c(meanDif(seq_len(N), x, group), D)
    ## compute & return the p-value
    Ds <- sum(D >= D[1]) # how many >= to the observed diff?
    Ds / (nperm + 1)     # what proportion of perms is this (the pval)?
}


###################################################
### code chunk number 25: reset-continuation
###################################################
options("prompt" = "R> ", "continue" = "+  ")


###################################################
### code chunk number 26: run-ptest
###################################################
set.seed(42) ## same seed as earlier
pval <- with(jackal, pt.test(Length, Sex, nperm = 4999))
pval


###################################################
### code chunk number 27: change-continuation
###################################################
options("prompt" = " ", "continue" = " ")


###################################################
### code chunk number 28: parallel-ptest-fun
###################################################
ppt.test <- function(x, group, nperm = 199, cores = 2) {
    ## mean difference function
    meanDif <- function(i, .x, .grp) {
        .grp <- .grp[i]
        mean(.x[.grp == "Male"]) - mean(.x[.grp == "Female"])
    }
    ## check x and group are of same length
    stopifnot(all.equal(length(x), length(group)))
    ## number of observations
    N <- nobs(x)
    ## generate the required set of permutations
    pset <- shuffleSet(N, nset = nperm)
    if (cores > 1) {
        ## initiate a cluster
        cl <- makeCluster(cores)
        on.exit(stopCluster(cl = cl))
        ## iterate over the set of permutations applying meanDif
        D <- parRapply(cl, pset, meanDif, .x = x, .grp = group)
    } else {
        D <- apply(pset, 1, meanDif, .x = x, .grp = group)
    }
    ## add on the observed mean difference
    D <- c(meanDif(seq_len(N), x, group), D)
    ## compute & return the p-value
    Ds <- sum(D >= D[1]) # how many >= to the observed diff?
    Ds / (nperm + 1)     # what proportion of perms is this (the pval)?
}


###################################################
### code chunk number 29: reset-continuation
###################################################
options("prompt" = "R> ", "continue" = "+  ")


###################################################
### code chunk number 30: run-pptest
###################################################
require("parallel")
set.seed(42)
system.time(ppval <- ppt.test(jackal$Length, jackal$Sex, nperm = 9999,
                              cores = 2))
ppval


###################################################
### code chunk number 31: run-pptest2
###################################################
set.seed(42)
system.time(ppval2 <- ppt.test(jackal$Length, jackal$Sex, nperm = 9999,
                               cores = 1))
ppval2


###################################################
### code chunk number 32: get-set-eg0
###################################################
hh <- how()


###################################################
### code chunk number 33: get-set-eg1
###################################################
getNperm(hh)


###################################################
### code chunk number 34: <get-set-eg2
###################################################
getCall(hh)
setNperm(hh) <- 999
getNperm(hh)
getCall(hh)


###################################################
### code chunk number 35: get-set-eg3
###################################################
hh <- how(within = Within(type = "series"),
          plots = Plots(type = "series", strata = gl(10, 5)),
          blocks = gl(5, 10))


###################################################
### code chunk number 36: get-set-eg4
###################################################
pl <- getPlots(hh)
setType(pl) <- "free"
setPlots(hh) <- pl


###################################################
### code chunk number 37: get-set-eg5
###################################################
getType(hh, which = "plots")


###################################################
### code chunk number 38: get-set-eg6
###################################################
getCall(getPlots(hh))


###################################################
### code chunk number 39: get-set-eg7
###################################################
hh <- update(hh, plots = update(getPlots(hh), type = "series"))
getType(hh, which = "plots")


###################################################
### code chunk number 40: seesionInfo
###################################################
toLatex(sessionInfo(), locale = FALSE)


