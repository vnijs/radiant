library(testthat)
library_if_available(permute)

context("Testing allPerms()")

test_that("allPerms - blocks - within block free", {
    ## example data from Joris Meys from
    ## http://stackoverflow.com/a/21313632/429846
    thedata <- data.frame(score = c(replicate(4, sample(1:3))),
                          judge = rep(1:4, each = 3),
                          wine = rep.int(1:3, 4))

    ## without the observed permutation included
    hh <- how(within = Within("free"),
              blocks = factor(thedata$judge),
              complete = TRUE, maxperm = 1e9)
    nr <- nrow(thedata)
    np <- numPerms(nr, hh)
    p <- allPerms(nr, control = hh)
    expect_that(nrow(p), equals(np - 1)) ## default is to drop observed

    ## check no duplicate indices within rows
    dup <- any(apply(p, 1, function(x) any(duplicated(x))))
    expect_false(dup, info = "Blocks: even; within: free; no observed")

    ## with the observed permutation included
    hh <- how(within = Within("free"),
              blocks = factor(thedata$judge),
              complete = TRUE, maxperm = 1e9,
              observed = TRUE)
    p <- allPerms(nr, control = hh)
    expect_that(nrow(p), equals(np)) ## now includes observed

    ## check no duplicate indices within rows
    dup <- any(apply(p, 1, function(x) any(duplicated(x))))
    expect_false(dup, info = "Blocks: even; within: free; observed")
})

test_that("allPerms; blocks: within; block free - uneven block sizes", {
    fac <- factor(rep(1:3, times = c(2,2,4)))

    ## without the observed permutation included
    hh <- how(within = Within("free"),
              blocks = fac,
              complete = TRUE, maxperm = 1e9)
    ll <- length(fac)
    np <- numPerms(ll, hh)
    expect_that(np, equals(prod(factorial(2), factorial(2), factorial(4))))
    p <- allPerms(ll, control = hh)
    expect_that(nrow(p), equals(np - 1)) ## default is to drop observed

    ## check no duplicate indices within rows
    dup <- any(apply(p, 1, function(x) any(duplicated(x))))
    expect_false(dup, info = "Blocks: uneven; within: free; no observed")

    ## with the observed permutation included
    hh <- how(within = Within("free"),
              blocks = fac,
              complete = TRUE, maxperm = 1e9,
              observed = TRUE)
    p <- allPerms(ll, control = hh)
    expect_that(nrow(p), equals(np)) ## now includes observed

    ## check no duplicate indices within rows
    dup <- any(apply(p, 1, function(x) any(duplicated(x))))
    expect_false(dup, info = "Blocks: uneven; within: free; observed")
})

## testing plot-level permutations ------------------------------------
test_that("allPerms: plots; within: free; even: yes;", {
    fac <- rep(1:3, each = 3)

    hh <- how(plots = Plots(strata = fac),
              complete = TRUE, maxperm = 1e9)
    ll <- length(fac)
    np <- numPerms(ll, hh)
    p <- allPerms(ll, control = hh)
    expect_that(nrow(p), equals(np - 1), ## default is to drop observed
                info = "Check n all perms == numPerms output.")

    ## check no duplicate indices within rows
    dup <- any(apply(p, 1, function(x) any(duplicated(x))))
    expect_false(dup,
                 info = "Unique? Plots: even; within: free; no observed")

    ## with the observed permutation included
    hh <- how(within = Within("free"),
              plot = Plots(strata = fac),
              complete = TRUE, maxperm = 1e9,
              observed = TRUE)
    p <- allPerms(ll, control = hh)
    expect_that(nrow(p), equals(np)) ## now includes observed

    ## check no duplicate indices within rows
    dup <- any(apply(p, 1, function(x) any(duplicated(x))))
    expect_false(dup, info = "Unique? Plots: even; within: free; inc observed")
})

test_that("allPerms; plots: within; plot free - uneven plot sizes", {
    fac <- factor(rep(1:3, times = c(2,2,4)))

    ## without the observed permutation included
    hh <- how(within = Within("free"),
              plots = Plots(strata = fac),
              complete = TRUE, maxperm = 1e9)
    ll <- length(fac)
    np <- numPerms(ll, hh)
    expect_that(np, equals(prod(factorial(2), factorial(2), factorial(4))))
    p <- allPerms(ll, control = hh)
    expect_that(nrow(p), equals(np - 1)) ## default is to drop observed

    ## check no duplicate indices within rows
    dup <- any(apply(p, 1, function(x) any(duplicated(x))))
    expect_false(dup, info = "Plots: uneven; within: free; no observed")

    ## with the observed permutation included
    hh <- how(within = Within("free"),
              plots = Plots(strata = fac),
              complete = TRUE, maxperm = 1e9,
              observed = TRUE)
    p <- allPerms(ll, control = hh)
    expect_that(nrow(p), equals(np)) ## now includes observed

    ## check no duplicate indices within rows
    dup <- any(apply(p, 1, function(x) any(duplicated(x))))
    expect_false(dup, info = "Plots: uneven; within: free; observed")
})
