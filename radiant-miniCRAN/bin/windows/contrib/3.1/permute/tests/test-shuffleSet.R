library(testthat)
library_if_available(permute)

context("Testing shuffleSet()")

## test that shuffleSet interleves the separate block-level
## permutations correctly back into the original ordering
## This always generates odd, even, odd, ..., numbered vector
## of observations, hence when we take the modulus 2 we get
## a vector of 1,0,1,0,1,...
test_that("shuffleSet interleves block-level perms correctly", {
    gr <- factor(rep(1:2, length=20))
    ctrl <- how(nperm = 5, blocks = gr)
    p <- shuffleSet(20, control = ctrl) %% 2
    y <- rep(c(1L, 0L), length.out = ncol(p))
    nc <- ncol(p)
    for (i in seq_len(nrow(p))) {
        expect_that(p[i, ], equals(y))
    }
})

## test that nset permutations are always returned if
## make = FALSE in how()
test_that( "shuffleSet returns exactly nset permutations when make == FALSE", {
    ## simple random permutation
    h <- how(make = FALSE)
    ss <- shuffleSet(n = 4, nset = 10, control = h)
    expect_that(nrow(ss), equals(10))

    ## time series
    h <- how(within = Within(type = "series"), make = FALSE)
    ss <- shuffleSet(n = 20, nset = 15, control = h)
    expect_that(nrow(ss), equals(15))

})

## test that shuffleSet always returns a matrix, even for nset == 1
test_that("shuffleSet returns a matrix even for nset == 1", {
    h <- how()
    ss <- shuffleSet(25, nset = 1, control = h)
    expect_that(ss, is_a("matrix"))
})
