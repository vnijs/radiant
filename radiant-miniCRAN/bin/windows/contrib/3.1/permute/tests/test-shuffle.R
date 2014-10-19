library(testthat)
library_if_available(permute)

context("Testing shuffle()")

## test no permutation
test_that("shuffle(n) returns seq_len(n) when not permuting", {
    ctrl <- how(within = Within(type = "none"))

    expect_that(shuffle(3, control = ctrl), is_identical_to(seq_len(3)))
    expect_that(shuffle(1, control = ctrl), is_identical_to(1L))
})

## test shuffle returns integers
test_that("shuffle() returns integers", {
    ctrl <- how(within = Within(type = "none"))

    expect_that(shuffle(4), is_a("integer"))
    expect_that(shuffle(100), is_a("integer"))
    expect_that(shuffle(1, control = ctrl), is_identical_to(1L))
    expect_that(shuffle(3, control = ctrl), is_identical_to(c(1L, 2L, 3L)))
})

## test what shuffle returns when permuting only the strata
## must *not* assume that the samples are in contiguous blocks
test_that("shuffle() works for non-contigous blocks of samples", {
    ## permuting levels of Plots instead of observations
    ## non-contiguous blocks - checks that r1972 continues to work
    Plot <- factor(rep(1:4, 5))
    CTRL <- how(plots = Plots(strata = Plot, type = "free"),
                within = Within(type = "none"))
    n <- 20
    set.seed(2)
    result <- shuffle(n, CTRL)
    out1 <- as.integer(c( 3, 2, 1, 4,
                          7, 6, 5, 8,
                         11,10, 9,12,
                         15,14,13,16,
                         19,18,17,20))
    expect_that(result, is_identical_to(out1))
    out2 <- factor(as.integer(rep(c(3,2,1,4), 5)), levels = 1:4)
    expect_that(Plot[result], is_identical_to(out2))
})
