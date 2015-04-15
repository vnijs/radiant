library(radiant)
library(png)

trim_trailing <- function (x) sub("\\s+$", "", x)
trim_leading <- function (x) sub("^\\s+", "", x)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

compare_output <- function(res1, res2) {
  for (i in 1:length(res2)) {
    if (res1[i] != res2[i]) {
      print(i)
      print(res1[i])
      print(res2[i])
    }
  }
}

######### tests ########

context("Compare means")

test_that("compare_means 1", {
  result <- compare_means("diamonds","cut","price")
  res1 <- capture.output(summary(result))[15] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "Fair not equal to Very Good      Fair = Very Good       545.322 0.177"
  expect_equal(res1,res2)
})

context("Compare proportions")

test_that("compare_props 2", {
  result <- compare_props("titanic", "pclass", "survived")
  res1 <- capture.output(summary(result))[7] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "200 123 323 0.619 0.027 0.053"
  expect_equal(res1,res2)
})

context("Single proportion")

test_that("single_prop 1", {
  result <- single_prop("diamonds","color")
  expect_equal(result$sp_levels,"D")
  res1 <- capture.output(summary(result))[8] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "0.127    1666.565  < .001  1 0.116  0.14 382 3000"
  expect_equal(res1,res2)
})

test_that("single_prop 2", {
  result <- single_prop("diamonds","clarity", sp_levels = "IF", sp_comp_value = 0.05)
  expect_equal(result$sp_levels,"IF")
  res1 <- capture.output(summary(result))[8] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "0.033      18.253  < .001  1 0.027  0.04 99 3000"
  expect_equal(res1,res2)
})

context("Regression")

test_that("regression", {
  result <- regression("diamonds", "price", c("carat", "clarity"))
  res1 <- capture.output(summary(result))[8] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "carat    8438.030    51.101 165.125  < .001 ***"
  expect_equal(res1,res2)

  # full output - cannot open file when running tests
  res1 <- capture.output(summary(result)) %>% trim
  # cat(paste0(res1,"\n"), file = "~/gh/radiant_dev/tests/testthat/output/regression1.txt")
  res2 <- paste0(readLines("output/regression1.txt")) %>% trim
  expect_equal(res1,res2)
})

test_that("regression - plots", {
  result <- regression("diamonds", "price", c("carat", "clarity"))
  grb <- plot(result, reg_plots = "dashboard", shiny = TRUE)
  expect_true(all(c("arrange","ggplot") %in% class(grb)))
  expect_equal(try(print(grb), silent = TRUE), NULL)
  # expect_true(file.exists("Rplots.pdf"))  # not always created it seems
  unlink("Rplots.pdf")

  # useful for interactive testing - keep plots out of build
#   png("output/regression1.png")
#     plot(result, reg_plots = "dashboard")
#   dev.off()
#   res1 <- readPNG("output/regression1.png")
#   res2 <- readPNG("output/regression1-correct.png")
#   expect_equal(res1,res2)
#   unlink("output/regression1.png")
#   rm(res1, res2)
})
