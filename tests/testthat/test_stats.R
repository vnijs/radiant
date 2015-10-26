# library(png)
# library(radiant)
# library(testthat)

trim_trailing <- function(x) sub("\\s+$", "", x)
trim_leading <- function(x) sub("^\\s+", "", x)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

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

# library(radiant)
# library(testthat)

context("Compare means")

test_that("compare_means 1", {
  result <- compare_means("diamonds","cut","price")
  res1 <- capture.output(summary(result))[9] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "Fair 4505.238  101 3749.540 373.093 740.206"
  expect_equal(res1,res2)
})

test_that("compare_means 2", {
  result <- compare_means("diamonds","cut","price")
  res1 <- capture.output(summary(result, show = TRUE))[19] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "Fair = Ideal          Fair not equal to Ideal          1035.014 0.009   389.429  2.658   118.618  263.879 1806.149 **"
  expect_equal(res1,res2)
})

context("Compare proportions")

test_that("compare_props 1", {
  result <- compare_props("titanic", "pclass", "survived")
  res1 <- capture.output(summary(result))[9] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "1st 179 103 282 0.635 0.029 0.056"
  expect_equal(res1,res2)
})


test_that("compare_props 2", {
  result <- compare_props("titanic", "pclass", "survived")
  res1 <- capture.output(summary(result, show = TRUE))[14] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "1st = 2nd   1st not equal to 2nd   0.194 < .001   20.576     1  0.112 0.277 ***"
  expect_equal(res1,res2)
})

context("Single proportion")

test_that("single_prop 1", {
  result <- single_prop("diamonds", "color")
  expect_equal(result$lev, "D")
  res1 <- capture.output(summary(result))[13] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "-0.373 382  < .001 0.116  0.14 ***"
  expect_equal(res1,res2)
})

test_that("single_prop 2", {
  result <- single_prop("diamonds", "clarity", lev = "IF", comp_value = 0.05)
  expect_equal(result$lev, "IF")
  res1 <- capture.output(summary(result))[13] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "-0.017 99  < .001 0.027  0.04 ***"
  expect_equal(res1,res2)
})

context("Single mean")

test_that("single_mean 1", {
  result <- single_mean("diamonds", "carat")
  res1 <- capture.output(summary(result))[12] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "0.794 0.009  91.816  < .001 2999 0.777 0.811 ***"
  expect_equal(res1,res2)
})

test_that("single_mean 2", {
  result <- single_mean("titanic", "age", comp_value = 40)
  res1 <- capture.output(summary(result))[12] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "-10.187 0.445   -22.9  < .001 1042 28.94 30.686 ***"
  expect_equal(res1,res2)
})

context("Regression")

test_that("regression", {
  result <- regression("diamonds", "price", c("carat", "clarity"))
  res1 <- capture.output(summary(result))[10] %>% trim
  # cat(paste0(res1, "\n"))
  res2 <- "carat    8438.030    51.101 165.125  < .001 ***"
  expect_equal(res1,res2)

  res1 <- capture.output(summary(result)) %>% trim
  # cat(paste0(res1,"\n"), file = "~/gh/radiant/tests/testthat/output/regression1.txt")
  ## full output - cannot open file when testing the tests
  res2 <- paste0(readLines("output/regression1.txt")) %>% trim
  expect_equal(res1,res2)
})

test_that("regression - plots", {
  result <- regression("diamonds", "price", c("carat", "clarity"))
  grb <- plot(result, plots = "dashboard", shiny = TRUE)
  # class(grb)
  # library(testthat)
  expect_true(all(c("gtable","grob") %in% class(grb)))
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
