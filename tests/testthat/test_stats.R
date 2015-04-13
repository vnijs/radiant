library(radiant)
library(png)

# change this directory to run locally
# t_path <- "~/gh/radiant_dev/tests"
# t_path <- system.file(package = "radiant")
# t_path <- "."
# setwd("~/gh/radiant_dev/tests")

trim_trailing <- function (x) sub("\\s+$", "", x)
trim_leading <- function (x) sub("^\\s+", "", x)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

compare_output <- function(res1, res2) {
  for(i in 1:length(res2)) {
    if(res1[i] != res2[i]) {
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
  # cat(paste0(res1,"\n"))
  # cat(paste0(res1,"\n"), file = "~/gh/radiant_dev/tests/output/regression1.txt")
  res2 <- paste0(readLines(file.path(t_path,"output/regression1.txt"))) %>% trim
  file.path(t_path,"output/regression1.txt")
  expect_equal(res1,res2)
})

test_that("regression - plots", {
  result <- regression("diamonds", "price", c("carat", "clarity"))
  # saved grob if > 2 time the filesize of the png
  # grb <- plot(result, reg_plots = "dashboard", shiny = TRUE)
  # save(grb, file = file.path(t_path,"/output/regression1-correct.rda"))
  png(paste0(t_path,"/output/regression1.png"))
    plot(result, reg_plots = "dashboard")
  dev.off()
  res1 <- readPNG(file.path(t_path,"/output/regression1.png"))
  res2 <- readPNG(file.path(t_path,"/output/regression1-correct.png"))
  expect_equal(res1,res2)
  unlink(file.path(t_path,"/output/regression1.png"))
  rm(res1, res2)
})
