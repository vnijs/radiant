
context("Radiant functions")

test_that("set_class", {
  foo <- . %>% .^2 %>% set_class(c("foo", class(.)))
  expect_equal(3 %>% foo %>% class,c("foo","numeric"))
})

test_that("sig_star", {
  sig_stars(c(.0009, .049, .009, .4, .09)) %>%
  expect_equal(c("***","*","**","","."))
})

test_that("sshh", {
  expect_equal(sshh(c(message("should be null"), test = 3)), NULL)
  expect_equal(sshh(warning("should be null")), NULL)
})

test_that("sshhr", {
  test <- 3 %>% set_names("test")
  expect_equal(sshhr(c(message("should be null"), test = 3)), test)
  expect_equal(sshhr(c(warning("should be null"), test = 3)), c("should be null",test))
})

test_that("getdata", {
 getdata("mtcars","mpg:disp", filt = "mpg > 20", rows = 1:5) %>%
 expect_equal(.,mtcars[mtcars$mpg > 20,c("mpg","cyl","disp")][1:5,1:3] %>% set_rownames(1:5))
})

test_that("changedata", {
  r_data <<- list() %>% { .$dat <- data.frame(a = 1:20); . }
  changedata("dat",20:1, "b")
  expect_equal(r_data$dat,data.frame(a = 1:20, b = 20:1))
  rm(r_data, envir = .GlobalEnv)
})

test_that("getclass", {
  expect_equal(getclass(diamonds), sapply(diamonds,class))
})

test_that("is_empty", {
  expect_true(is_empty(""))
  expect_true(is_empty(NULL))
  expect_false(is_empty(3))
  expect_true(is_empty(c()))
  expect_true(is_empty("nothing", empty = "nothing"))
})

test_that("options", {
  options("width"=10)
  options("scipen"=0)
  radiant:::.onLoad("","")
  expect_equal(options()$width, 200)
  expect_equal(options()$scipen, 100)
})

test_that("filter", {
  dat <- getdata("diamonds", filt = "cut == 'Very Good'")
  expect_equal(nrow(dat), 677)
})
