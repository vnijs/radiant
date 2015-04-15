library(radiant)

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
 getdata("mtcars","mpg:disp", filt = "mpg > 20", slice = "1:5") %>%
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

test_that("state_init", {
 r_state <<- list()
 expect_equal(state_init("test"),"")
 expect_equal(state_init("test",0),0)
 # outside of tests use <- not <<-
 r_state$test <<- c("a","b")
 expect_equal(state_init("test",0),c("a","b"))
 expect_equal(state_init("rb", "a"),"a")
 r_state$rb <<- "b"
 expect_equal(state_init("rb", "a"),"b")
 rm(r_state, envir = .GlobalEnv)
 expect_error(state_init("rb", "a"),"Error in state_init.*")
 cat("\n")
})

test_that("state_single", {
 r_state <<- list()
 expect_equal(state_single("test",1:10,1),1)
 # outside of tests use <- not <<-
 r_state$test <<- 8
 expect_equal(state_single("test",1:10,1),8)
 expect_equal(state_single("si"),character(0))
 r_state$si <<- "b"
 expect_equal(state_single("si",c("a","b"),"a"),"b")
 rm(r_state, envir = .GlobalEnv)
 expect_error(state_single("si"),"Error in state_single.*")
 cat("\n")
})

test_that("state_multiple", {
 r_state <<- list()
 expect_equal(state_multiple("test",1:10,1:3),1:3)
 # outside of tests use <- not <<-
 r_state$test <<- 8:10
 expect_equal(state_multiple("test",1:10,1:3),8:10)
 expect_equal(state_multiple("sim", c("a","b")),character(0))
 r_state$sim <<- c("a","b")
 expect_equal(state_multiple("sim", c("a","b")),c("a","b"))
 rm(r_state, envir = .GlobalEnv)
 expect_error(state_multiple("sim", c("a","b")),"Error in state_multiple.*")
})

test_that("options", {
  options("width"=10)
  options("scipen"=0)
  radiant:::.onLoad("","")
  expect_equal(options()$width, 200)
  expect_equal(options()$scipen, 100)
})
