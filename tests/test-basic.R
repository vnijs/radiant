## run radiant from a console
# library(shiny)
# shiny::runApp('/Users/vnijs/Desktop/GitHub/radiant_dev/inst/marketing', port = 4475, launch.browser=TRUE)


#
#
# Important links
#
#

# https://github.com/ropensci/RSelenium/issues/1
# http://ropensci.github.io/RSelenium/
# http://stackoverflow.com/questions/14237257/how-to-fill-in-an-online-form-and-get-results-back-in-r

if(!require(RSelenium))
  install.packages("RSelenium")

library(RSelenium)
library(testthat)

# starting testing
context("basic")

RSelenium::startServer()
# RSelenium::checkForServer()

remDr <- remoteDriver()
remDr$open(silent = TRUE)
appURL <- "http://127.0.0.1:4475"

test_that("can connect to app", {
  remDr$navigate(appURL)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "Radiant - Marketing Research")
})

test_that("controls are present", {
  webElems <- remDr$findElements("css selector", ".control-label")
  appCtrlLabels <- sapply(webElems, function(x){x$getElementText()})
  appCtrlLabels
  expect_equal(appCtrlLabels[[1]], "Datasets:")
  expect_equal(appCtrlLabels[[2]], "Load data:")
  expect_equal(appCtrlLabels[[3]], "")
  expect_equal(appCtrlLabels[[4]], "Save data:")
})

currentTabLabels <- c("Data","Random","Base","Regression","Maps","Factor","Cluster", "Conjoint","R","Quit","Help","Manage","View","Visualize","Explore","Merge","Transform")

test_that(paste("tabs and nav elements are all present:",paste(currentTabLabels,collapse=" ")), {
  webElems <- remDr$findElements("css selector", ".nav a")
  appTabLabels <- sapply(webElems, function(x){x$getElementText()})
  appTabLabels <- unlist(appTabLabels)
  appTabLabels <- appTabLabels[appTabLabels != ""]
  expect_equal(appTabLabels, currentTabLabels)
})

webElem <- remDr$findElements("css selector", "#uploadfile")
webElem$clickElement()
str(webElem)

webElem <- remDr$findElements("xpath","//*[(@id = 'uploadfile')]")
webElem$clickElement()


webElem <- remDr$findElement("css selector", "#man_show_remove")
webElem$isElementSelected()[[1]]


ret <- try(remDr$findElement("id", "man_add_descr"), silent = TRUE)
!is(ret, 'try-error')
init_state <- ret$isElementSelected()[[1]]
ret$clickElement()
change_state <- ret$isElementSelected()[[1]]
expect_is(init_state, "logical")
expect_is(change_state, "logical")
expect_false(init_state == change_state)


# I guess this means that if there is no error, the element is present
ret <- try(remDr$findElement("id", "updateDescr"))
!is(ret, 'try-error')

ret <- try(remDr$findElement("id", "uploadfile"))
is(ret, 'try-error')

ret <- try(remDr$findElement("id", "downloadData"))
!is(ret, 'try-error')

ret <- try(remDr$findElement("id", "data_rename"))
!is(ret, 'try-error')

ret <- try(remDr$findElement("id", "renameButtons"))
!is(ret, 'try-error')

# remDv$findElement(using = "xpath", "//input[@id = 'cmdCalc']")$clickElement()

remDr$findElement("id", "uploadfile")$clickElement()
remDr$findElement("id", "downloadData")$clickElement()

# go to the Transform tab (6th tab)
# remDr$findElement("xpath", "//*[@id='datatabs']/li[6]/a")$clickElement()

# going to the Data tab!!
webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
dataElem <- webElem[[which(tabTitles == 'Data')]]
dataElem$clickElement()

webElem <- remDr$findElements(value = "//*[@id = 'datatabs']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
visElem <- webElem[[which(tabTitles == 'Visualize')]]
visElem$clickElement()

visElem <- webElem[[which(tabTitles == 'Transform')]]
visElem$clickElement()


# going to the correlation tab!!
webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
regressElem <- webElem[[which(tabTitles == 'Regression')]]
regressElem$clickElement()

webElem <- regressElem$findChildElements(value = "../*[@class = 'dropdown-menu']/*/a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
correlationElem <- webElem[[which(subTitles == 'Correlation')]]
correlationElem$clickElement()



# webElem <- remDr$findElement("id", "View")
# webElem <- remDr$findElement("xpath", "//*[@id='datatabs']/li[3]/a")

# remDr$mouseMoveToLocation(webElement = webElem) # move to the required element
# remDr$click(2) # right mouse button click
