library(devtools)
install_github("hadley/devtools")
install_github("ropensci/RSelenium")
library(RSelenium)
install_github("hadley/testthat")
library(testthat)

RSelenium::checkForServer()
RSelenium::startServer()

# start a browser
remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName = "firefox"
)

remDr$open()
remDr$getStatus()
remDr$navigate("http://127.0.0.1:4475/")
remDr$getCurrentUrl()

# webElems <- remDr$findElements("css selector", ".control-label")
# appCtrlLabels <- sapply(webElems, function(x){x$getElementText()})

# extraCapabilities <- list(name = "radiant screenshot", "screen-resolution" = "1280x1024")
# extraCapabilities <- list(name = "radiant screenshot", username = user , accessKey = pass, "screen-resolution" = "1280x1024")

test_dir("~/Desktop/GitHub/radiant_dev/tests/", filter = 'basic', reporter = "Tap")
