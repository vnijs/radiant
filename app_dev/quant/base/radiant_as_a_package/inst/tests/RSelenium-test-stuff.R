require(devtools)
install_github("RSelenium", "johndharrison")
require(RSelenium)
port = 8100
host = "127.0.0.1"

# run checkForServer if the standalone selenium server is needed
# RSelenium::checkForServer() 
RemoteWebDriver("http://localhost:8100");

startServer()
remDr <- remoteDriver$new()
Sys.sleep(20) # time for server
remDr$open()
remDr$navigate(paste0("http://", host, ":", port))
# click the navbar button
# necessary as the tab names dont seem to populate untill they are needed
# webElem <- remDr$findElement(value = "//*[@class='btn btn-navbar']")
# webElem$clickElement()

webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
regressElem <- webElem[[which(tabTitles == 'Regression')]]
regressElem$clickElement()

webElem <- regressElem$findChildElements(value = "..//*[@class = 'dropdown-menu']//a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
correlationElem <- webElem[[which(subTitles == 'Correlation')]]
correlationElem$clickElement()
str(correlationElem)

webElem <- correlationElem$findElements(value = "//*[@id = 'summary_correlation']")
webElem <- correlationElem$findElements(value = "//*[@class = 'tab-pane active']//pre")
webElem <- correlationElem$findElements(value = "//*[@class = 'tab-pane active']")

sapply(webElem, function(x){x$getElementText()})
sapply(webElem, function(x){x$getPageSource()})


webElem <- correlationElem$findChildElements(value = "..//*[@id = 'summary_correlation']")
webElem <- correlationElem$findChildElements(value = "..//*[@class = 'tab-pane active']")
str(webElem)


webElem <- correlationElem$findElements(value = "//*[@id = 'plots_correlation']")
subTitles <- sapply(webElem, function(x){x$getElementText()})
corrPlotsElem <- webElem[[which(subTitles == 'plots_correlation')]]
corrPlotsElem$clickElement()



subTitles <- sapply(webElem, function(x){x$getElementText()})
correlationElem <- webElem[[which(subTitles == 'Correlation')]]
correlationElem$clickElement()



# close server
remDr$closeServer()

# John Harrison's example from https://groups.google.com/forum/#!msg/shiny-discuss/CSI9k5leehU/sEwroWNMRf0J
# startServer()
# remDr <- remoteDriver$new()
# remDr$open()
# remDr$navigate("http://glimmer.rstudio.com/winston/heightweight/")

# # find the X variable DOM element
# webElem <- remDr$findElement(using = "id", value = "x_var")

# # check what is selected as default
# webElem$getElementAttribute("value") # [1] "ageYear"

# # change to x = Weight option
# webElem <- remDr$findElement(value = "//option[@value = 'weightLb']")
# webElem$clickElement() # app should change to x variable = weight

# # change to x = Height
# webElem <- remDr$findElement(value = "//option[@value = 'heightIn']")
# webElem$clickElement() # app should change to x variable = weight

# # seperate male and female values
# webElem <- remDr$findElement(using = "id", value = "sex")
# webElem$clickElement() # app should change to x variable = weight

# # close server
# remDr$closeServer()


# remDr$navigate("http://vnijs.rady.ucsd.edu:3838/marketing")

# webElem <- remDr$findElement(value = "//*[contains(concat(' ', @class, ' '), ' btn btn-navbar ')]")
# webElem$clickElement()






require(RSelenium)
port = 8100
host = "127.0.0.1"
# assume radiant is running
# for simplicity assume a second R is running
# require(radiant)
# require(shiny)
# runApp(system.file("marketing", package="radiant")
#        , host = "0.0.0.0", port = 4678)

# run checkForServer if the standalone selenium server is needed
# RSelenium::checkForServer() 

RemoteWebDriver("http://localhost:9515", DesiredCapabilities.chrome());

startServer()
remDr <- remoteDriver$new()
Sys.sleep(20) # time for server
remDr$open()
remDr$navigate(paste0("http://", host, ":", port))
# click the navbar button
# necessary as the tab names dont seem to populate untill they are needed
webElem <- remDr$findElement(value = "//*[@class='btn btn-navbar']")
webElem$clickElement()
# get the nav-bar elements
webElem <- remDr$findElements(value = "//*[@class = 'nav']/li/a")
webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
webElem <- remDr$findElements(value = "//*[@class = 'nav shiny-tab-input']/li/a")
webElem <- remDr$findElements(value = "//*[@class = 'nav-collapse']/li/a")
webElem <- remDr$findElements(value = "//*[@class = 'nav-collapse']")
webElem <- remDr$findElements(value = "//*[@class = 'nav shiny-tab-input']")
webElem

remDr$findElements(value = "//*[@class = 'nav shiny-tab-input']")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
tabTitles
stateElem <- webElem[[which(tabTitles == 'State')]]

# webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
# tabTitles <- sapply(webElem, function(x){x$getElementText()})
# regressElem <- webElem[[which(tabTitles == 'Data')]]
# regressElem$clickElement()


webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
regressElem <- webElem[[which(tabTitles == 'Regression')]]
regressElem$clickElement()

str(regressElem)
x <- regressElem$findChildElement(value = "//*[@class = 'dropdown-menu']/li/a")
str(x)
regressElem$findChildElements(value = "../[@class = 'dropdown-menu']")
findChildElement
regressElem$findChildElements(value = "../[@class = 'dropdown-menu']/li/a")
regressElem$findChildElements(value = "[@class = 'dropdown-menu]")
regressElem$findChildElements(value = "../[@class = 'dropdown-menu']//a")
webElem <- regressElem$findChildElements(value = "../[@class = 'dropdown-menu']//a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
correlationElem <- webElem[[which(subTitles == 'Correlation')]]

# click the state tab

stateElem$clickElement()

regressElem$findChildElement(value = "[@class = 'dropdown-menu']")


webElem <- regressElem$findChildElement(value = "../[@class = 'dropdown-menu']//a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
correlationElem <- webElem[[which(subTitles == 'Correlation')]]

regressElem$findChildElements(value = "[@class = 'dropdown-menu']")
subTitles <- sapply(webElem, function(x){x$getElementText()})
correlationElem <- webElem[[which(subTitles == 'Correlation')]]




require(RSelenium)
setwd('~/Desktop/test')

startServer()
remDr <- remoteDriver$new()
remDr$open()
remDr$navigate("http://vnijs.rady.ucsd.edu:3838/marketing")

webElem <- remDr$findElement(using = "nav_radiant", value = "Correlation")



# close server
remDr$closeServer()

# John Harrison's example from https://groups.google.com/forum/#!msg/shiny-discuss/CSI9k5leehU/sEwroWNMRf0J
# startServer()
# remDr <- remoteDriver$new()
# remDr$open()
# remDr$navigate("http://glimmer.rstudio.com/winston/heightweight/")

# # find the X variable DOM element
# webElem <- remDr$findElement(using = "id", value = "x_var")

# # check what is selected as default
# webElem$getElementAttribute("value") # [1] "ageYear"

# # change to x = Weight option
# webElem <- remDr$findElement(value = "//option[@value = 'weightLb']")
# webElem$clickElement() # app should change to x variable = weight

# # change to x = Height
# webElem <- remDr$findElement(value = "//option[@value = 'heightIn']")
# webElem$clickElement() # app should change to x variable = weight

# # seperate male and female values
# webElem <- remDr$findElement(using = "id", value = "sex")
# webElem$clickElement() # app should change to x variable = weight

# # close server
# remDr$closeServer()


remDr$navigate("http://vnijs.rady.ucsd.edu:3838/marketing")

webElem <- remDr$findElement(value = "//*[contains(concat(' ', @class, ' '), ' btn btn-navbar ')]")
webElem$clickElement()

webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
regressElem <- webElem[[which(tabTitles == 'Regression')]]
regressElem$clickElement()

webElem <- regressElem$findChildElements(value = "..//*[@class = 'dropdown-menu']//a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
str(subTitles)
correlationElem <- webElem[[which(subTitles == 'Correlation')]]
correlationElem$clickElement()
str(correlationElem)

##########
require(RSelenium)
port = 4678
host = "127.0.0.1"

require(radiant)
require(shiny)

runApp(system.file("marketing", package="radiant")

, host = "0.0.0.0", port = 4678)

RSelenium::checkForServer()
startServer()
remDr <- remoteDriver$new()
Sys.sleep(20) # time for server
remDr$open()
#remDr$navigate(paste0("http://", host, ":", port))
remDr$navigate("http://vnijs.rady.ucsd.edu:3838/marketing")

webElem <- remDr$findElement(value = "//*[contains(concat(' ', @class, ' '), ' btn btn-navbar ')]")
webElem$clickElement()

webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
regressElem <- webElem[[which(tabTitles == 'Regression')]]
regressElem$clickElement()

webElem <- regressElem$findChildElements(value = "../[@class = 'dropdown-menu']//a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
correlationElem <- webElem[[which(subTitles == 'Correlation')]]

correlationElem$clickElement()

click the correlation tab

correlationElem$clickElement()

I will look into loading the state file later (have to go for a coffee now ;) )

—
Reply to this email directly or view it on GitHub.

