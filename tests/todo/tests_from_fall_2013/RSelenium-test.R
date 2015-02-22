# require(devtools)
# install_github("RSelenium", "johndharrison")
install_github('shiny','rstudio')
require(RSelenium)
port = 8100
host = "127.0.0.1"

#####################################################################
# make a list with values and output and save that for comparison
# using testthat
#####################################################################
# options(warn=2, error=recover)

# shiny::runApp('~/radiant/inst/marketing', launch.browser = TRUE, port = 8100)
# https://download.mozilla.org/?product=firefox-25.0&os=osx&lang=en-US
startServer()
remDr <- remoteDriver$new()
# remDr <- remoteDriver$new(browserName = 'safari')
Sys.sleep(15) # time for server
remDr$open()
Sys.sleep(5) # time for server
remDr$navigate(paste0("http://", host, ":", port))
# remDr$navigate("http://vnijs.rady.ucsd.edu:3838/marketing")

# Next line only needed if nav-bar is collapsed
# webElem <- remDr$findElement(value = "//*[contains(concat(' ', @class, ' '), ' btn btn-navbar ')]")
webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
regressElem <- webElem[[which(tabTitles == 'Regression')]]
regressElem$clickElement()

Sys.sleep(5) # time for server

webElem <- regressElem$findChildElements(value = "../*[@class = 'dropdown-menu']/*/a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
correlationElem <- webElem[[which(subTitles == 'Correlation')]]
correlationElem$clickElement()

Sys.sleep(5) # time for server

# find variables list for correlation
webElem <- remDr$findElements(value = "//*[@id = 'cor_var']/option")

# find value options
selectNames <- sapply(webElem, function(x){x$getElementAttribute('value')})

# select price, cut and color
lapply(which(selectNames%in%c("price", "cut", "color")), 
		function(x){ webElem[[x]]$clickElement() })


# finding cor_var  
# webElem <- remDr$findElements(value = "//*[@id = 'cor_var']/option")
webElem <- remDr$findElement(using = "id", value = "cor_type")

# # check what is selected as default
webElem$getElementAttribute("value") # [1] "pearson"

# change cor_type to spearman
webElem <- remDr$findElement(value = "//option[@value = 'spearman']")
webElem$clickElement() # app should change to spearman

# change cut-off to .2
webElem <- remDr$findElement(using = "id", value = "cor_cutoff")
webElem$getElementAttribute("value") 	# [1] "0"
webElem$sendKeysToElement(list(value = ".2"))

Sys.sleep(5) # time for server

# getting the output in summary_correlation
webElem <- remDr$findElements(value = "//*[@id = 'summary_correlation']")
sum_cor_res <- webElem[[1]]$getElementText()
sum_cor_res

# Going to the Plots tab
webElem <- remDr$findElements(value = "//*[@id = 'tabs_correlation']/*/a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
plotsElem <- webElem[[which(subTitles == 'Plots')]]
plotsElem$clickElement()

# Getting image src, width, and height
webElem <- remDr$findElements(value = "//*[@id = 'plots_correlation']/img")
webElem[[1]]$getElementAttribute('src')
webElem[[1]]$getElementAttribute('width')
webElem[[1]]$getElementAttribute('height')

# Going to Data > View and subsetting
webElem <- remDr$findElements(value = "//*[@id = 'nav_radiant']/li/a")
tabTitles <- sapply(webElem, function(x){x$getElementText()})
dataElem <- webElem[[which(tabTitles == 'Data')]]
dataElem$clickElement()

# Going to the View tab
webElem <- remDr$findElements(value = "//*[@id = 'datatabs']/*/a")
subTitles <- sapply(webElem, function(x){x$getElementText()})
viewElem <- webElem[[which(subTitles == 'View')]]
viewElem$clickElement()

webElem <- remDr$findElement(using = "id", value = "view_select")
webElem$getElementAttribute("value") 	# [1] "0"
webElem$sendKeysToElement(list(value = "price > 5000", key = "enter"))

webElem <- remDr$findElements(value = "//*[@class = 'dataTables_info']")
nr_obs <- webElem[[1]]$getElementText()


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

