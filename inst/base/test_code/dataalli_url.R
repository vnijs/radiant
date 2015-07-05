library(shiny)

url1 <- url2 <- ""

# runApp(list(
#   ui = shinyUI(
#     navbarPage( "test", id = 'someID',
#                 tabPanel("tab1", h1("page1")),
#
#                 navbarMenu( "menu",
#                             tabPanel('tab2a',  value='foo', h1("page2a")),
#                             tabPanel('tab2b',  value='bar', h1("page2b")),
#                             tabPanel('tab_sub',  uiOutput("sub_tabPanel"))
#                 ),
#                 tabPanel("tab3", h1("page3"))
#     )),
#
#   server = function(input, output, session) {
#
#     output$sub_tabPanel <- renderUI({
#       tabsetPanel(id = "tab_sub_tabs",
#                   tabPanel("Summary", h1("sub1")),
#                   tabPanel(
#                     "Plot", h2("sub2"),
#                     tabsetPanel(
#                       id = "Plot_tabs",
#                       tabPanel("plot1", "plot1"),
#                       tabPanel("plot2", "plot2")
#                     )
#                   )
#       )
#     })
#
#     values <- reactiveValues(myurl = c(), parent_tab = "")
#
#     observe({
#       # make sure this is called on pageload (to look at the query string)
#       # and whenever any tab is successfully changed.
#       # If you want to stop running this code after the initial load was
#       # successful so that further manual tab changes don't run this,
#       # maybe just have some boolean flag for that.
#
#       input$someID
#       input$tab_sub_tabs
#       query <- parseQueryString(session$clientData$url_search)
#       url <- query$url
#       url <-
#       if (is.null(url)) {
#         url <- ""
#       }
#
#       # "depth" is how many levels the url in the query string is
#       depth <- function(x) length(unlist(strsplit(x,"/")))
#
#       # if we reached the end, done!
#       if (length(values$myurl) == depth(url)) {
#         return()
#       }
#       # base case - need to tell it what the first main nav name is
#       else if (length(values$myurl) == 0) {
#         values$parent_tab <- "someID"
#       }
#       # if we're waiting for a tab switch but the UI hasn't updated yet
#       else if (is.null(input[[values$parent_tab]])) {
#         return()
#       }
#       # same - waiting for a tab switch
#       else if (tail(values$myurl, 1) != input[[values$parent_tab]]) {
#         return()
#       }
#       # the UI is on the tab that we last switched to, and there are more
#       # tabs to switch inside the current tab
#       # make sure the tabs follow the naming scheme
#       else {
#         values$parent_tab <- paste0(tail(values$myurl, 1), "_tabs")
#       }
#
#       # figure out the id/value of the next tab
#       new_tab <- unlist(strsplit(url, "/"))[length(values$myurl)+1]
#
#       # easy peasy.
#       updateTabsetPanel(session, values$parent_tab, new_tab)
#       values$myurl <- c(values$myurl, new_tab)
#     })
#   }
# ))
