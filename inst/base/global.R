## encoding
options(encoding = "native.enc") ## default
# options(encoding = "UTF-8")      ## for chines
## use getOption("encoding") to see if things were changed

loc <- function(os, language = "english") {
  switch(language,
         english = ifelse(os == "Windows", "English_United States.1252", "en_US.UTF-8"),
         chinese = ifelse(os == "Windows", "Chinese", "zh_CN.utf-8"))
}

## setting local
Sys.setlocale(category = "LC_ALL", loc(Sys.info()[["sysname"]]))
# Sys.setlocale(category = "LC_ALL", loc(Sys.info()[["sysname"]], "chinese"))
## use Sys.setlocale() to see if things were changed
# Sys.setlocale()

## path to use for local and server use
r_path <- ifelse((file.exists("../base") && file.exists("../quant")), "..",
                  system.file(package = "radiant"))
if (r_path == "") r_path <- ".."  # if radiant is not installed revert to local inst

## reactive programming in Shiny requires (some) use of global variables
## currently these are r_env, r_data, r_state, r_local, r_path, r_sessions, r_ssuid

## print options
options("width" = 200)
options("scipen" = 100)

## pkgs used
pkgs_cran <- c("car", "gridExtra", "GPArotation", "psych", "wordcloud",
               "AlgDesign", "knitr", "lubridate", "ggplot2", "ggdendro",
               "pryr", "shiny", "magrittr", "tidyr", "dplyr", "broom",
               "htmlwidgets", "readr", "rmarkdown")
pkgs_gh <- c("shinyAce")
pkgs <- c(pkgs_cran, pkgs_gh)
rm(pkgs_cran,pkgs_gh)

## list of function arguments
expl_functions <-
  list("n" = "length", "mean" = "mean_rm", "median" = "median_rm",
       "sum" = "sum_rm", "min" = "min_rm", "max" = "max_rm", "25%" = "p25",
       "75%" = "p75", "sd" = "sd_rm", "se" = "serr", "cv" = "cv",
       "skew" = "skew", "kurtosis" = "kurtosi", "# missing" = "nmissing")

## for report and code in menu R
knitr::opts_knit$set(progress = TRUE)
knitr::opts_chunk$set(echo=FALSE, comment=NA, cache=FALSE, message=FALSE,
                      warning=FALSE, fig.path = "~/r_figures/")

## using DT rather than Shiny versions of datatable
renderDataTable <- DT::renderDataTable
dataTableOutput <- DT::dataTableOutput
datatable       <- DT::datatable

## running local or on a server
if (Sys.getenv('SHINY_PORT') == "") {

  r_local <- TRUE
  options(shiny.maxRequestSize = -1) ## no limit to filesize locally

  ## if radiant package was not loaded load dependencies
  if (!"package:radiant" %in% search())
    sapply(pkgs, require, character.only = TRUE)

} else {
  r_local <- FALSE
  options(shiny.maxRequestSize = 5 * 1024^2)   ## limit upload filesize on server (5MB)
  sapply(pkgs, require, character.only = TRUE)
}

## environment to hold session information
r_sessions <- new.env(parent = emptyenv())

## create directory to hold session files
if (!r_local)
  "~/r_sessions/" %>% { if (!file.exists(.)) dir.create(., recursive = TRUE) }

## adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", file.path(r_path,"base/tools/help/figures/"))
addResourcePath("imgs", file.path(r_path,"base/www/imgs/"))
addResourcePath("js", file.path(r_path,"base/www/js/"))

## using local mathjax if available to avoid shiny bug
## https://github.com/rstudio/shiny/issues/692
## however, only use for local due to problems with mathjax rendering in IE
if (r_local && "MathJaxR" %in% installed.packages()[,"Package"]) {
  addResourcePath("MathJax", file.path(system.file(package = "MathJaxR"), "MathJax/"))
  withMathJax <- MathJaxR::withMathJaxR
}

## options used for debugging
# options(shiny.trace = TRUE)
# options(shiny.error = recover)
# options(warn=2)
# options(warn=0)

## Windows or Mac
# if (.Platform$OS.type == 'windows') {
#   Sys.setlocale(category = 'LC_ALL','English_United States.1252')
# } else {
#   Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
# }

nav_ui <-
  list(windowTitle = "Radiant", id = "nav_radiant", inverse = TRUE,
       collapsible = TRUE, tabPanel("Data", withMathJax(), uiOutput("ui_data")))

shared_ui <-
  tagList(
    navbarMenu("R",
               tabPanel("Report", uiOutput("report"), icon = icon("edit")),
               tabPanel("Code", uiOutput("rcode"), icon = icon("code"))
    ),

    navbarMenu(title = "", id = "State", icon = icon("save"),
               tabPanel(downloadLink("saveStateNav", " Save state", class = "fa fa-download")),
               # tabPanel(downloadLink("loadState", "Load state"), icon = icon("folder-open")),
               tabPanel(actionLink("shareState", "Share state", icon = icon("share"))),
               tabPanel("View state", uiOutput("view_state"), icon = icon("user"))
    ),

    ## works but badly aligned in navbar
    # tabPanel(tags$a(id = "quitApp", href = "#", class = "action-button",
    #          list(icon("power-off"), ""), onclick = "window.close();")),

    ## stop app *and* close browser window
    navbarMenu(title = "", id = "Stop", icon = icon("power-off"),
               tabPanel(actionLink("stop_radiant", "Stop", icon = icon("stop"),
                                   onclick = "setTimeout(function(){window.close();}, 100); ")),
               tabPanel(tags$a(id = "refresh_radiant", href = "#", class = "action-button",
                               list(icon("refresh"), "Refresh"), onclick = "window.location.reload();")),
               ## had to remove class = "action-button" to make this work
               tabPanel(tags$a(id = "new_session", href = "./", target = "_blank",
                               list(icon("plus"), "New session")))
    ),

    navbarMenu(title = "", id = "Help", icon = icon("question-circle"),
               tabPanel("Help", uiOutput("help_quant"), icon = icon("question")),
               tabPanel("Videos", uiOutput("help_videos"), icon = icon("film")),
               tabPanel("About", uiOutput("help_about"), icon = icon("info")),
               tabPanel(tags$a("", href = "http://vnijs.github.io/radiant/", target = "_blank",
                               list(icon("globe"), "Radiant docs")))
    ),

    tags$head(
      tags$script(src = "js/session.js"),
      tags$script(src = "js/jquery-ui.custom.min.js"),
      tags$script(src = "js/video_reset.js"),
      tags$link(rel = "shortcut icon", href = "imgs/icon.png")
    )
  )


