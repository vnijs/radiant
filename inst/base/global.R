# library(shiny)
# library(magrittr)

## turn off warnings globally
# options(warn=-1)

## encoding
# options(r_encoding = getOption("encoding"))
# r_encoding = getOption("encoding")
r_encoding = "UTF-8"

## path to use for local and server use
# r_path <- ifelse ((file.exists("../base") && file.exists("../quant")), "..",
#                   system.file(package = "radiant"))
# if (r_path == "") r_path <- ".."  # if radiant is not installed revert to local inst

r_path <- ifelse ((file.exists("../base") && file.exists("../quant")), "..",
                  system.file(package = "radiant"))
if (r_path == "") r_path <- ".."  # if radiant is not installed revert to local inst

# getOption("r_path")
# options(r_path = r_path); rm(r_path)

## reactive programming in Shiny requires (some) use of global variables
## global across all users: r_sessions
## global for a user: r_data, r_state, r_ssuid
## others are set in options: r_env, r_local, r_path

## print options
options("width"  = 200)
options("scipen" = 100)

## pkgs used
r_pkgs <- c("DiagrammeR", "car", "gridExtra", "GPArotation", "psych", "wordcloud",
            "AlgDesign", "knitr", "lubridate", "ggplot2", "ggdendro",
            "pryr", "shiny", "magrittr", "tidyr", "dplyr", "broom",
            "htmlwidgets", "readr", "rmarkdown", "shinyAce", "data.tree",
            "yaml", "scales")
# options(r_pkgs = r_pkgs); rm(r_pkgs)

## list of function arguments
r_functions <-
  list("n" = "length", "n_missing" = "n_missing", "n_distinct" = "n_distinct",
       "mean" = "mean_rm", "median" = "median_rm", "mode" = "mode_rm", "sum" = "sum_rm",
       "var" = "var_rm", "sd" = "sd_rm", "se" = "serr", "cv" = "cv", "varp" = "varp_rm",
       "sdp" = "sdp_rm", "min" = "min_rm", "max" = "max_rm", "5%" = "p05",
       "25%" = "p25", "75%" = "p75", "95%" = "p95", "skew" = "skew",
       "kurtosis" = "kurtosi")
# options(r_functions = r_functions); rm(r_functions)

## for report and code in menu R
knitr::opts_knit$set(progress = TRUE)
knitr::opts_chunk$set(echo = FALSE, comment = NA, cache = FALSE, message = FALSE,
                      warning = FALSE, fig.path = "~/r_figures/")

## running local or on a server
if (Sys.getenv('SHINY_PORT') == "") {

  r_local <- TRUE
  options(shiny.maxRequestSize = -1) ## no limit to filesize locally

  ## if radiant package was not loaded load dependencies
  # if (!"package:radiant" %in% search())
    # sapply(r_pkgs, require, character.only = TRUE)

  ## needed but clunky
  # sapply(r_pkgs, require, character.only = TRUE)

} else {
  r_local <- FALSE
  options(shiny.maxRequestSize = 10 * 1024^2)   ## limit upload filesize on server (5MB)

  ## needed but clunky
  # sapply(r_pkgs, require, character.only = TRUE)
}

## needed but clunky
sapply(r_pkgs, require, character.only = TRUE)

## environment to hold session information
r_sessions <- new.env(parent = emptyenv())

## create directory to hold session files
file.path(normalizePath("~"),"r_sessions") %>% {if (!file.exists(.)) dir.create(.)}

## adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", file.path(r_path,"base/tools/help/figures/"))
addResourcePath("imgs", file.path(r_path,"base/www/imgs/"))
addResourcePath("js", file.path(r_path,"base/www/js/"))
# addResourcePath("rmarkdown", file.path(r_path,"base/www/rmarkdown/"))

## using local mathjax if available to avoid shiny bug
## https://github.com/rstudio/shiny/issues/692
## however, only use for local due to problems with mathjax rendering in IE
if (r_local && "MathJaxR" %in% installed.packages()[,"Package"]) {
  addResourcePath("MathJax", file.path(system.file(package = "MathJaxR"), "MathJax/"))
  withMathJax <- MathJaxR::withMathJaxR
}

nav_ui <-
  list(windowTitle = "Radiant", id = "nav_radiant", inverse = TRUE,
       collapsible = TRUE, tabPanel("Data", withMathJax(), uiOutput("ui_data")))

r_help <- "help_base"

shared_ui <-
  tagList(
    navbarMenu("R",
               tabPanel("Report", uiOutput("report"), icon = icon("edit")),
               tabPanel("Code", uiOutput("rcode"), icon = icon("code"))
    ),

    navbarMenu(title = "", id = "State", icon = icon("save"),
               tabPanel(downloadLink("saveStateNav", " Save state", class = "fa fa-download")),
               ## waiting for this feature in Shiny
               # tabPanel(tags$a(id = "loadStateNav", href = "", class = "shiny-input-container",
               #                 type='file', accept='.rmd,.Rmd,.md', list(icon("refresh"), "Refresh"))),
               # tabPanel(uploadLink("loadState", "Load state"), icon = icon("folder-open")),
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
    )
  )

js_head <-
  tags$head(
    tags$script(src = "js/session.js"),
    tags$script(src = "js/jquery-ui.custom.min.js"),
    tags$script(src = "js/returnTextAreaBinding.js"),
    tags$script(src = "js/returnTextInputBinding.js"),
    # tags$script(src = "js/draggable_modal.js"),
    tags$script(src = "js/video_reset.js"),
    tags$link(rel = "shortcut icon", href = "imgs/icon.png")
  )


