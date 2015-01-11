# only write if running on developer computer
if(file.exists("/Users/vnijs/Dropbox/GitHub/radiant")) {
  # list of all radiant files with time-stamps
  # dbox_remote <- file.info(list.files(recursive = TRUE, include.dirs = TRUE))
  # save(dbox_remote, file = "dbox_remote.rda")
  # options(shiny.reactlog=TRUE)
  # options(error = recover)
  # shiny.trace shows JSON packets transferred over websockets
  options(shiny.trace = TRUE)
  vimKeyBinding <- TRUE
} else {
  vimKeyBinding <- FALSE
}

testingRadiant <- FALSE
options(digits = 3)

# allowing anyfile size when run locally  
if(Sys.getenv('SHINY_PORT') == "") {
  # no limit to filesize locally
  options(shiny.maxRequestSize=-1)
  running_local <<- TRUE
} else {
  running_local <<- FALSE
}

setInitValues <- function() {
  # initialize state list and reactive values
  if(testingRadiant) {
    # load previous state for testing

  } else {

    state_list <<- list()
    values <<- reactiveValues()

    # initial plot height and width
    values$plotHeight <- 650
    values$plotWidth <- 650

    # Datasets can change over time (i.e. the changedata function). Therefore,
    # the data need to be a reactive value so the other reactive functions
    # and outputs that depend on these datasets will know when they are changed.
    # robj <- load("../base/data/data_init/diamonds.rda") 
    robj <- load("data/data_init/diamonds.rda") 
    df <- get(robj)
    values[["diamonds"]] <- df
    values[["diamonds_descr"]] <- attr(df,'description')
    values$datasetlist <- c("diamonds")
  }
}

setInitValues()   # using a function here so it can also be called from state.R to reset the app


# main install happens through update.R 
options(repos = c(CRAN = "http://cran.rstudio.com"))
# libs <- c("shiny", "knitr", "shinyAce", "car", "tools", "gridExtra", "markdown", "R.utils", "psych", 
#   "arm", "plyr", "reshape2", "vegan", "ggplot2", "lubridate", "wordcloud", "AlgDesign")

libs <- c("shiny", "knitr", "R.utils", "markdown", "shinyAce", "car", "MASS", "gridExtra", "psych", 
  "plyr", "dplyr","reshape2", "vegan", "lubridate", "wordcloud", "AlgDesign", "ggplot2")

# would prefer to use importFrom but ...
# detach("package:R.utils", unload=TRUE)

# problem with ggplot2::fortify

# pander for pretty printing of data and analysis output
# panderOptions('digits',3)







# check if all packages in libs are available
available <- suppressWarnings(sapply(libs, require, character.only=TRUE))
inst.libs <- libs[available == FALSE]
if(length(inst.libs) != 0) {
  install.packages(inst.libs, dependencies = TRUE)
  suppressWarnings(sapply(inst.libs, require, character.only=TRUE))
}

# binding for a text input that updates when the return key is pressed
returnTextInput <- function(inputId, label, value = "") {
  tagList(
    singleton(tags$head(tags$script(src = "js/returnTextInputBinding.js"))),
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value, class = "returnTextInput")
  )
}

# binding for a sortable list of variables or factor levels
html_list <- function(vars, id) {
  hl <- paste0("<ul id=\'",id,"\' class='stab'>")
  for(i in vars) hl <- paste0(hl, "<li class='ui-state-default stab'><span class='label'>",i,"</span></li>")
  paste0(hl, "</ul>")
}

# binding for a sortable list of variables or factor levels
returnOrder <- function(inputId, vars) {
  tagList(
    singleton(tags$head(tags$script(src = 'js/sort.js'))),
    singleton(includeCSS("www/sort.css")),
    HTML(html_list(vars, inputId)),
    tags$script(paste0("$(function() {$( '#",inputId,"' ).sortable({placeholder: 'ui-state-highlight'}); $( '#",inputId,"' ).disableSelection(); });"))
  )
}

# function to render .Rmd files to html on-the-fly
includeRmd <- function(path){
  # shiny:::dependsOnFile(path)
  contents <- paste(readLines(path, warn = FALSE), collapse = '\n')
  # do not embed image or add css
  html <- knit2html(text = contents, fragment.only = TRUE, options = "", stylesheet = "www/empty.css")
  Encoding(html) <- 'UTF-8'
  HTML(html)
}

# binding to a bootstrap popover, function by Joe Cheng https://gist.github.com/jcheng5/5913297
helpPopup <- function(title, content, placement=c('right', 'top', 'left', 'bottom'), 
  trigger=c('click', 'hover', 'focus', 'manual')) {

  tagList(
    singleton(tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })"))),
    tags$a(href = "#", `data-toggle` = "popover", title = title, `data-content` = content,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1], 
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1], tags$i(class="icon-question-sign"))
  )
}

# adding the figures path to avoid making a copy of all figures in www/figures
addResourcePath("figures", "tools/help/figures/")
# addResourcePath("www", "../base/www/")
# addResourcePath("tools", "../base/tools/")

# binding to a bootstrap modal
helpModal <- function(title, link, content) {
  html <- sprintf("<div id='%s' class='modal hide fade in' style='display: none; '>
                     <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
                       <h3>%s</h3>
                     </div>
                     <div class='modal-body'>%s</div>
                   </div>
                   <a title='Help' data-toggle='modal' href='#%s' class='icon-question-sign'></a>", link, title, content, link)
  Encoding(html) <- 'UTF-8'
  HTML(html)
}

helpAndReport <- function(title, link, content) {
  html <- sprintf("<div id='%sHelp' class='modal hide fade in' style='display: none; '>
                     <div class='modal-header'><a class='close' data-dismiss='modal' href='#'>&times;</a>
                       <h3>%s</h3>
                     </div>
                     <div class='modal-body'>%s</div>
                   </div>
                   <div>
                     <a title='Help' data-toggle='modal' href='#%sHelp' class='icon-question-sign alignleft'></a>
                     <a title='Report results' class='icon-book action-button shiny-bound-input alignright' href='#%sReport' id='%sReport'></a>
                   </div> 
                   <div style='clear: both;'></div>
                   ", link, title, content, link, link, link)
  Encoding(html) <- 'UTF-8'
  HTML(html)
}

inclMD <- function(file) return(markdownToHTML(file, options = c(""), stylesheet="www/empty.css"))
# inclMD <- function(file) return(includeHTML(file))
