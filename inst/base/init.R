################################################################################
# functions to set initial values and take information from state_list
# when available
################################################################################

ip <- session$request$REMOTE_ADDR

# observe({
#   qs <- parseQueryString(session$clientData$url_search)
#   if(!is.null(qs$id) && qs$id != "")
#     ip <<- qs$id
# })

init_state <- function(values) {

  # initial plot height and width
  values$plotHeight <- 600
  values$plotWidth <- 600

  # Datasets can change over time (i.e. the changedata function). Therefore,
  # the data need to be a reactive value so the other reactive functions
  # and outputs that depend on these datasets will know when they are changed.
  robj <- load("../base/data/data_init/diamonds.rda")
  df <- get(robj)
  values[["diamonds"]] <- df
  values[["diamonds_descr"]] <- attr(df,'description')
  values$datasetlist <- c("diamonds")
  values
}

ip_inputs <- paste0("RadiantInputs",ip)
ip_values <- paste0("RadiantValues",ip)
ip_dump <- paste0("RadiantDumpTime",ip)

#### test section
# rm(list = ls())
# library(lubridate)
# library(dplyr)
# t1 <- lubridate::now()
# less_1 <- t1 - 5
# less_2 <- t1 - minutes(1)
# more_2 <- t1 - minutes(5)

# ip <- "127.0.0.1"
# ip_dump <- paste0("RadiantDumpTime",ip)
# assign(ip_dump, less_2)

# ip <- "127.0.0.2"
# ip_dump2 <- paste0("RadiantDumpTime",ip)
# assign(ip_dump2, more_2)

## email may work only on linux
# check_state_dump_times()
#### end test section

state_email <- function(body, subject = paste0("From: ", Sys.info()['nodename'])) {
  if(!require(sendmailR))
    install.packages("sendmailR", repos = "http://cran.rstudio.com")
  library(sendmailR)

  from <- '<vincent.nijs@gmail.com>'
  to <- '<vincent.nijs@gmail.com>'
  body <- paste0(body,collapse="\n")
  sendmail(from, to, subject, body,
           control=list(smtpServer='ASPMX.L.GOOGLE.COM'))
}

check_state_dump_times <- function() {

  dump_times <- ls(pattern = "^RadiantDumpTime", envir = .GlobalEnv)
  for (i in dump_times) {
    dump_time <- difftime(now(), get(i, envir=.GlobalEnv), units = "mins")
    body_part1 <- c("Before:\n",ls(pattern="^Radiant" ,envir = .GlobalEnv))
    if (dump_time > 1) {
      sub("RadiantDumpTime","",i) %>%
        paste0(c("RadiantInputs","RadiantValues","RadiantDumpTime"),.) %>%
        rm(list = ., envir = .GlobalEnv)
      body_part2 <- c("\n\nAfter:\n",ls(pattern="^Radiant" ,envir = .GlobalEnv))
      state_email(c(body_part1,body_part2))
    } else {
      state_email(c(body_part1, "\n\nDump times:\n",
                    dump_times,dump_time, "\n\nFull ls():\n",
                    ls(envir = .GlobalEnv)))
    }
  }
}

if(!running_local) {
  # are there any state files dumped more than 3 minutes ago?
  check_state_dump_times()
}

# load previous state if available
if (exists("state_list") && exists("values")) {
  values <- do.call(reactiveValues, values)
  state_list <- state_list
  rm(values, state_list, envir = .GlobalEnv)
} else if (exists(ip_inputs) && exists(ip_values)) {
  values <- do.call(reactiveValues, get(ip_values))
  state_list <- get(ip_inputs)
  rm(list = c(ip_inputs, ip_values, ip_dump), envir = .GlobalEnv)
} else {
  state_list <- list()
  values <- init_state(reactiveValues())
}

observe({
  # reset state_list on dataset change
  if(is.null(state_list$dataset) || is.null(input$dataset)) return()
  if(state_list$dataset != input$dataset) state_list <<- list()
})
