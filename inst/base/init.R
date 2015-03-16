################################################################################
# functions to set initial values and take information from r_state
# when available
################################################################################

# getting ip doesn't work - use shiny-resume when complete
ip <- ifelse(running_local, "", session$request$REMOTE_ADDR)

init_state <- function(r_data) {

  # initial plot height and width
  r_data$plot_height <- 600
  r_data$plot_width <- 600

  # From Joe Cheng
  # "Datasets can change over time (i.e. the changedata function). Therefore,
  # the data need to be a reactive value so the other reactive functions
  # and outputs that depend on these datasets will know when they are changed."
  robj <- load("../base/data/diamonds.rda")
  df <- get(robj)
  r_data[["diamonds"]] <- df
  r_data[["diamonds_descr"]] <- attr(df,'description')
  r_data$datasetlist <- c("diamonds")
  r_data
}

ip_inputs <- paste0("RadiantInputs",ip)
ip_data <- paste0("RadiantValues",ip)
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

if(!running_local) {

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

  # are there any state files dumped more than 1 minute ago?
  check_state_dump_times()
}

# load previous state if available
if (exists("r_state") && exists("r_data")) {
  r_data <- do.call(reactiveValues, r_data)
  r_state <- r_state
  rm(r_data, r_state, envir = .GlobalEnv)
} else if (exists(ip_inputs) && exists(ip_data)) {
  r_data <- do.call(reactiveValues, get(ip_data))
  r_state <- get(ip_inputs)
  rm(list = c(ip_inputs, ip_data, ip_dump), envir = .GlobalEnv)
} else {
  r_state <- list()
  r_data <- init_state(reactiveValues())
}

if(running_local) {
  # reference to radiant environment that can be accessed by exported functions
  # does *not* make a copy of the data - nice
  r_env <<- pryr::where("r_data")

  # adding any data.frame in the global environment to r_data should not affect
  # memory usage ... at least until the entry in r_data is changed
  df_list <- sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.data.frame) %>% { names(.[.]) }

  for(df in df_list) {
    isolate({
      r_data[[df]] <- get(df, envir = .GlobalEnv)
      attr(r_data[[df]],'description')
      r_data[[paste0(df,"_descr")]] <- attr(r_data[[df]],'description') %>% { if(is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\n Check the 'Add/edit data description' box on the left of your screen" else . }
      r_data$datasetlist %<>% c(df, .) %>% unique
      rm(list = df, envir = .GlobalEnv)
    })
  }
}
