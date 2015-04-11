################################################################################
# functions to set initial values and take information from r_state
# when available
################################################################################

init_state <- function(r_data) {

  # initial plot height and width
  r_data$plot_height <- 600
  r_data$plot_width <- 600

  # Joe Cheng: "Datasets can change over time (i.e. the changedata function).
  # Therefore, the data need to be a reactive value so the other reactive
  # functions and outputs that depend on these datasets will know when they
  # are changed."
  robj <- load(file.path(path,"/base/data/diamonds.rda"))
  df <- get(robj)
  r_data[["diamonds"]] <- df
  r_data[["diamonds_descr"]] <- attr(df,'description')
  r_data$datasetlist <- c("diamonds")
  r_data
}

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

  check_age_and_size <- function() {

    ids <- ls(sessionStore)
    ages <- list()
    for (i in ids) {
      session_age <- difftime(Sys.time(), sessionStore[[i]]$timestamp, units = "days")
      if(session_age > 7) sessionStore[[i]] <- NULL
      ages[i] <- session_age %>% round(3)
    }

    session_size <- pryr::object_size(sessionStore) %>% as.numeric %>%
                      {. / 1048576} %>% round(3)

    if(length(sessionStore) > 10 || session_size > 20)
      state_email(c("Session size (MB):",session_size,"\nSession ages in days:",ages))
  }

  # are there any state files dumped more than 1 minute ago?
  check_age_and_size()
}

# from Joe Cheng's https://github.com/jcheng5/shiny-resume/blob/master/session.R
isolate({
  params <- parseQueryString(session$clientData$url_search)
  prevSSUID <- params[["SSUID"]]
})

# set the session id
if(running_local) {
  ssuid <- "local"
} else {
  if(is.null(prevSSUID)) {
    ssuid <- shiny:::createUniqueId(16)
  } else {
    ssuid <- prevSSUID
  }
}

# (re)start the session and push the id into the url
session$sendCustomMessage("session_start", ssuid)

# load previous state if available
if (exists("r_state") && exists("r_data")) {
  r_data <- do.call(reactiveValues, r_data)
  r_state <- r_state
  rm(r_data, r_state, envir = .GlobalEnv)
} else if(!is.null(sessionStore[[ssuid]]$r_data)) {
  r_data <- do.call(reactiveValues, sessionStore[[ssuid]]$r_data)
  r_state <- sessionStore[[ssuid]]$r_state
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
      r_data[[paste0(df,"_descr")]] <- attr(r_data[[df]],'description') %>%
        { if(is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\n Check the 'Add/edit data description' box on the left of your screen" else . }
      r_data$datasetlist %<>% c(df, .) %>% unique
      rm(list = df, envir = .GlobalEnv)
    })
  }
}
