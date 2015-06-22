################################################################################
## functions to set initial values and take information from r_state
## when available
################################################################################

init_state <- function(r_data) {

  ## initial plot height and width
  r_data$plot_height <- 600
  r_data$plot_width <- 600

  r_data$manual <- FALSE

  ## Joe Cheng: "Datasets can change over time (i.e. the changedata function).
  ## Therefore, the data need to be a reactive value so the other reactive
  ## functions and outputs that depend on these datasets will know when they
  ## are changed."
  robj <- load(file.path(r_path,"base/data/diamonds.rda"))
  df <- get(robj)
  r_data[["diamonds"]] <- df
  r_data[["diamonds_descr"]] <- attr(df,'description')
  r_data$datasetlist <- c("diamonds")
  r_data$url <- NULL
  r_data
}

if (!r_local) {

  state_email <- function(body, subject = paste0("From: ", Sys.info()['nodename'])) {
    if (!require(sendmailR)) {
      install.packages("sendmailR", repos = "http://cran.rstudio.com")
      library(sendmailR)
    }

    from <- '<vincent.nijs@gmail.com>'
    to <- '<vincent.nijs@gmail.com>'
    body <- paste0(body,collapse="\n")
    sendmail(from, to, subject, body,
             control=list(smtpServer='ASPMX.L.GOOGLE.COM'))
  }

  check_age_and_size <- function() {

    ids <- ls(r_sessions)
    ages <- list()
    for (i in ids) {
      session_age <- difftime(Sys.time(), r_sessions[[i]]$timestamp, units = "days")
      if (session_age > 1) r_sessions[[i]] <- NULL
      ages[i] <- session_age %>% round(3)
    }

    session_size <- pryr::object_size(r_sessions) %>% as.numeric %>%
                      {. / 1048576} %>% round(3)

    if (length(r_sessions) > 20 || session_size > 20)
      state_email(c("Session size (MB):",session_size,"\nSession ages in days:",ages))
  }

  ## are there any state files dumped more than 1 minute ago?
  check_age_and_size()
}

## from Joe Cheng's https://github.com/jcheng5/shiny-resume/blob/master/session.R
isolate({
  prevSSUID <- parseQueryString(session$clientData$url_search)[["SSUID"]]
})

## set the session id
r_ssuid <-
  if (r_local) {
    "local"
  } else {
    ifelse(is.null(prevSSUID), shiny:::createUniqueId(5), prevSSUID)
  }

## (re)start the session and push the id into the url
session$sendCustomMessage("session_start", r_ssuid)

## load previous state if available
if (exists("r_state") && exists("r_data")) {
  r_data  <- do.call(reactiveValues, r_data)
  r_state <- r_state
  rm(r_data, r_state, envir = .GlobalEnv)
} else if (!is.null(r_sessions[[r_ssuid]]$r_data)) {
  r_data  <- do.call(reactiveValues, r_sessions[[r_ssuid]]$r_data)
  r_state <- r_sessions[[r_ssuid]]$r_state
} else if (file.exists(paste0("~/r_sessions/r_", r_ssuid, ".rds"))) {
  ## read from file if not in global
  rs <- readRDS(paste0("~/r_sessions/r_", r_ssuid, ".rds"))
  r_data  <- do.call(reactiveValues, rs$r_data)
  r_state <- rs$r_state
  rm(rs)
} else {
  r_data  <- init_state(reactiveValues())
  r_state <- list()
}

if (r_local) {
  ## reference to radiant environment that can be accessed by exported functions
  ## does *not* make a copy of the data - nice
  r_env <<- pryr::where("r_data")

  ## adding any data.frame from the global environment to r_data should not affect
  ## memory usage ... at least until the entry in r_data is changed
  df_list <- sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.data.frame) %>%
    { names(.[.]) }

  for (df in df_list) {
    isolate({
      r_data[[df]] <- get(df, envir = .GlobalEnv)
      attr(r_data[[df]],'description')
      r_data[[paste0(df,"_descr")]] <- attr(r_data[[df]],'description') %>%
        { if (is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\n Check the 'Add/edit data description' box on the left of your screen" else . }
      r_data$datasetlist %<>% c(df, .) %>% unique
      # rm(list = df, envir = .GlobalEnv)
    })
  }
}

## relevant links
# http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs/25385474#25385474
# https://groups.google.com/forum/#!topic/shiny-discuss/Xgxq08N8HBE
# https://gist.github.com/jcheng5/5427d6f264408abf3049

#####################################
## url stuff
#####################################

url_patterns <- list(
  "data/"         = list("nav_radiant" = "Data", "tabs_data" = "Manage"),
  "data/view/"    = list("nav_radiant" = "Data", "tabs_data" = "View"),
  "data/combine/" = list("nav_radiant" = "Data", "tabs_data" = "Combine"),
  "sample/sampling/"    = list("nav_radiant" = "Sampling"),
  "sample/sample-size/" = list("nav_radiant" = "Sample size"),

  "base/single-mean/"        = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Summary"),
  "base/single-mean/plot/"   = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Plot"),
  "base/compare-means/"      = list("nav_radiant" = "Compare means", "tabs_compare_means" = "Summary"),
  "base/compare-means/plot/" = list("nav_radiant" = "Compare means", "tabs_compare_means" = "Plot"),

  "regression/correlation/"      = list("nav_radiant" = "Correlation"),
  "regression/correlation/plot/" = list("nav_radiant" = "Correlation", "tabs_correlation" = "Plot"),
  "regression/linear/"           = list("nav_radiant" = "Linear (OLS)"),
  "regression/linear/predict/"   = list("nav_radiant" = "Linear (OLS)", "tabs_regression" = "Predict"),
  "regression/linear/plot/"      = list("nav_radiant" = "Linear (OLS)", "tabs_regression" = "Plot"),
  "regression/glm/"              = list("nav_radiant" = "GLM"),
  "regression/glm/predict/"      = list("nav_radiant" = "GLM", "tabs_glm_reg" = "Predict"),
  "regression/glm/plot/"         = list("nav_radiant" = "GLM", "tabs_glm_reg" = "Plot")
)

# url_patterns <- list(
#   "base/single-mean/" = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Summary"),
#   "base/single-mean/plot/" = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Plot"),
#   "regression/correlation/" = list("nav_radiant" = "Correlation", "tabs_correlation" = "Summary"),
#   "regression/correlation/plot/" = list("nav_radiant" = "Correlation", "tabs_correlation" = "Plot"),
#   "sample/sampling/"    = list("nav_radiant" = "Sampling"),
#   "sample/sample-size/" = list("nav_radiant" = "Sample size")
# )

# library(data.table);
# url_patterns
# urlDT <- rbindlist(url_patterns,fill=TRUE); urlDT[,patt:=names(url_patterns)]
# urlDT

# url_list <-
#   list("Data" = list("tabs_data" =
#                      list("Manage" = "data/",
#                           "View" = "data/view/",
#                           "Combine" = "data/combine/")),
#        "Single mean" = list("tabs_single_mean" =
#                             list("Summary" = "base/single-mean/",
#                                  "Plot" = "base/single-mean/plot/")),
#        "Correlation" = list("tabs_correlation" =
#                             list("Summary" = "regression/correlation/",
#                                  "Plot" = "regression/correlation/plot/")),
#        "Sampling" = "sample/sampling/",
#        "Sample size"= "sample/sample-size/")

# names(url_list)
# length(url_list[["Single mean"]])
# is.list(url_list[["Single mean"]])
# length(url_list[["Sampling"]])

# url_patterns_re <- list()
# for (i in names(url_list)) {
#   res <- url_list[[i]]
#   if(!is.list(res)) {
#     url_patterns_re[[res]] <- list("nav_radiant" = i)
#   } else {
#     tabs <- names(res)
#     for (j in names(res[[tabs]])) {
#       url <- res[[tabs]][[j]]
#       url_patterns_re[[url]] <- setNames(list(i,j), c("nav_radiant",tabs))
#     }
#   }
# }

# all(unlist(url_patterns) == unlist(url_patterns_re))

# observe({
#   url_query <- parseQueryString(session$clientData$url_search)
#   if (!"url" %in% names(url_query)) return()
#   isolate(r_data$url <- url_query$url)
# })

# observe({
  # reactlist <- reactiveValuesToList(input)
  # reactvals <- grep("^ss-|^shiny-", names(reactlist), value=TRUE, invert=TRUE) # strip shiny related URL parameters
  # reactstr <- lapply(reactlist[reactvals], as.character) # handle conversion of special data types
  # input$nav_radiant
  # session$sendCustomMessage(type='setURL', reactstr)
# })

# session <- list()
# session$clientData <- list()
# session$clientData$url_search <- "?url=data/combine/&SSUID=local"
# session$clientData$url_search <- "?data/combine/&SSUID=local"
# parseQueryString(session$clientData$url_search) %>% {.[which(names(.) %in% names(url_patterns))] %>% names}

observe({
  url_query <- parseQueryString(session$clientData$url_search)
  if ("url" %in% names(url_query)) {
    r_data$url <- url_query$url
  } else if (is_empty(r_data$url)) {
    return()
  }

  ## create an observer and suspend when done
  url_observe <- observe({
    if(is.null(input$dataset)) return()
    url <- url_patterns[[r_data$url]]
    if (is.null(url)) {
      ## if pattern not found suspend observer
      url_observe$suspend()
      return()
    }
    for(u in names(url)) {
      if(is.null(input[[u]])) return()
      if(input[[u]] != url[[u]])
        updateTabsetPanel(session, u, selected = url[[u]])
      if(names(tail(url,1)) == u) url_observe$suspend()
    }
  })
})
