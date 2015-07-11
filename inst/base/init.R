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

#####################################
## url processing to share results
#####################################

## relevant links
# http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs/25385474#25385474
# https://groups.google.com/forum/#!topic/shiny-discuss/Xgxq08N8HBE
# https://gist.github.com/jcheng5/5427d6f264408abf3049

## try http://127.0.0.1:3174/?url=decide/simulate/&SSUID=local

url_list <-
  list("Data" = list("tabs_data" = list("Manage"    = "data/",
                                        "View"      = "data/view/",
                                        "Visualize" = "data/visualize/",
                                        "Pivot"     = "data/pivot/",
                                        "Explore"   = "data/explore/",
                                        "Transform" = "data/transform/",
                                        "Combine"   = "data/combine/")),

       "Sampling"    = "sample/sampling/",
       "Sample size" = "sample/sample-size/",

       "Single mean" = list("tabs_single_mean" = list("Summary" = "base/single-mean/",
                                                      "Plot"    = "base/single-mean/plot/")),

       "Compare means" = list("tabs_compare_means" = list("Summary" = "base/compare-means/",
                                                          "Plot"    = "base/compare-means/plot/")),

       "Single proportion" = list("tabs_single_prop" = list("Summary" = "base/single-prop/",
                                                            "Plot"    = "base/single-prop/plot/")),

       "Compare proportions" = list("tabs_compare_props" = list("Summary" = "base/compare-props/",
                                                                "Plot"    = "base/compare-props/plot/")),

       "Cross-tabs" = list("tabs_cross_tabs" = list("Summary" = "base/cross-tabs/",
                                                     "Plot"    = "base/cross-tabs/plot/")),

       "Correlation" = list("tabs_correlation" = list("Summary" = "regression/correlation/",
                                                      "Plot"    = "regression/correlation/plot/")),

       "Linear (OLS)" = list("tabs_regression" = list("Summary" = "regression/linear/",
                                                      "Predict" = "regression/linear/predict/",
                                                      "Plot"    = "regression/linear/plot/")),

       "GLM" = list("tabs_glm_reg" = list("Summary" = "regression/glm/",
                                          "Predict" = "regression/glm/predict/",
                                          "Plot"    = "regression/glm/plot/")),

       "Simulate"    = list("tabs_simulate"    = list("Model"   = "decide/simulate/",
                                                      "Repeat"  = "decide/simulate/repeat/"))
  )

## generate url patterns for navigation
url_patterns <- list()
for (i in names(url_list)) {
  res <- url_list[[i]]
  if(!is.list(res)) {
    url_patterns[[res]] <- list("nav_radiant" = i)
  } else {
    tabs <- names(res)
    for (j in names(res[[tabs]])) {
      url <- res[[tabs]][[j]]
      url_patterns[[url]] <- setNames(list(i,j), c("nav_radiant",tabs))
    }
  }
}

## try http://127.0.0.1:3174/?url=decide/simulate/&SSUID=local
# unlink("~/gh/radiant/tests/urls/urls.Rmd")
# urls <- grep("/", url_list %>% unlist, value = TRUE)
# for(u in urls) {
#   cat(paste0("http://127.0.0.1:6452/?url=", u, "&SSUID=local<br>\n"),
#       file = "~/gh/radiant/tests/urls/urls.Rmd", append = TRUE)
# }
# knitr::knit2html("~/gh/radiant/tests/urls/urls.Rmd", output = "~/gh/radiant/tests/urls/urls.html")

## parse the url and use updateTabsetPanel to navigate to the desired tab
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
