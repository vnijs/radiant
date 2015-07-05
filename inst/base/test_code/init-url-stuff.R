################################################################################
## functions to set initial values and take information from r_state
## when available
################################################################################

# init_state <- function(r_data) {
#
#   ## initial plot height and width
#   r_data$plot_height <- 600
#   r_data$plot_width <- 600
#
#   r_data$manual <- FALSE

  ## Joe Cheng: "Datasets can change over time (i.e. the changedata function).
#   ## Therefore, the data need to be a reactive value so the other reactive
#   ## functions and outputs that depend on these datasets will know when they
#   ## are changed."
#   robj <- load(file.path(r_path,"base/data/diamonds.rda"))
#   df <- get(robj)
#   r_data[["diamonds"]] <- df
#   r_data[["diamonds_descr"]] <- attr(df,'description')
#   r_data$datasetlist <- c("diamonds")
#   r_data$url <- NULL
#   r_data
# }
#
# if (!r_local) {
#
#   state_email <- function(body, subject = paste0("From: ", Sys.info()['nodename'])) {
#     if (!require(sendmailR)) {
#       install.packages("sendmailR", repos = "http://cran.rstudio.com")
#       library(sendmailR)
#     }
#
#     from <- '<vincent.nijs@gmail.com>'
#     to <- '<vincent.nijs@gmail.com>'
#     body <- paste0(body,collapse="\n")
#     sendmail(from, to, subject, body,
#              control=list(smtpServer='ASPMX.L.GOOGLE.COM'))
#   }
#
#   check_age_and_size <- function() {
#
#     ids <- ls(r_sessions)
#     ages <- list()
#     for (i in ids) {
#       session_age <- difftime(Sys.time(), r_sessions[[i]]$timestamp, units = "days")
#       if (session_age > 1) r_sessions[[i]] <- NULL
#       ages[i] <- session_age %>% round(3)
#     }
#
#     session_size <- pryr::object_size(r_sessions) %>% as.numeric %>%
#                       {. / 1048576} %>% round(3)
#
#     if (length(r_sessions) > 20 || session_size > 20)
#       state_email(c("Session size (MB):",session_size,"\nSession ages in days:",ages))
#   }
#
#   ## are there any state files dumped more than 1 minute ago?
#   check_age_and_size()
# }
#
# ## from Joe Cheng's https://github.com/jcheng5/shiny-resume/blob/master/session.R
# isolate({
#   prevSSUID <- parseQueryString(session$clientData$url_search)[["SSUID"]]
# })
#
# ## set the session id
# r_ssuid <-
#   if (r_local) {
#     "local"
#   } else {
#     ifelse(is.null(prevSSUID), shiny:::createUniqueId(5), prevSSUID)
#   }
#
# ## (re)start the session and push the id into the url
# session$sendCustomMessage("session_start", r_ssuid)
#
# ## load previous state if available
# if (exists("r_state") && exists("r_data")) {
#   r_data  <- do.call(reactiveValues, r_data)
#   r_state <- r_state
#   rm(r_data, r_state, envir = .GlobalEnv)
# } else if (!is.null(r_sessions[[r_ssuid]]$r_data)) {
#   r_data  <- do.call(reactiveValues, r_sessions[[r_ssuid]]$r_data)
#   r_state <- r_sessions[[r_ssuid]]$r_state
# } else if (file.exists(paste0("~/r_sessions/r_", r_ssuid, ".rds"))) {
#   ## read from file if not in global
#   rs <- readRDS(paste0("~/r_sessions/r_", r_ssuid, ".rds"))
#   r_data  <- do.call(reactiveValues, rs$r_data)
#   r_state <- rs$r_state
#   rm(rs)
# } else {
#   r_data  <- init_state(reactiveValues())
#   r_state <- list()
# }
#
# if (r_local) {
#   ## reference to radiant environment that can be accessed by exported functions
#   ## does *not* make a copy of the data - nice
#   r_env <<- pryr::where("r_data")
#
#   ## adding any data.frame from the global environment to r_data should not affect
#   ## memory usage ... at least until the entry in r_data is changed
#   df_list <- sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), is.data.frame) %>%
#     { names(.[.]) }
#
#   for (df in df_list) {
#     isolate({
#       r_data[[df]] <- get(df, envir = .GlobalEnv)
#       attr(r_data[[df]],'description')
#       r_data[[paste0(df,"_descr")]] <- attr(r_data[[df]],'description') %>%
#         { if (is.null(.)) "No description provided. Please use Radiant to add an overview of the data in markdown format.\n Check the 'Add/edit data description' box on the left of your screen" else . }
#       r_data$datasetlist %<>% c(df, .) %>% unique
#       # rm(list = df, envir = .GlobalEnv)
#     })
#   }
# }
#
# # r_url <- list()
# # r_url <<- list(id = "nav_radiant", menu = "Data", base = "Data", tab = "Manage")
# # r_url$tab <- if(is.null(r_url$tab)) "" else r_url$tab
#
# ## relevant links
# # http://stackoverflow.com/questions/25306519/shiny-saving-url-state-subpages-and-tabs/25385474#25385474
# # https://groups.google.com/forum/#!topic/shiny-discuss/Xgxq08N8HBE
# # https://gist.github.com/jcheng5/5427d6f264408abf3049
#
# # if (is.null(r_state$url)) r_state$url <- list()
# # r_state$url <- c("Data", "Combine")
# # r_state$url <- "Sampling"
# # r_state$url <- "Sample size"
# # r_state$url <- c("Single mean", "Plot")
# # r_state$url <- c("Compare means", "Plot")
# # r_state$url <- c("Single proportion", "Plot")
# # r_state$url <- c("Compare proportions", "Plot")
# # r_state$url <- c("Cross-tabs", "Plot")
# # r_state$url <- c("Correlation", "Plot")
# # r_state$url <- c("Linear (OLS)", "Predict")
# # r_state$url <- c("GLM", "Predict")
#
# # if (is.null(r_state$url)) r_state$url <- ""
# # r_state$url <- "regression/glm/predict/"
#
# #####################################
# ## url stuff
# #####################################
#
# # isolate(if (is.null(r_data$url)) r_data$url <- "regression/glm/predict/")
# isolate(if (is.null(r_data$url)) r_data$url <- "regression/glm/plot/")
#
# # url_observe <- observe({
# observe({
#   # if(is_empty(r_state$url)) return()
#   if(is_empty(r_data$url)) return()
#   if(is.null(input$dataset)) return()
#   # url_depth <- length(r_state$url)
#   # if(url_depth == 0) return()
#   # if(!is.null(r_data$url)) return()
#   # input$nav_radiant
#   # query <- parseQueryString(session$clientData$url_search)
#   # if ('tab' %in% names(queryParams))
#   # if(!is.null(query$url)) {
#   # url <- strsplit(query$url,"/")[[1]]
#   #       url1 <<- url[1]
#   #       url2 <<- url[2]
#   #       updateTabsetPanel(session, 'someID', url1)
#
#   # url <- "data/manage"
#   # url <- "base/single-mean"
#   # url <- "base/single-mean/plot"
#   # url <- "base/sampling"
#   # strsplit(url, "/")
#
#   url_pattern <- list(
#     "data/"      = list("nav_radiant" = "Data"),
#     "data/view/" = list("nav_radiant" = "Data", "tabs_data" = "View"),
#     "sample/sampling/"    = list("nav_radiant" = "Sampling"),
#     "sample/sample-size/" = list("nav_radiant" = "Sample size"),
#     "base/single-mean/"      = list("nav_radiant" = "Single mean"),
#     "base/single-mean/plot/" = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Plot"),
#     "regression/correlation/"      = list("nav_radiant" = "Correlation"),
#     "regression/correlation/plot/" = list("nav_radiant" = "Correlation", "tabs_correlation" = "Plot"),
#     "regression/linear/"         = list("nav_radiant" = "Linear (OLS)"),
#     "regression/linear/predict/" = list("nav_radiant" = "Linear (OLS)", "tabs_regression" = "Predict"),
#     "regression/linear/plot/"    = list("nav_radiant" = "Linear (OLS)", "tabs_regression" = "Plot"),
#     "regression/glm/"         = list("nav_radiant" = "GLM"),
#     "regression/glm/predict/" = list("nav_radiant" = "GLM", "tabs_glm_reg" = "Predict"),
#     "regression/glm/plot/"    = list("nav_radiant" = "GLM", "tabs_glm_reg" = "Plot")
#   )
#
#   # test <- observe({})
#
#   # url <- url_pattern[["data/"]]
#   # url <- url_pattern[["data/view/"]]
#   # url <- url_pattern[["base/single-mean/plot/"]]
#   # url <- url_pattern[["sample/sampling/"]]
#   # url <- url_pattern[["sample/sample-size/"]]
#   test <- observe({
#     url <- url_pattern[[r_data$url]]
#     # tail(url,1)
#     # updateTabsetPanel(session, "nav_radiant", selected = url[["nav_radiant"]])
#     # for(u in names(url)[-1]) {
#     for(u in names(url)) {
#       if(is.null(input[[u]])) return()
#       if(input[[u]] != url[[u]])
#         updateTabsetPanel(session, u, selected = url[[u]])
#       # if(names(tail(url,1)) == u) url_observe$suspend()
#       if(names(tail(url,1)) == u) test$suspend()
#     }
#   })
#   # updateTabsetPanel(session, "nav_radiant", selected = url[["nav_radiant"]])
#   # if (length(url) == 1) return()
#   # tab <- names(url[2])
#   # if (is.null(input[[tab]])) {
#   #   r_waiting <<- TRUE
#   #   return()
#   # }
#   # if (r_waiting) {
#   #   updateTabsetPanel(session, tab, selected = url[[tab]])
#   #   r_waiting <<- FALSE
#   # }
#
#   # updateTabsetPanel(session, "nav_radiant", selected = r_state$url[1])
#   # if(url_depth == 1) return()
#   # tab <- url_list[[r_state$url[1]]]
#   # if(is.null(input[[tab]])) return()
#   # updateTabsetPanel(session, tab, selected = r_state$url[2])
#
#   # url_list <- list("Data" = "tabs_data",
#   #                  "Single mean" = "tabs_single_mean",
#   #                  "Compare means" = "tabs_compare_means",
#   #                  "Single proportion" = "tabs_single_prop",
#   #                  "Compare proportions" = "tabs_compare_props",
#   #                  "Cross-tabs" = "tabs_cross_tabs",
#   #                  "Correlation" = "tabs_correlation",
#   #                  "Linear (OLS)" = "tabs_regression",
#   #                  "GLM" = "tabs_glm_reg")
#
#   # updateTabsetPanel(session, "nav_radiant", selected = r_state$url[1])
#   # if(url_depth == 1) return()
#   # tab <- url_list[[r_state$url[1]]]
#   # if(is.null(input[[tab]])) return()
#   # updateTabsetPanel(session, tab, selected = r_state$url[2])
# })
#
#
# # whereami <- list()
# # for(up in names(url_patterns)) {
# #   # paste(url_patterns[[i]], collapse="/") %>% print
# #   print(up)
# #   key <- url_patterns[[up]]$nav_radiant
# #   if (!key %in% names(whereami))
# #     whereami[key] <- names(url_patterns[[up]])[[2]]
# # }
# # # whereami
# # # whereami["test"] <- "stuff"
# # whereami
# # url_patterns[[up]]
#
# # url_patterns[[up]]
# # up
#
# # list("Single mean" = list("tab" = "tabs_single_mean", "Summary" = , "Plot" = ))
#
#
# all(unlist(url_p) == unlist(url_patterns))
#
# res <- test[["Single mean"]]
# res <- test[["Sampling"]]
# input <- list()
# input$tabs_single_mean <- "Summary"
#
# if (!is.list(res)) {
#   res
# } else {
#   names(res) %>% {res[[.]][[input[[.]]]]}
# }
#
#     )
# test[["Single mean"]] %>% names
#      # "tabs_single_mean/Plot" = "base/single-mean/plot/")
#
# # list(list(a = 1, b = 2) = 4)
