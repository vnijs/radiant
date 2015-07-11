# url_patterns <- list(
#   "data/"         = list("nav_radiant" = "Data", "tabs_data" = "Manage"),
#   "data/view/"    = list("nav_radiant" = "Data", "tabs_data" = "View"),
#   "data/combine/" = list("nav_radiant" = "Data", "tabs_data" = "Combine"),
#   "sample/sampling/"    = list("nav_radiant" = "Sampling"),
#   "sample/sample-size/" = list("nav_radiant" = "Sample size"),

#   "base/single-mean/"        = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Summary"),
#   "base/single-mean/plot/"   = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Plot"),
#   "base/compare-means/"      = list("nav_radiant" = "Compare means", "tabs_compare_means" = "Summary"),
#   "base/compare-means/plot/" = list("nav_radiant" = "Compare means", "tabs_compare_means" = "Plot"),

#   "regression/correlation/"      = list("nav_radiant" = "Correlation"),
#   "regression/correlation/plot/" = list("nav_radiant" = "Correlation", "tabs_correlation" = "Plot"),
#   "regression/linear/"           = list("nav_radiant" = "Linear (OLS)"),
#   "regression/linear/predict/"   = list("nav_radiant" = "Linear (OLS)", "tabs_regression" = "Predict"),
#   "regression/linear/plot/"      = list("nav_radiant" = "Linear (OLS)", "tabs_regression" = "Plot"),
#   "regression/glm/"              = list("nav_radiant" = "GLM"),
#   "regression/glm/predict/"      = list("nav_radiant" = "GLM", "tabs_glm_reg" = "Predict"),
#   "regression/glm/plot/"         = list("nav_radiant" = "GLM", "tabs_glm_reg" = "Plot")
# )

# url_patterns <- list(
#   "base/single-mean/" = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Summary"),
#   "base/single-mean/plot/" = list("nav_radiant" = "Single mean", "tabs_single_mean" = "Plot"),
#   "regression/correlation/" = list("nav_radiant" = "Correlation", "tabs_correlation" = "Summary"),
#   "regression/correlation/plot/" = list("nav_radiant" = "Correlation", "tabs_correlation" = "Plot"),
#   "sample/sampling/"    = list("nav_radiant" = "Sampling"),
#   "sample/sample-size/" = list("nav_radiant" = "Sample size")
# )

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
