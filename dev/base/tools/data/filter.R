# # UI-elements for explore
# output$uiFilter_columns <- renderUI({
#   vars <- varnames()
#   if(length(vars) == 0) return()
#   selectizeInput("fil_columns", label = "Select columns(s):", choices = as.list(vars),
#     selected = state_multvar("fil_columns",vars), multiple = TRUE,
#     options = list(placeholder = 'Select column(s)', plugins = list('remove_button', 'drag_drop'))
#   )
# })
#
# output$ui_FExplore <- renderUI({
#   list(
#     wellPanel(
#       uiOutput("uiFilter_columns"),
#       returnTextInput("expl_fil", "Subset (e.g., price > 5000)", state_init("expl_select")),
#     ),
#     helpAndReport('Filter','rfilter',inclMD("../base/tools/help/explore.md"))
#   )
# })
#
# observe({
#   if(is.null(input$exploreReport) || input$exploreReport == 0) return()
#   isolate({
#     inp <- list(input$datasets, input$fil_columns, input$fil_filter)
#     updateReport(inp,"rfilter")
#   })
# })
#
# .explore <- reactive({
#   if(is.null(input$fil_columns)) return()
#   if(is.null(inChecker(input$fil_columns))) return()
#
#   withProgress(message = 'Calculating', value = 0, {
#     explore(input$datasets, input$expl_columns, input$expl_byvar, input$expl_function, input$expl_select, input$expl_show_tab, input$expl_show_viz)
#   })
# })
#
# explore <- function(datasets, expl_columns, expl_byvar, expl_function, expl_select, expl_show_tab, expl_show_viz) {
#
#   dat <- values[[datasets]]
#   if(expl_select != '') {
#     selcom <- gsub(" ", "", expl_select)
#     seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
#     if(!is(seldat, 'try-error')) {
#       if(is.data.frame(seldat)) {
#         dat <- seldat
#         seldat <- NULL
#       }
#     }
#   }
#
#   if(is.null(expl_byvar)) {
#     dat <- dat[,expl_columns, drop = FALSE]
#     isNum <- sapply(dat, is.numeric)
#     if(sum(isNum) > 0) {
#       res <- data.frame(psych::describe(dat[isNum])[,c("n","mean","median","min","max","sd","se","skew","kurtosis")])
#
#       # adding Coefficient of Variation
#       cv <- res$sd/res$mean
#       res <- cbind(res,cv)
#
#       # adding Q1 and Q3
#       perc <- function(x) quantile(x,c(.25,.75))
#       percres <- colwise(perc)(dat[,isNum, drop = FALSE])
#       rownames(percres) <- c("25%","75%")
#       res <- cbind(res,t(percres))
#
#       # number of missing values
#       res$missing <- c(colwise(nmissing)(dat[,isNum, drop = FALSE]))
#
#       # when you move to dplyr have the stats selected here be determined by the set of selected functions
#       # return desired stats in order
#       return(res[,c("n","mean","median","25%","75%","min","max","sd","se","cv","skew","kurtosis","missing")])
#     }
#   } else {
#     dat <- dat[,c(expl_byvar,expl_columns)]
#     plyres <- list()
#     for(func in expl_function) {
#       # if(sum(func %in% c('length','nmissing') > 0)) {
#       if(func %in% c('length','nmissing')) {
#         plyres[[func]] <- ddply(dat, c(expl_byvar), colwise(func))
#       } else {
#         plyres[[func]] <- ddply(dat, c(expl_byvar), colwise(func, na.rm = TRUE))
#       }
#     }
#
#     plyres$expl_columns <- expl_columns
#     plyres$expl_function <- expl_function
#     plyres$expl_byvar <- expl_byvar
#     plyres$expl_show_viz <- expl_show_viz
#
#     return(plyres)
#   }
# }
#
# .summary_explore <- reactive({
#
#   result <- .explore()
#   if(is.null(result)) return(invisible())
#   summary_explore(result)
# })
#
# summary_explore <- function(result = .explore()) {
#
#   if(class(result)[1] != 'list') {
#     cat("Summarize numeric variables:\n")
#     print(result)
#   } else {
#     for(func in result$expl_function) {
#       cat("Results grouped by: ", result$expl_byvar, "\n")
#       cat("Function used: ", names(which(expl_functions == func)), "\n")
#       print(result[[func]])
#       cat("\n")
#     }
#   }
# }
#
# output$expl_summary <- renderPrint({
#
#   if(!is.null(input$expl_show_tab) && !input$expl_show_tab) return(invisible())
#   .summary_explore()
# })
#
# .plots_explore <- reactive({
#
#   result <- .explore()
#   if(is.null(result)) return()
#
#   withProgress(message = 'Making plot', value = 0, {
#     plots_explore(result)
#   })
# })
#
# plots_explore <- function(result = .explore()) {
#
#   if(is.null(result$expl_show_viz) || result$expl_show_viz == FALSE) return(invisible())
#
#   by_var <- fill_var <- result$expl_byvar[1]
#   if(length(result$expl_byvar) > 1) fill_var <- result$expl_byvar[2]
#
#   plots <- list()
#   for(func in result$expl_function) {
#     for(var in result$expl_columns) {
#       plots[[paste0(var,"_",func)]] <- ggplot(data = result[[func]], aes_string(x = by_var, y = var, fill = fill_var)) +
#         geom_bar(stat="identity", position = "dodge", alpha=.3) +
#         ggtitle(paste("Function used:", names(which(expl_functions == func))))
#     }
#   }
#
#   do.call(grid.arrange, c(plots, list(ncol = 1)))
# }
#
# expl_plot_width <- function() {
#   # return(input$expl_plot_width)
#   650
# }
#
# expl_plot_height <- function() {
#   # return(input$expl_plot_height)
#   400 * length(input$expl_function) * length(input$expl_columns)
# }
#
# output$expl_plots <- renderPlot({
#
#   # if(isolate(input$datatabs) != 'Explore') return(invisible())
#
#   if(!input$expl_show_viz || is.null(input$expl_byvar)) return()
#   .plots_explore()
# }, width = expl_plot_width, height = expl_plot_height)
#
# #######################################
# ### When Explore is moved to dplyr
# #######################################
#
# # require(devtools)
# # install_github("assertthat")
# # install_github("dplyr")
#
# # require(assertthat)
# # require(dplyr)
#
# # filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
# # head(select(hflights, Year:DayOfWeek))
# # summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))
#
# # by_dest <- group_by(hflights, Dest)
# # filter(by_dest, ArrDelay == max(ArrDelay))
#
# # res <- summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))
#
# # by_day <- group_by(hflights, Year, Month, DayofMonth)
# # by_month <- summarise(by_day, delayed = sum(ArrDelay > 0, na.rm = TRUE))
# # by_month
# # summarise(summarise(by_month, delayed = sum(delayed)), delayed = sum(delayed))
# # summarise(by_month, delayed = sum(delayed))
#
# # by_dest <- group_by(hflights, Dest)
# # filter(by_dest, ArrDelay == max(ArrDelay))
# # summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))
