output$ui_filter_vars1 <- renderUI({
  vars <- varnames()
  selectizeInput(inputId = "filter_vars1",
                 label = "X-variable",
                 choices = vars,
                 selected = state_init_list("filter_vars1",vars[1], vars),
                 multiple = FALSE)
})

output$ui_filter_vars2 <- renderUI({
 	isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
 	vars <- varnames()[isNum]
  selectizeInput(inputId = "filter_vars2",
                 label = "Y-variable",
                 choices = c("None" = "None", vars),
                 selected = state_init_list("filter_vars2","None", vars),
                 multiple = FALSE)
})

output$ui_filter_fill <- renderUI({
  vars <- varnames()
  v1 <- input$filter_vars1
  v2 <- input$filter_vars2
  if(v1 %>% no_val) return(NULL)
  if(getdata_class()[v1] == 'factor') return(NULL)
  if(v2 == 'None') return(NULL)
  selectizeInput(inputId = "filter_fill",
                 label = "Color",
                 choices = c("None" = "None", vars),
                 # selected = state_init_list("filter_vars3","None", vars),
                 selected = state_init_list("filter_fill","None", vars),
                 multiple = FALSE)
})

observe({
	input$filter_vars1
	input$filter_vars2
  updateCheckboxInput(session, "filter_line",
                           label = "Line",
  												 value = state_init("filter_line", FALSE))
})


# observe({
# 	input$filter_vars1
# 	input$filter_vars2
#   updateCheckboxInput(session, "filter_line",
#                            label = "Line",
#   												 value = state_init("filter_line", FALSE))
# })

# observe({
# 	input$filter_vars1
# 	input$filter_vars2
#   updateCheckboxInput(session, "filter_smooth",
#                            label = "Smooth",
#   												 value = state_init("filter_smooth", FALSE))
# })

output$ui_Filter <- renderUI({
  list(
  	wellPanel(
    	uiOutput("ui_filter_vars1"),
      uiOutput("ui_filter_vars2"),
	  	uiOutput("ui_filter_fill"),
      checkboxInput('filter_tooltip', 'Tooltip',
                    value = state_init("filter_tooltip", FALSE))
	  	# ,conditionalPanel(condition = "input.filter_fill != null",
	   #  	checkboxInput('filter_line', 'Line',
	   #  	              value = state_init("filter_line", FALSE)),
	   #    checkboxInput('filter_smooth', 'Smooth',
	   #                  value = state_init("filter_smooth", FALSE))
	   #  )
    )
  )
})

# move to radiant.R
no_val <- function(x, val = "") is.null(x) || x == val
not_val <- function(x, val = "") is.null(x) || x != val

observe({

	if(input$datatabs %>% not_val('Filter')) return(NULL)
  if(input$filter_vars1 %>% no_val('None')) return(NULL)
	if(is.null(inChecker(input$filter_vars1))) return(NULL)

  xvar <- prop("x", as.symbol(input$filter_vars1))
	yvar <- input$filter_vars2
	fillvar <- input$filter_fill

	opa <- 1
  if(getdata_class()[input$filter_vars1] != "factor" &&
		yvar %>% not_val("None")) opa <- .3

  dat <- getdata()

  # if(is.null(dat$id)) dat$id <- seq_len(nrow(dat))
  # lb <- linked_brush(keys = dat$id, "red")
	# ggvis(x = xvar, y = yvar, key := ~id) %>%
	# layer_points(fill := lb$fill, fill.brush := "red", opacity := 0.3) %>%
  # lb$input() %>%

	ggvis_select <- function(dat, xvar,yvar,fillvar,opa = 1) {
		if(yvar %>% no_val("None")) {
 				return(ggvis(dat, x = xvar, opacity := opa))
 			} else {
  			yvar <- prop("y", as.symbol(yvar))
 				# if(not_val(fillvar,"None")) {
 				if(fillvar %>% no_val("None")) {
 					return(ggvis(dat, x = xvar, y = yvar, opacity := opa))
 				} else {
  				fillvar <- prop("fill", as.symbol(fillvar))
 					return(ggvis(dat, x = xvar, y = yvar, fill = fillvar, opacity := opa))
 				}
 			}
	}

	all_values <- function(x){
    if(is.null(x)) return(NULL)
    paste0(names(x), ": ", format(x), collapse = "<br />")
  }

  ggvis_addons <- function(plot,
                           line = input$filter_line,
                           smth = input$filter_smooth,
                           tt = input$filter_tooltip)
  {
  	# if(line) {
  	#   plot %<>% layer_points() %>%
  	# 		layer_model_predictions(model='lm', se=TRUE)
  	# }
  	# if(smth) {
  	# 	plot %<>% layer_smooths(span = 1, se = TRUE)
  	# 	if(!line) plot %<>% layer_points()
  	# }
  	if(tt)
  		plot %<>% add_tooltip(all_values, "hover")

  	plot
  }

 	dat %>%
		ggvis_select(xvar,yvar,fillvar,opa) %>%
		ggvis_addons() %>%
 	  bind_shiny("filter_scatter")

 	# selected <- lb$selected
	# filter_data$sel_rows <- selected()
  # A subset of only the selected points
  # selected <- lb$selected
  # dat_selected <- reactive({
  #   dat[selected(), ]
  # })
})

output$filter_dataviewer <- renderDataTable({
	# getdata()[filter_data$sel_rows,]
})


# observe({
#   if(input$filter_vars1 %>% no_val('None')) return(NULL)

#   xvar <- prop("x", as.symbol(input$filter_vars1))
# 	yvar <- ""
#   if(input$filter_vars2 %>% not_val('None'))
#   	yvar <- prop("y", as.symbol(input$filter_vars2))

#  	getdata %>%
#  		ggvis(x = xvar, y = yvar) %>%
# 	  bind_shiny("filter_scatter")
# })





	  # layer_points() %>%
	  # set_options(width = 300, height = 300) %>%

# output$grid_ggvis <- renderUI({

#   if(input$filter_vars1 %>% no_val('None') ||
#      input$filter_vars2 %>% no_val('None')) return(NULL)

#   ggvisOutput("filter_scatter")
# })


# output$grid_ggvis_linked <- renderUI({

# 	# very flaky - image disapper when changing variables

#   if(input$datasets != 'diamonds')
#   	return("<h3>Please select the Diamonds data</h3>" %>% HTML)
#   if(input$filter_vars1 %>% no_val('None') ||
#      input$filter_vars2 %>% no_val('None')) return(NULL)

#   xvar <- prop("x", as.symbol(input$filter_vars1))
#   yvar <- prop("y", as.symbol(input$filter_vars2))
#   # zvar <- prop("x", as.symbol(input$filter_vars3))
#   # zvar <- prop("x", as.symbol("price"))

#   dat <- getdata()
#   if(is.null(dat$id)) dat$id <- seq_len(nrow(dat))

#   lb <- linked_brush(keys = dat$id, "red")

#   dat %>%
#     ggvis(x = xvar, y = yvar, key := ~id) %>%
#     # ggvis(~carat, ~price, key := ~id) %>%
#     layer_points(fill := lb$fill, fill.brush := "red", opacity := 0.3) %>%
#     lb$input() %>%
#     set_options(width = 300, height = 300) %>%
#     bind_shiny("plot1") # Very important!


#   # A subset of only the selected points
#   selected <- lb$selected
#   dat_selected <- reactive({
#     dat[selected(), ]
#   })

#   # library(dplyr)
#   # library(ggplot2)
#   # library(magrittr)
#   # dat <- diamonds
#   # zvar <- "price"
#   bw <- select_(dat, .dots = "price") %>%
#   			{(max(.) - min(.)) / 20} %>%
#   			round(0)

#   dat %>%
#     ggvis(~price) %>%
#     layer_histograms(width = bw, boundary = 0) %>%
#     add_data(dat_selected) %>%
#     layer_histograms(width = bw, boundary = 0, fill := "#dd3333") %>%
#     set_options(width = 300, height = 300) %>%
#     bind_shiny("plot2")

#   p1 <- ggvisOutput("plot1")
#   p2 <- ggvisOutput("plot2")

#   # clunky but seems to work
#   html1 <- HTML("<table><td>")
#   html2 <- HTML("</td><td>")
#   html3 <- HTML("</td></table>")
#   list(html1,p1,html2,p2,html3)

# })

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
