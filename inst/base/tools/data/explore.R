# UI-elements for explore
output$uiExpl_columns <- renderUI({
  # isFct <- "factor" == getdata_class()
  # vars <- varnames()[!isFct]
  isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  if(length(vars) == 0) return()

  selectizeInput("expl_columns", label = "Select columns(s):", choices = as.list(vars),
    selected = state_multvar("expl_columns",vars), multiple = TRUE,
    options = list(placeholder = 'Select column(s)', plugins = list('remove_button', 'drag_drop'))
  )
})

output$uiExpl_byvar <- renderUI({
  isGroupable <- getdata_class() %in% c("factor","integer","character")
#   isFct <- "factor" == getdata_class()
#   vars <- varnames()[isFct]
  vars <- varnames()[isGroupable]
  if(length(vars) == 0) return()
  selectizeInput("expl_byvar", label = "Group by:", choices = vars,
    selected = state_multvar("expl_byvar",vars), multiple = TRUE,
    # options = list(maxItems = 2, placeholder = 'Select group-by variable', plugins = list('remove_button', 'drag_drop'))
    options = list(placeholder = 'Select group-by variable', plugins = list('remove_button', 'drag_drop'))
  )
})

# nmissing <<- function(x) sum(is.na(x))
# # adding Q1 and Q3, na.rm set to true in colwise function call below
# p25 <<- function(x, na.rm = TRUE) quantile(x,.25, na.rm = na.rm)
# p75 <<- function(x, na.rm = TRUE) quantile(x,.75, na.rm = na.rm)
# serr <<- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / length(na.omit(x))
# cv <<- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)

expl_functions <- list("N" = "length", "Mean" = "mean", "Median" = "median", "25%" = "p25", "75%" = "p75",
                        "Max" = "max", "Min" = "min", "Std. dev" = "sd", "Std. err" = "serr", "cv" = "cv", "Skew" = "skew",
                        "Kurtosis" = "kurtosi", "# missing" = "nmissing")

output$uiExpl_function <- renderUI({
  if(is.null(input$expl_byvar)) return()
  selectizeInput("expl_function", label = "Apply function(s):", choices = expl_functions,
                 selected = state_init_multvar("expl_function",c("length","mean"), expl_functions),
                 multiple = TRUE, options = list(placeholder = 'Select functions',
                                                 plugins = list('remove_button', 'drag_drop'))
    )
})

output$uiExpl_show_viz <- renderUI({
  if(is.null(input$expl_byvar)) return()
  checkboxInput('expl_show_viz', 'Show plot', value = state_init("expl_show_viz", FALSE))
})

output$ui_Explore <- renderUI({
  list(
    wellPanel(
      uiOutput("uiExpl_columns"),
      uiOutput("uiExpl_byvar"),
      uiOutput("uiExpl_function"),
      returnTextInput("expl_select", "Subset (e.g., price > 5000)", state_init("expl_select")),
      div(class="row-fluid",
          div(class="span6",checkboxInput('expl_show_tab', 'Show table',
                                          value = state_init("expl_show_tab", TRUE))),
          div(class="span6", uiOutput("uiExpl_show_viz"))
      )
    ),
    helpAndReport('Explore','explore',inclMD("../base/tools/help/explore.md"))
  )
})

observe({
  if(is.null(input$exploreReport) || input$exploreReport == 0) return()
  isolate({
    inp <- list(input$dataset, input$expl_columns, input$expl_byvar, input$expl_function, input$expl_select,
                input$expl_show_tab, input$expl_show_viz)
    updateReport(inp,"explore", round(7 * expl_plot_width()/650,2), round(7 * expl_plot_height()/650,2))
  })
})

.explore <- reactive({
  if(input$expl_columns %>% not_available) return()

  withProgress(message = 'Calculating', value = 0, {
    explore(input$dataset, input$expl_columns, input$expl_byvar, input$expl_function, input$expl_select, input$expl_show_tab, input$expl_show_viz)
  })
})

nmissing <- function(x) sum(is.na(x))
# adding Q1 and Q3, na.rm set to true in colwise function call below
p25 <- function(x, na.rm = TRUE) quantile(x,.25, na.rm = na.rm)
p75 <- function(x, na.rm = TRUE) quantile(x,.75, na.rm = na.rm)
serr <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / length(na.omit(x))
cv <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)

explore <- function(dataset, expl_columns, expl_byvar, expl_function, expl_select, expl_show_tab, expl_show_viz) {

  dat <- r_data[[dataset]]
  if(expl_select != '') {
    selcom <- gsub(" ", "", expl_select)
    seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
    if(!is(seldat, 'try-error')) {
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
      }
    }
  }

  if(is.null(expl_byvar)) {
    dat %<>% select_(.dots = expl_columns)
    isNum <- sapply(dat, is.numeric)
    if(sum(isNum) > 0) {

      # skew, kurtosis = kurtosi, missing = nmissing)) %>%
      select(dat, which(isNum)) %>%
        gather_("variable", "values", expl_columns) %>%
        group_by(variable) %>%
        summarise_each(funs(n = length, mean, median, min, max, `25%` = p25,
                            `75%` = p75, sd, se = serr, cv = sd/mean,
                            missing = nmissing)) %>%
        as.data.frame -> dat
        dat[,-1] %<>% round(3)
        colnames(dat)[1] <- ""
        dat
    }
  } else {

    dat %>%
      group_by_(.dots = expl_byvar) %>%
      select_(.dots = expl_columns) -> dat

    plyres <- list()
    for(func in expl_function) {
      gfunc <- get(func)
      plyres[[func]] <- dat %>% summarise_each(funs(gfunc)) %>% as.data.frame
    }

    plyres$expl_columns <- expl_columns
    plyres$expl_function <- expl_function
    plyres$expl_byvar <- expl_byvar
    plyres$expl_show_viz <- expl_show_viz

    plyres
  }
}

.summary_explore <- reactive({

  result <- .explore()
  if(is.null(result)) return(invisible())
  summary_explore(result)
})

summary_explore <- function(result = .explore()) {

  if(class(result)[1] != 'list') {
    cat("Summarize numeric variables:\n")
    result %>% print(row.names = FALSE)
  } else {
    for(func in result$expl_function) {
      cat("Results grouped by: ", result$expl_byvar, "\n")
      cat("Function used: ", names(which(expl_functions == func)), "\n")
      result[[func]][,result$expl_columns] %<>% round(3)
      result[[func]] %>% print
      cat("\n")
    }
  }
}

output$expl_summary <- renderPrint({

  if(!is.null(input$expl_show_tab) && !input$expl_show_tab) return(invisible())
  .summary_explore()
})

.plots_explore <- reactive({

  result <- .explore()
  if(is.null(result)) return()

  withProgress(message = 'Making plot', value = 0, {
    plots_explore(result)
  })
})

plots_explore <- function(result = .explore()) {

  if(is.null(result$expl_show_viz) || result$expl_show_viz == FALSE) return(invisible())

  by_var <- fill_var <- result$expl_byvar[1]
  if(length(result$expl_byvar) > 1) fill_var <- result$expl_byvar[2]

  plots <- list()
  for(func in result$expl_function) {
    for(var in result$expl_columns) {
      plots[[paste0(var,"_",func)]] <- ggplot(data = result[[func]], aes_string(x = by_var, y = var, fill = fill_var)) +
        geom_bar(stat="identity", position = "dodge", alpha=.7) +
        ggtitle(paste("Function used:", names(which(expl_functions == func))))
    }
  }

  do.call(grid.arrange, c(plots, list(ncol = 1)))
}

expl_plot_width <- function() {
  # return(input$expl_plot_width)
  650
}

expl_plot_height <- function() {
  # return(input$expl_plot_height)
  400 * length(input$expl_function) * length(input$expl_columns)
}

output$expl_plots <- renderPlot({

  # if(isolate(input$datatabs) != 'Explore') return(invisible())

  if(!input$expl_show_viz || is.null(input$expl_byvar)) return()
  .plots_explore()
}, width = expl_plot_width, height = expl_plot_height)

#######################################
### When Explore is moved to dplyr
#######################################

# require(devtools)
# install_github("assertthat")
# install_github("dplyr")

# require(assertthat)
# require(dplyr)

# filter(hflights, Month == 1, DayofMonth == 1, Dest == "DFW")
# head(select(hflights, Year:DayOfWeek))
# summarise(hflights, delay = mean(ArrDelay, na.rm = TRUE), n = length(ArrDelay))

# by_dest <- group_by(hflights, Dest)
# filter(by_dest, ArrDelay == max(ArrDelay))

# res <- summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))

# by_day <- group_by(hflights, Year, Month, DayofMonth)
# by_month <- summarise(by_day, delayed = sum(ArrDelay > 0, na.rm = TRUE))
# by_month
# summarise(summarise(by_month, delayed = sum(delayed)), delayed = sum(delayed))
# summarise(by_month, delayed = sum(delayed))

# by_dest <- group_by(hflights, Dest)
# filter(by_dest, ArrDelay == max(ArrDelay))
# summarise(group_by(hflights, Dest), arr = mean(ArrDelay, na.rm = TRUE))
