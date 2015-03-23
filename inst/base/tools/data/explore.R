# UI-elements for explore
output$uiExpl_columns <- renderUI({
  isNum <- "numeric" == getdata_class() | "integer" == getdata_class()
  vars <- varnames()[isNum]
  selectInput("expl_columns", label = "Select columns(s):", choices = vars,
    selected = state_multiple("expl_columns",vars), multiple = TRUE,
    size = min(8, length(vars)), selectize = FALSE)
})

output$uiExpl_byvar <- renderUI({
  vars <- groupable_vars()
  selectizeInput("expl_byvar", label = "Group by:", choices = vars,
    selected = state_multiple("expl_byvar",vars), multiple = TRUE,
    options = list(placeholder = 'Select group-by variable',
                   plugins = list('remove_button', 'drag_drop'))
  )
})

expl_functions <- list("n" = "length", "mean" = "r_mean", "median" = "r_median",
                       "min" = "r_min", "max" = "r_max", "25%" = "p25",
                       "75%" = "p75", "sd" = "r_sd", "se" = "serr",
                       "cv" = "cv", "skew" = "skew", "kurtosis" = "kurtosi",
                       "# missing" = "nmissing")

output$uiExpl_function <- renderUI({
  if(is.null(input$expl_byvar)) return()
  selectizeInput("expl_function", label = "Apply function(s):",
                 choices = expl_functions,
                 selected = state_multiple("expl_function",
                                           expl_functions, c("length","mean")),
                 multiple = TRUE,
                 options = list(placeholder = 'Select functions',
                                plugins = list('remove_button', 'drag_drop'))
    )
})

output$uiExpl_show_viz <- renderUI({
  if(is.null(input$expl_byvar)) return()
  checkboxInput('expl_show_viz', 'Show plot',
                value = state_init("expl_show_viz", FALSE))
})

output$ui_Explore <- renderUI({
  list(
    wellPanel(
      uiOutput("uiExpl_columns"),
      uiOutput("uiExpl_byvar"),
      uiOutput("uiExpl_function"),
      div(class="row",
        div(class="col-xs-6", checkboxInput('expl_show_tab', 'Show table',
            value = state_init("expl_show_tab", TRUE))),
        div(class="col-xs-6", uiOutput("uiExpl_show_viz"))
      )
    ),
    help_modal('Explore','exploreHelp',inclMD("../base/tools/help/explore.md"))
  )
})

.explore <- reactive({
  if(input$expl_columns %>% not_available) return()

  withProgress(message = 'Calculating', value = 0, {
    explore(input$dataset, input$expl_columns, input$expl_byvar,
            input$expl_function, input$expl_show_tab, input$expl_show_viz)
  })
})

nmissing <- function(x) sum(is.na(x))
# adding Q1 and Q3, na.rm set to true in colwise function call below
p25 <- function(x, na.rm = TRUE) quantile(x,.25, na.rm = na.rm)
p75 <- function(x, na.rm = TRUE) quantile(x,.75, na.rm = na.rm)
serr <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / length(na.omit(x))
cv <- function(x, na.rm = TRUE) sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
r_mean <- function(x) mean(x, na.rm = TRUE)
r_median <- function(x) median(x, na.rm = TRUE)
r_min <- function(x) min(x, na.rm = TRUE)
r_max <- function(x) max(x, na.rm = TRUE)
r_sd <- function(x) sd(x, na.rm = TRUE)

explore <- function(dataset, expl_columns,
                    expl_byvar = c(),
                    expl_function = c(),
                    expl_show_tab = TRUE,
                    expl_show_viz = FALSE) {

  # move to getdata when exported
  dat <- .getdata()

  if(is.null(expl_byvar)) {
    dat %<>% select_(.dots = expl_columns)
    isNum <- sapply(dat, is.numeric)
    if(sum(isNum) > 0) {
      select(dat, which(isNum)) %>%
      tidyr::gather_("variable", "values", expl_columns) %>%
      group_by(variable) %>%
      summarise_each(funs(n = length, missing = nmissing, mean(.,na.rm=TRUE),
                     median(.,na.rm=TRUE), min(.,na.rm=TRUE), max(.,na.rm=TRUE),
                     `25%` = p25, `75%` = p75, sd(.,na.rm=TRUE), se = serr,
                     cv = sd/mean)) %>%
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
  if(!is.null(input$expl_show_tab) && !input$expl_show_tab)
    return(invisible())
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

  if(is.null(result$expl_show_viz) || result$expl_show_viz == FALSE)
    return(invisible())

  by_var <- fill_var <- result$expl_byvar[1]
  if(length(result$expl_byvar) > 1) fill_var <- result$expl_byvar[2]

  plots <- list()
  for(func in result$expl_function) {
    for(var in result$expl_columns) {
      plots[[paste0(var,"_",func)]] <-
        ggplot(data = result[[func]], aes_string(x = by_var, y = var, fill = fill_var)) +
          geom_bar(stat="identity", position = "dodge", alpha=.7) +
          ggtitle(paste("Function used:", names(which(expl_functions == func))))
    }
  }

  do.call(gridExtra::grid.arrange, c(plots, list(ncol = 1)))
}

expl_plot_width <- function() 650
expl_plot_height <- function()
  400 * length(input$expl_function) * length(input$expl_columns)

output$expl_plots <- renderPlot({

  # if(isolate(input$datatabs) != 'Explore') return(invisible())

  if(!input$expl_show_viz || is.null(input$expl_byvar)) return()
  .plots_explore()
}, width = expl_plot_width, height = expl_plot_height)
