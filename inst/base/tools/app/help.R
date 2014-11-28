#######################################
# Help menu
#######################################

help_data <- c("Manage" = "manage.md","View" = "view.md", "Visualize" = "visualize.md",
               "Explore" = "explore.md", "Merge" = "merge.md", "Transform" = "transform.md")
output$help_data <- reactive({

  if(length(input$help_data) == 0) return()
  all_help <- ""
  local_hd <- help_data[which(help_data %in% input$help_data)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../base/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})


help_random <- c("Central Limit Theorem" = "ctl.md","Sampling" = "random.md", "Sample size" = "sampleSize.md")
output$help_random <- reactive({

  if(length(input$help_random) == 0) return()
  all_help <- ""
  local_hd <- help_random[which(help_random %in% input$help_random)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../quant/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})

help_base_menu <- c("Single mean" = "singleMean.md", "Compare means" = "compareMeans.md", "Cross-tabs" = "crossTabs.md")
output$help_base_menu <- reactive({

  if(length(input$help_base_menu) == 0) return()
  all_help <- ""
  local_hd <- help_base_menu[which(help_base_menu %in% input$help_base_menu)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../quant/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})

help_regression <- c("Correlation" = "correlation.md", "Regression" = "regression.md")
output$help_regression <- reactive({

  if(length(input$help_regression) == 0) return()
  all_help <- ""
  local_hd <- help_regression[which(help_regression %in% input$help_regression)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../quant/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})

output$help_quant <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        HTML("<label>Data menu: <a id='help_data_all' title='Check all' href='#' class='action-button icon-ok'></a>
        <a id='help_data_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
        checkboxGroupInput("help_data", "", help_data,
   	      selected = state_init_list("help_data","", help_data))
      ),
      wellPanel(
        HTML("<label>Random menu: <a id='help_random_all' title='Check all' href='#' class='action-button icon-ok'></a>
        <a id='help_random_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
        checkboxGroupInput("help_random", "", help_random,
           selected = state_init_list("help_random","", help_random))
      ),
      wellPanel(
        HTML("<label>Base menu: <a id='help_base_all' title='Check all' href='#' class='action-button icon-ok'></a>
        <a id='help_base_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
        checkboxGroupInput("help_base_menu", "", help_base_menu,
           selected = state_init_list("help_base_menu","", help_base_menu))
      ),
      wellPanel(
        HTML("<label>Regression menu: <a id='help_regression_all' title='Check all' href='#' class='action-button icon-ok'></a>
        <a id='help_regression_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
        checkboxGroupInput("help_regression", "", help_regression,
           selected = state_init_list("help_regression","", help_regression))
      ),
      wellPanel(
        helpText("Help is available on each page by clicking the ? icon on the bottom left of your screen.")
      )
    ),
    mainPanel(
      HTML("<h3>Select one or more help documents</h3>"),
      htmlOutput("help_data"),
      htmlOutput("help_random"),
      htmlOutput("help_base_menu"),
      htmlOutput("help_regression")
    )
  )
})


observe({
  if(is.null(input$help_data_all) || input$help_data_all == 0) return()
  updateCheckboxGroupInput(session, "help_data",
                           label = "",
                           choices = help_data,
                           selected = state_init_list("help_data",help_data, help_data))
})

observe({
  if(is.null(input$help_data_none) || input$help_data_none == 0) return()
  updateCheckboxGroupInput(session, "help_data",
                           label = "",
                           choices = help_data,
                           selected = state_init_list("help_data","", help_data))
})

observe({
  if(is.null(input$help_random_all) || input$help_random_all == 0) return()
  updateCheckboxGroupInput(session, "help_random",
                           label = "",
                           choices = help_random,
                           selected = state_init_list("help_random",help_random, help_random))
})

observe({
  if(is.null(input$help_random_none) || input$help_random_none == 0) return()
  updateCheckboxGroupInput(session, "help_random",
                           label = "",
                           choices = help_random,
                           selected = state_init_list("help_random","", help_random))
})

observe({
  if(is.null(input$help_base_all) || input$help_base_all == 0) return()
  updateCheckboxGroupInput(session, "help_base_menu",
                           label = "",
                           choices = help_base_menu,
                           selected = state_init_list("help_base_menu",help_base_menu, help_base_menu))
})

observe({
  if(is.null(input$help_base_none) || input$help_base_none == 0) return()
  updateCheckboxGroupInput(session, "help_base_menu",
                           label = "",
                           choices = help_base_menu,
                           selected = state_init_list("help_base_menu","", help_base_menu))
})

observe({
  if(is.null(input$help_regression_all) || input$help_regression_all == 0) return()
  updateCheckboxGroupInput(session, "help_regression",
                           label = "",
                           choices = help_regression,
                           selected = state_init_list("help_regression",help_regression, help_regression))
})

observe({
  if(is.null(input$help_regression_none) || input$help_regression_none == 0) return()
  updateCheckboxGroupInput(session, "help_regression",
                           label = "",
                           choices = help_regression,
                           selected = state_init_list("help_regression","", help_regression))
})




output$help_ <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        checkboxGroupInput("help_data", "Data menu:", help_data,
           selected = state_init_list("help_data","", help_data))
      ),
      wellPanel(
        helpText("Help is available on each page by clicking the ? icon on the bottom left of your screen.")
      )
    ),
    mainPanel(
      htmlOutput("help_data")
    )
  )
})
