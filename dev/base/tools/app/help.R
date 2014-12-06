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

help_random <- c("Central Limit Theorem" = "ctl.md","Sampling" = "random.md", "Sample size" = "sampleSize.Rmd")
output$help_random <- reactive({

  if(length(input$help_random) == 0) return()
  all_help <- ""
  local_hd <- help_random[which(help_random %in% input$help_random)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclRmd(paste0("../quant/tools/help/",local_hd[i])),
                      '<script>', 'MathJax.Hub.Typeset();', '</script>', sep="\n")
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

help_regression <- c("Correlation" = "correlation.md", "Regression" = "regression.Rmd")
output$help_regression <- reactive({

  if(length(input$help_regression) == 0) return()
  all_help <- ""
  local_hd <- help_regression[which(help_regression %in% input$help_regression)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclRmd(paste0("../quant/tools/help/",local_hd[i])),
                     '<script>', 'MathJax.Hub.Typeset();', '</script>', sep="\n")
  }
  all_help %>% HTML
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


output$help_base <- renderUI({
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


help_quant_ui <- tagList(
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
  )
)

help_quant_main <- tagList(
  HTML("<h3>Select one or more help documents</h3>"),
  htmlOutput("help_data"),
  htmlOutput("help_random"),
  htmlOutput("help_base_menu"),
  htmlOutput("help_regression")
)


output$help_quant <- renderUI({
  sidebarLayout(
    sidebarPanel(
      help_quant_ui,
      wellPanel(
        helpText("Help is available on each page by clicking the ? icon on the bottom left of your screen.")
      )
    ),
    mainPanel(
      help_quant_main
    )
  )
})


help_maps <- c("(Dis)similarity" = "mds.md", "Attributes" = "percepmap.md")
output$help_maps <- reactive({

  if(length(input$help_maps) == 0) return()
  all_help <- ""
  local_hd <- help_maps[which(help_maps %in% input$help_maps)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../marketing/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})

observe({
  if(is.null(input$help_maps_all) || input$help_maps_all == 0) return()
  updateCheckboxGroupInput(session, "help_maps",
                           label = "",
                           choices = help_maps,
                           selected = state_init_list("help_maps",help_maps, help_maps))
})

observe({
  if(is.null(input$help_maps_none) || input$help_maps_none == 0) return()
  updateCheckboxGroupInput(session, "help_maps",
                           label = "",
                           choices = help_maps,
                           selected = state_init_list("help_maps","", help_maps))
})

############ Factor ############

help_factor <- c("Pre-factor" = "preFactor.md", "Factor" = "fullFactor.md")
output$help_factor <- reactive({

  if(length(input$help_factor) == 0) return()
  all_help <- ""
  local_hd <- help_factor[which(help_factor %in% input$help_factor)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../marketing/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})

observe({
  if(is.null(input$help_factor_all) || input$help_factor_all == 0) return()
  updateCheckboxGroupInput(session, "help_factor",
                           label = "",
                           choices = help_factor,
                           selected = state_init_list("help_factor",help_factor, help_factor))
})

observe({
  if(is.null(input$help_factor_none) || input$help_factor_none == 0) return()
  updateCheckboxGroupInput(session, "help_factor",
                           label = "",
                           choices = help_factor,
                           selected = state_init_list("help_factor","", help_factor))
})

############ Cluster ############

help_cluster <- c("Hierarchical" = "hcclustering.md", "Kmeans" = "kmeansClustering.md")
output$help_cluster <- reactive({

  if(length(input$help_cluster) == 0) return()
  all_help <- ""
  local_hd <- help_cluster[which(help_cluster %in% input$help_cluster)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../marketing/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})


observe({
  if(is.null(input$help_cluster_all) || input$help_cluster_all == 0) return()
  updateCheckboxGroupInput(session, "help_cluster",
                           label = "",
                           choices = help_cluster,
                           selected = state_init_list("help_cluster",help_cluster, help_cluster))
})

observe({
  if(is.null(input$help_cluster_none) || input$help_cluster_none == 0) return()
  updateCheckboxGroupInput(session, "help_cluster",
                           label = "",
                           choices = help_cluster,
                           selected = state_init_list("help_cluster","", help_cluster))
})

help_marketing_ui <- tagList(
  wellPanel(
    HTML("<label>Maps menu: <a id='help_maps_all' title='Check all' href='#' class='action-button icon-ok'></a>
    <a id='help_maps_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
    checkboxGroupInput("help_maps", "", help_maps,
      selected = state_init_list("help_maps","", help_maps))
  ),
  wellPanel(
    HTML("<label>Factor menu: <a id='help_factor_all' title='Check all' href='#' class='action-button icon-ok'></a>
    <a id='help_factor_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
    checkboxGroupInput("help_factor", "", help_factor,
      selected = state_init_list("help_factor","", help_factor))
  ),
  wellPanel(
    HTML("<label>Cluster menu: <a id='help_cluster_all' title='Check all' href='#' class='action-button icon-ok'></a>
    <a id='help_cluster_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
    checkboxGroupInput("help_cluster", "", help_cluster,
      selected = state_init_list("help_cluster","", help_cluster))
  )
)

output$help_marketing <- renderUI({
  sidebarLayout(
    sidebarPanel(
      help_quant_ui,
      help_marketing_ui,
      wellPanel(
        helpText("Help is available on each page by clicking the ? icon on the bottom left of your screen.")
      )
    ),
    mainPanel(
      help_quant_main,
      htmlOutput("help_maps"),
      htmlOutput("help_factor"),
      htmlOutput("help_cluster")
    )
  )
})


# output$help_quant <- renderUI({
#   sidebarLayout(
#     sidebarPanel(
#       wellPanel(
#         HTML("<label>Data menu: <a id='help_data_all' title='Check all' href='#' class='action-button icon-ok'></a>
#         <a id='help_data_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
#         checkboxGroupInput("help_data", "", help_data,
#           selected = state_init_list("help_data","", help_data))
#       ),
#       wellPanel(
#         HTML("<label>Random menu: <a id='help_random_all' title='Check all' href='#' class='action-button icon-ok'></a>
#         <a id='help_random_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
#         checkboxGroupInput("help_random", "", help_random,
#            selected = state_init_list("help_random","", help_random))
#       ),
#       wellPanel(
#         HTML("<label>Base menu: <a id='help_base_all' title='Check all' href='#' class='action-button icon-ok'></a>
#         <a id='help_base_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
#         checkboxGroupInput("help_base_menu", "", help_base_menu,
#            selected = state_init_list("help_base_menu","", help_base_menu))
#       ),
#       wellPanel(
#         HTML("<label>Regression menu: <a id='help_regression_all' title='Check all' href='#' class='action-button icon-ok'></a>
#         <a id='help_regression_none' title='Uncheck all' href='#' class='action-button icon-remove'></a></label>"),
#         checkboxGroupInput("help_regression", "", help_regression,
#            selected = state_init_list("help_regression","", help_regression))
#       ),
#       wellPanel(
#         helpText("Help is available on each page by clicking the ? icon on the bottom left of your screen.")
#       )
#     ),
#     mainPanel(
#       HTML("<h3>Select one or more help documents</h3>"),
#       htmlOutput("help_data"),
#       htmlOutput("help_random"),
#       htmlOutput("help_base_menu"),
#       htmlOutput("help_regression")
#     )
#   )
# })
