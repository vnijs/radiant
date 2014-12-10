#######################################
# Help menu
#######################################

help2html <- function(x) x %>% gsub("\\\\%","%",.) %>% HTML

append_help <- function(help_str, help_path, Rmd = FALSE) {
  if(length(input[[help_str]]) == 0) return()
  help_block <- get(help_str)
  local_hd <- help_block[which(help_block %in% input[[help_str]])]
  all_help <- c()
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),
                      inclMD(paste0(help_path,local_hd[i])),
                      sep="\n")
  }
  mathjax_script <- ifelse(Rmd, "<script>MathJax.Hub.Typeset();</script>", "")
  paste(all_help,"\n",mathjax_script) %>% help2html
}

help_data <- c("Manage" = "manage.md","View" = "view.md", "Visualize" = "visualize.md",
               "Explore" = "explore.md", "Merge" = "merge.md", "Transform" = "transform.md")
output$help_data <- reactive(append_help("help_data", "../base/tools/help/"))

help_random <- c("Central Limit Theorem" = "ctl.md","Sampling" = "random.md",
                 "Sample size" = "sampleSize.Rmd")
output$help_random <- reactive(append_help("help_random", "../quant/tools/help/", Rmd = TRUE))

help_base_menu <- c("Single mean" = "singleMean.md", "Compare means" = "compareMeans.md",
                    "Cross-tabs" = "crossTabs.md")
output$help_base_menu <- reactive(append_help("help_base_menu", "../quant/tools/help/"))

help_regression <- c("Correlation" = "correlation.md", "Regression" = "regression.Rmd")
output$help_regression <- reactive(append_help("help_regression", "../quant/tools/help/", Rmd = TRUE))


help_switch <- function(help_all, help_str, help_on = TRUE) {
  if(is.null(help_all) || help_all == 0) return()
  help_choices <- help_init <- get(help_str)
  if(!help_on) help_init <- ""
  updateCheckboxGroupInput(session, help_str,
                           label = "",
                           choices = help_choices,
                           selected = state_init_list(help_str, help_init, help_choices))
}

observe( help_switch(input$help_data_all, "help_data") )
observe( help_switch(input$help_data_none, "help_data", help_on = FALSE) )

observe( help_switch(input$help_random_all, "help_random") )
observe( help_switch(input$help_random_none, "help_random", help_on = FALSE) )

observe( help_switch(input$help_base_all, "help_base_menu") )
observe( help_switch(input$help_base_none, "help_base_menu", help_on = FALSE) )

observe( help_switch(input$help_regression_all, "help_regression") )
observe( help_switch(input$help_regression_none, "help_regression", help_on = FALSE) )

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
output$help_maps <- reactive(append_help("help_maps", "../marketing/tools/help/"))
observe( help_switch(input$help_maps_all, "help_maps") )
observe( help_switch(input$help_maps_none, "help_maps", help_on = FALSE) )

help_factor <- c("Pre-factor" = "preFactor.md", "Factor" = "fullFactor.md")
output$help_factor <- reactive(append_help("help_factor", "../marketing/tools/help/"))
observe( help_switch(input$help_factor_all, "help_factor") )
observe( help_switch(input$help_factor_none, "help_factor", help_on = FALSE) )

help_cluster <- c("Hierarchical" = "hcclustering.md", "Kmeans" = "kmeansClustering.md")
output$help_cluster <- reactive(append_help("help_cluster", "../marketing/tools/help/"))
observe( help_switch(input$help_cluster_all, "help_cluster") )
observe( help_switch(input$help_cluster_none, "help_cluster", help_on = FALSE) )


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
