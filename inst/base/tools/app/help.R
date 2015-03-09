#######################################
# Other elements in help menu
#######################################
output$help_videos <- renderUI({
  "../base/tools/app/tutorials.md" %>% inclMD %>% HTML
})

output$help_about <- renderUI({
  "../base/tools/app/about.md" %>% inclMD %>% HTML
})

#######################################
# Main function of help menu
#######################################
# help2html <- function(x) x %>% gsub("\\\\%","%",.) %>% HTML

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
  cc <- "&copy; Vincent Nijs (2015) <a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/' target='_blank'><img alt='Creative Commons License' style='border-width:0' src ='imgs/80x15.png' /></a></br>"
  paste(all_help,"\n",mathjax_script,"\n",cc) %>% HTML
}

help_data <- c("Manage" = "manage.md","View" = "view.md", "Visualize" = "visualize.md",
               "Explore" = "explore.md", "Merge" = "merge.md", "Transform" = "transform.md")
output$help_data <- reactive(append_help("help_data", "../base/tools/help/"))

help_sample <- c("Sampling" = "sampling.md", "Sample size" = "sample_size.Rmd")
output$help_sample <- reactive(append_help("help_sample", "../quant/tools/help/", Rmd = TRUE))

help_base_menu <- c("Single mean" = "single_mean.md", "Compare means" = "compare_means.md",
                    "Single proportion" = "single_prop.md", "Compare proportions" = "compare_props.md",
                    "Cross-tabs" = "cross_tabs.md")
output$help_base_menu <- reactive(append_help("help_base_menu", "../quant/tools/help/"))

help_regression <- c("Correlation" = "correlation.md", "Regression" = "regression.Rmd", "GLM" = "glm_reg.Rmd")
output$help_regression <- reactive(append_help("help_regression", "../quant/tools/help/", Rmd = TRUE))


help_switch <- function(help_all, help_str, help_on = TRUE) {
  if(is.null(help_all) || help_all == 0) return()
  help_choices <- help_init <- get(help_str)
  init <- ""
  # if(help_on) init <- state_init(help_str, help_init)
  if(help_on) init <- help_init
  updateCheckboxGroupInput(session, help_str,
    label = NULL,
    choices = help_choices,
    selected = init, inline = TRUE)
}

observe( help_switch(input$help_data_all, "help_data") )
observe( help_switch(input$help_data_none, "help_data", help_on = FALSE) )

observe( help_switch(input$help_sample_all, "help_sample") )
observe( help_switch(input$help_sample_none, "help_sample", help_on = FALSE) )

observe( help_switch(input$help_base_all, "help_base_menu") )
observe( help_switch(input$help_base_none, "help_base_menu", help_on = FALSE) )

observe( help_switch(input$help_regression_all, "help_regression") )
observe( help_switch(input$help_regression_none, "help_regression", help_on = FALSE) )

output$help_base <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        checkboxGroupInput("help_data", "Data menu:", help_data,
          selected = state_init("help_data"), inline = TRUE)
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
    HTML("<label>Data menu: <i id='help_data_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_data_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_data", NULL, help_data,
      selected = state_init("help_data"), inline = TRUE)
  ),
  wellPanel(
    HTML("<label>Sample menu: <i id='help_sample_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_sample_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_sample", NULL, help_sample,
       selected = state_init("help_sample"), inline = TRUE)
  ),
  wellPanel(
    HTML("<label>Base menu: <i id='help_base_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_base_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_base_menu", NULL, help_base_menu,
       selected = state_init("help_base_menu"), inline = TRUE)
  ),
  wellPanel(
    HTML("<label>Regression menu: <i id='help_regression_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_regression_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_regression", NULL, help_regression,
       selected = state_init("help_regression"), inline = TRUE)
  )
)

help_quant_main <- tagList(
  HTML("<h3>Select one or more help documents</h3>"),
  # HTML("<script type='text/javascript' src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>"),
  htmlOutput("help_data"),
  htmlOutput("help_sample"),
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


help_maps <- c("(Dis)similarity" = "mds.md", "Attributes" = "pmap.md")
output$help_maps <- reactive(append_help("help_maps", "../marketing/tools/help/"))
observe( help_switch(input$help_maps_all, "help_maps") )
observe( help_switch(input$help_maps_none, "help_maps", help_on = FALSE) )

help_factor <- c("Pre-factor" = "pre_factor.md", "Factor" = "full_factor.md")
output$help_factor <- reactive(append_help("help_factor", "../marketing/tools/help/"))
observe( help_switch(input$help_factor_all, "help_factor") )
observe( help_switch(input$help_factor_none, "help_factor", help_on = FALSE) )

help_cluster <- c("Hierarchical" = "hier_clus.md", "Kmeans" = "kmeans_clus.md")
output$help_cluster <- reactive(append_help("help_cluster", "../marketing/tools/help/"))
observe( help_switch(input$help_cluster_all, "help_cluster") )
observe( help_switch(input$help_cluster_none, "help_cluster", help_on = FALSE) )

help_conjoint <- c("Conjoint" = "conjoint.md", "Conjoint profiles" = "conjoint_profiles.md")
output$help_conjoint <- reactive(append_help("help_conjoint", "../marketing/tools/help/"))
observe( help_switch(input$help_conjoint_all, "help_conjoint") )
observe( help_switch(input$help_conjoint_none, "help_conjoint", help_on = FALSE) )

help_marketing_ui <- tagList(
  wellPanel(
    HTML("<label>Maps menu: <i id='help_maps_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_maps_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_maps", NULL, help_maps,
      selected = state_init("help_maps"), inline = TRUE)
  ),
  wellPanel(
    HTML("<label>Factor menu: <i id='help_factor_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_factor_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_factor", NULL, help_factor,
      selected = state_init("help_factor"), inline = TRUE)
  ),
  wellPanel(
    HTML("<label>Cluster menu: <i id='help_cluster_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_cluster_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_cluster", NULL, help_cluster,
      selected = state_init("help_cluster"), inline = TRUE)
  ),
  wellPanel(
    HTML("<label>Conjoint menu: <i id='help_conjoint_all' title='Check all' href='#' class='action-button glyphicon glyphicon-ok'></i>
    <i id='help_conjoint_none' title='Uncheck all' href='#' class='action-button glyphicon glyphicon-remove'></i></label>"),
    checkboxGroupInput("help_conjoint", NULL, help_conjoint,
      selected = state_init("help_conjoint"), inline = TRUE)
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
      htmlOutput("help_cluster"),
      htmlOutput("help_conjoint")
    )
  )
})
