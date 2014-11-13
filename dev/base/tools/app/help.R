#######################################
# Help menu
#######################################

help_data <- c("Manage" = "manage.md","View" = "view.md", "Visualize" = "visualize.md",
               "Explore" = "explore.md", "Merge" = "merge.md", "Transform" = "transform.md")
output$help_data <- reactive({

  if(length(input$help_data) == 0) return(HTML("<h3>Select a help topic for the Data menu</h3>"))
  all_help <- ""
  local_hd <- help_data[which(help_data %in% input$help_data)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../base/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})


help_random <- c("Central Limit Theorem" = "ctl.md","Sampling" = "random.md", "Sample size" = "sampleSize.md")
output$help_random <- reactive({

  if(length(input$help_random) == 0) return(HTML("<h3>Select a help topic for the Random menu</h3>"))
  all_help <- ""
  local_hd <- help_random[which(help_random %in% input$help_random)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../quant/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})

help_base_menu <- c("Single mean" = "singleMean.md", "Compare means" = "compareMeans.md", "Cross-tabs" = "crossTabs.md")
output$help_base_menu <- reactive({

  if(length(input$help_base_menu) == 0) return(HTML("<h3>Select a help topic for the Base menu</h3>"))
  all_help <- ""
  local_hd <- help_base_menu[which(help_base_menu %in% input$help_base_menu)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../quant/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})

output$help_quant <- renderUI({
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        checkboxGroupInput("help_data", "Data menu:", help_data,
   	      selected = state_init_list("help_data","", help_data))
      ),
      wellPanel(
        checkboxGroupInput("help_random", "Random menu:", help_random,
           selected = state_init_list("help_random","", help_random))
      ),
      wellPanel(
        checkboxGroupInput("help_base_menu", "Base menu:", help_base_menu,
           selected = state_init_list("help_base_menu","", help_base_menu))
      ),
      wellPanel(
        helpText("Help is available on each page by clicking the ? icon on the bottom left of your screen.")
      )
    ),
    mainPanel(
      htmlOutput("help_data"),
      htmlOutput("help_random"),
      htmlOutput("help_base_menu")
    )
  )
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
