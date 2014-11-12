#######################################
# Help menu
#######################################

# setwd("dev/marketing")
help_data <- c("Manage" = "manage.md","View" = "view.md", "Visualize" = "visualize.md",
               "Explore" = "explore.md", "Merge" = "merge.md", "Transform" = "transform.md")

help_random <- c("Central Limit Theorem" = "ctl.md","Sampling" = "random.md", "Sample size" = "sampleSize.md")

output$help_data <- reactive({

  if(length(help_data) == 0) return("Select a help topic")
  all_help <- ""
  local_hd <- help_data[which(help_data %in% input$help_data)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../base/tools/help/",local_hd[i])), sep="\n")
  }
  all_help %>% HTML
})

output$help_random <- reactive({

  if(length(help_random) == 0) return("Select a help topic")
  all_help <- ""
  local_hd <- help_random[which(help_random %in% input$help_random)]
  for(i in names(local_hd)) {
    all_help <- paste(all_help, paste0("<h2>",i,"</h2>"),inclMD(paste0("../quant/tools/help/",local_hd[i])), sep="\n")
  }
#    print(length(input$help_data))
#    print(input$help_data)
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
        helpText("Help is available on each page by clicking the ? icon on the bottom left of your screen.")
      )
    ),
    mainPanel(
      htmlOutput("help_data"),
      htmlOutput("help_random")
    )
  )
})
