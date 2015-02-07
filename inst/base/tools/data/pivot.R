output$ui_pivot <- renderUI({
  # nothing here at the moment
})

output$pivotData <- renderRpivotTable({
  rpivotTable(data = r_data[[input$dataset]])
})

output$pivotDataTable <- renderDataTable({
  r_data[[input$dataset]]
})
