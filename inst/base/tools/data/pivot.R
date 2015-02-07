output$ui_pivot <- renderUI({
  # nothing here at the moment
})

output$pivotData <- renderRpivotTable({
  rpivotTable(data = r_data[[input$dataset]])
})

# test of ui-server stucture
output$pivotDataTable <- renderDataTable({
  r_data[[input$dataset]]
})
