output$ui_Pivot <- renderUI({
  helpModal('Pivot','pivotHelp',inclMD("../base/tools/help/pivot.md"))
})

output$pivotData <- renderRpivotTable({
  rpivotTable(data = .getdata())
})
