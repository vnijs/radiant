output$ui_Pivot <- renderUI({
  helpModal('Pivot','pivotHelp',inclMD("../base/tools/help/pivot.md"))
})

output$pivotData <- rpivotTable::renderRpivotTable({
  rpivotTable::rpivotTable(data = .getdata())
})
