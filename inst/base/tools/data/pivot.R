output$ui_Pivot <- renderUI({
  help_modal('Pivot','pivotHelp',inclMD(paste0(path,"/base/tools/help/pivot.md")))
})

output$pivotData <- rpivotTable::renderRpivotTable({
  rpivotTable::rpivotTable(data = .getdata())
})

