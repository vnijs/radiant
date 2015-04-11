output$ui_Pivot <- renderUI({
  help_modal('Pivot','pivotHelp',inclMD(file.path(r_path,"/base/tools/help/pivot.md")))
})

output$pivotData <- rpivotTable::renderRpivotTable({
  rpivotTable::rpivotTable(data = .getdata())
})

