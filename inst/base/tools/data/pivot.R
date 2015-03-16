# add to DESCRIPTION FILE after pryr
# htmlwidgets,
# rpivotTable (>= 0.1.2.6),

# output$ui_Pivot <- renderUI({
#   help_modal('Pivot','pivotHelp',inclMD("../base/tools/help/pivot.md"))
# })

# output$pivotData <- rpivotTable::renderRpivotTable({
#   rpivotTable::rpivotTable(data = .getdata())
# })


# add back to R/aaa.R
# #' radiant
# #'
# #' @name radiant
# #' @docType package
# #' @import ggplot2 shiny dplyr shinyAce htmlwidgets
# #' @importFrom knitr knit2html
# #' @importFrom pryr where
# #' @importFrom magrittr %<>% %T>% set_rownames set_colnames divide_by add
# #' @importFrom lubridate is.Date
# #' @importFrom broom tidy glance
# #' @importFrom tidyr gather_
# #' @importFrom AlgDesign optFederov
# #' @importFrom gridExtra grid.arrange
# #' @importFrom markdown markdownToHTML
# #' @importFrom wordcloud textplot
# #' @importFrom rpivotTable rpivotTable rpivotTableOutput renderRpivotTable
# NULL

# For global.R
# pkgs_cran <- c("car", "gridExtra", "GPArotation", "psych", "wordcloud", "AlgDesign",
#                "knitr", "lubridate", "ggplot2", "ggdendro", "shiny", "pryr", "shiny", "magrittr", "tidyr",
#                "dplyr", "broom", "htmlwidgets")
# pkgs_gh <- c("shinyAce","rpivotTable")
# pkgs <- c(pkgs_cran, pkgs_gh)
