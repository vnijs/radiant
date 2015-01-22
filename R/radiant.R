#' Launch the Radiant app in a browser
#'
#' @details See \url{http://mostly-harmless.github.io/radiant} for documentation and tutorials
#'
#' @export
radiant <- function(app = c("marketing","quant")) {

  if(packageVersion('shiny') > "0.10.2.2")
    warning("Radiant currently does not support Shiny 0.11. To use Radiant please remove Shiny 0.11.\nCopy-and-paste the code below to install the required packages.\n\n\ndetach('package:shiny', unload=TRUE)\nremove.packages('shiny')\noptions(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/'))\ninstall.packages(new.packages(), dependencies = TRUE)")

  addResourcePath("figures", system.file("base/tools/help/figures/", package="radiant"))
  addResourcePath("imgs", system.file("base/www/imgs/", package="radiant"))
  if(app[1] == "marketing")
    addResourcePath("figures_marketing", system.file("marketing/tools/help/figures/", package="radiant"))
  if(app[1] == "quant")
    addResourcePath("figures_quant", system.file("quant/tools/help/figures/", package="radiant"))

  runApp(system.file("marketing", package='radiant'))
}

# @import car gridExtra GPArotation psych vegan RColorBrewer wordcloud AlgDesign brew reshape2 plyr markdown knitr rmarkdown testthat lubridate ggplot2 shiny magrittr tidyr dplyr ggvis broom shinyAce
# @importFrom shiny addResourcePath runApp
