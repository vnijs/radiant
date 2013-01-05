# Data Analysis Menu in Shiny

Don't forget shift+CMD+P

This is a data-analysis menu for R using [shiny](http://www.rstudio.com/shiny/). See [thread](https://groups.google.com/forum/#!topic/shiny-discuss/ym1FmNsY-2w) for announcement/discussion.

## Todo:
- Finish cluster analysis
- Additional tools (e.g., factor, conjoint)
- Help files
- Options for compare means (e.g., one-sided) and regression (e.g., F-test for competing models)
- Add time-series plots with 1 or 2 y-axes
- Develop install structure so users can pick-and-choose menu-components. For an amazing implementation see [Boostrap](http://twitter.github.com/bootstrap/customize.html) which is *well* beyond what I have in mind.
- etc. etc.
- Host app on the Glimmer server
		
## To use the damshiny package locally:
Assuming you copied (or cloned) the repository to your desktop

`install.packages('shiny'); library(shiny)
shiny::runApp('~/Desktop/damshiny/inst/damshiny/')`

<!-- install.packages('devtools'); library(devtools)

Installing roxygen2 using install_github doesn't seem to work right now 1/3/2013
install_github('roxygen2')

install_github('damshiny')
library(damshiny)

When installed as a package use: shiny::runApp(system.file('rtut/damshiny/inst/damshiny', package='damshiny'))

Suggestions and input are very welcome. -->	