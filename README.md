## Data Analysis Menu in Shiny

This is a data-analysis menu for R using [shiny](http://www.rstudio.com/shiny/). If you would like to see the app in action please visit Rstudio's Glimmer-server at [http://glimmer.rstudio.com/mostlyhrmls/damshiny/](http://glimmer.rstudio.com/mostlyhrmls/damshiny/)

### Todo:
- Add analysis-tools (e.g., MDS, factor, perceptual maps, conjoint)
- Create help files
- Add options for compare means (e.g., one-sided) and regression (e.g., F-test for competing models)
- Add time-series plots with 1 or 2 y-axes
- Develop install structure so users can pick-and-choose menu-components. For an amazing implementation see [Bootstrap](http://twitter.github.com/bootstrap/customize.html) although this is *well* beyond what I have in mind.
- etc. etc.
		
### To use the damshiny package locally

	install.packages('shiny')
	install.packages('damshiny', repos = c('http://vnijs.rady.ucsd.edu/site_media/R_packages/','http://cran.rtudio.com')
	library(shiny)
	shiny::runApp(system.file('damshiny',package='damshiny'))

### To use the damshiny development version locally

	install.packages('devtools')
	library(devtools)
	install_github('shiny', username = 'rstudio')
	install_github('shiny-incubator', username = 'rstudio')
	install_github('damshiny', username = 'mostly-harmless')
	library(shiny)
	shiny::runApp(system.file('dev',package='damshiny'))


<!-- install.packages('devtools'); library(devtools)

Installing roxygen2 using install_github doesn't seem to work right now 1/3/2013
install_github('roxygen2')

install_github('damshiny')
library(damshiny)

When installed as a package use: shiny::runApp(system.file('rtut/damshiny/inst/damshiny', package='damshiny'))

Suggestions and input are very welcome. -->	