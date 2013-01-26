## Data analytics using Shiny

Interactive data analytics using [R](http://www.r-project.org/) and [Shiny](http://www.rstudio.com/shiny/) by [Rstudio](http://www.rstudio.com/). 

### Todo:
- Add analysis-tools (e.g., conjoint, factor, perceptual maps, MDS, GLM)
- Use knitr to log analysis output
- Create help files
- etc. etc.

<!-- ### To use the marketing app in the radyant package locally

	install.packages('shiny')
	install.packages('radyant', repos = c('http://vnijs.rady.ucsd.edu/site_media/R_packages/','http://cran.rtudio.com'))
	library(shiny)
	shiny::runApp(system.file('marketing',package='radyant'))
 -->

### Run the development version of the marketing app locally

- Required: [R](http://cran.rstudio.com/)
- Suggested: [Rstudio](http://www.rstudio.com/ide/download/)

Start R(studio) and copy-and-paste the commands below:

	install.packages('shiny')
	shiny::runGitHub('radyant','mostly-harmless', subdir = 'inst/marketing')

<!-- 	install.packages('devtools')
	library(devtools)
	install_github('shiny', username = 'rstudio')
	install_github('shiny-incubator', username = 'rstudio')
	install_github('radyant', username = 'mostly-harmless')
	library(shiny)
	shiny::runApp(system.file('marketing',package='radyant'))

 -->
<!-- ### To use the development version of the finance app locally

	install.packages('devtools')
	library(devtools)
	install_github('shiny', username = 'rstudio')
	install_github('shiny-incubator', username = 'rstudio')
	install_github('radyant', username = 'mostly-harmless')
	library(shiny)
	shiny::runApp(system.file('finance',package='radyant'))

 -->
 
### License
The radyant package is licensed under the GPLv3. See the files listed below for additional details.

COPYING - radyant package license (GPLv3)
NOTICE - Copyright notices for additional included software