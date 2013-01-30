## Data analytics using Shiny

Interactive data analytics using [R](http://www.r-project.org/) and [Shiny](http://www.rstudio.com/shiny/) by [Rstudio](http://www.rstudio.com/). 

### Todo:
- Add analysis-tools (e.g., conjoint, factor, perceptual maps, MDS, GLM)
- Use knitr to log analysis output
- Create help files
- etc. etc.

### Run the development version of the marketing app locally

- Required: [R](http://cran.rstudio.com/)
- Required: A modern browser (e.g., Chrome, Firefox, or Safari). Internet Explorer is not supported.
- Suggested: [Rstudio](http://www.rstudio.com/ide/download/)

Start R(studio) and copy-and-paste the commands below:

	install.packages('shiny', repos = "http://cran.rstudio.com")
	shiny::runGitHub('radyant','mostly-harmless', subdir = 'inst/marketing')

### License
The radyant package is licensed under the GPLv3. See the files listed below for additional details.

- COPYING - radyant package license (GPLv3)
- NOTICE - Copyright notices for additional included software