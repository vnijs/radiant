## Radiant: Business analytics using R and Shiny

Interactive business analytics using [R](http://www.r-project.org/) and [Shiny](http[://www.rstudio.com/shiny/). Developed by [Vincent Nijs]("http://rady.ucsd.edu/faculty/directory/nijs/") and [Terence August]("http://rady.ucsd.edu/faculty/directory/august/"). You can reach us at radiant@rady.ucsd.edu

### Source code

Three (related) apps are included in the inst directory. `Base`, offers data loading, saving, viewing, visualizing, merging, and transforming tools. The `quant` app sources the code from base and extends it. Finally, the marketing app sources the code from base and quant and extends it with additional tools.

### Install

- Required: [R](http://cran.rstudio.com/), version 3.1.1
- Required: A modern browser (e.g., Chrome, Safari, or Firefox)
- Suggested: [Rstudio](http://www.rstudio.com/products/rstudio/download/)

To download the app click the `Download ZIP` button and unzip the file to, for example, your Desktop. To start, for example, the base app use `setwd()` to move to the radiant directory in R(studio). Install Shiny using `install.packages('shiny')`. Then use `shiny::runApp('inst/base')` to run the base app. When Radiant starts for the first time a number of required packages will be installed from a local [miniCRAN](https://github.com/andrie/miniCRAN).

### Todo

- Use dplyr, tidyr, and magrittr to explore, transform, and filter data
- Code documentation
- Automated testing using Rselenium
- etc. etc.

### License

The Radiant tool is licensed under the <a href="http://www.tldrlegal.com/l/AGPL3" target="\_blank">AGPLv3</a>. The help files are licensed under the creative commons attribution, non-commercial, share-alike license <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="\_blank">CC-NC-SA</a>.

As a summary, the AGPLv3 license requires, attribution, include copyright and license in copies of the software, state changes if you modify the code, and disclose all source code. Details are in the COPYING file.

If you are interested in using Radiant please email me at vnijs@ucsd.edu

&copy; Vincent Nijs (2014) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a>
