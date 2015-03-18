# Radiant - Business analytics using R and Shiny


A platform-independent browser-based interface for business analytics in [R](http://www.r-project.org/), based on the [Shiny](http://www.rstudio.com/shiny/) package. Radiant is designed to facilitate decision making in business using data and models. It is currently used in the Research for Marketing Decisions and Quantitative Analysis classes at the <a href="http://rady.ucsd.edu/" target="\_blank">Rady School of Management</a> by <a href="http://rady.ucsd.edu/faculty/directory/nijs/" target="\_blank">Vincent Nijs</a> and <a href="http://rady.ucsd.edu/faculty/directory/august/">Terrence August</a>. Developed by <a href="http://rady.ucsd.edu/faculty/directory/nijs/" target="\_blank">Vincent Nijs</a>. For questions and comments please contact me at radiant@rady.ucsd.edu.

[![Travis-CI Build Status](https://travis-ci.org/mostly-harmless/radiant.png?branch=master)](https://travis-ci.org/mostly-harmless/radiant)
Version: 0.1.75, Date: 2015-3-17

## Key features

- Explore: Quickly and easily summarize and visualize your data
- Interactive: Results update immediately when inputs are changed (i.e., no separate dialog boxes)
- Save and share your work: Load and save the state of the application to continue your analysis at a later time or on another computer. Share the state-file with others or create a (reproducible) report using [Rmarkdown document](http://rmarkdown.rstudio.com/). You will not lose your work if you accidentally leave the radiant page in your browser
- Cross-platform: It runs in a browser (e.g., Chrome or Firefox) on Windows, Mac, and Linux. No data will leave your computer and you can run it without an internet connection. You can, however, also run it as a web application on a server.
- Context: Data and examples focus on business applications

## Goal

Provide access to the power of R for business and marketing analytics. Radiant also provides a bridge to programming in R(studio). For example, you can run your analyses in Radiant and output the relevant function calls to an [Rmarkdown document](http://rmarkdown.rstudio.com/) (see `R > Report`). Reproducible research, with no programming required. Most pages have an icon you can click (e.g., the book icon on the bottom left on the `Base > Single mean` page) to start working on your report. Press `Update` to render the report to HTML.

You can also use Rstudio to render and edit rmarkdown documents created in Radiant. When you install and load Radiant it exports functions that can be called from R-code and/or an rmarkdown document. If you close Radiant using `Quit > Quit` after launching it from Rstudio or Rgui you can paste the commands below into the command console to get the same output as in the browser.

```r
result <- single_mean("diamonds","price", sm_comp_value = 3500)
summary(result)
plot(result, sm_plot = c("hist","simulate"))
```

You can call functions for visualization and analysis in your R-code and access basic help from the R(studio) console using:

```r
?single_mean
```

As an example, compile the [`single_mean.Rmd`](https://github.com/mostly-harmless/radiant/blob/master/inst/examples/single_mean.Rmd?raw=true) file to html (or pdf or word if you prefer) in Rstudio. Try the code in [`radiant_rcode.R`](https://raw.githubusercontent.com/mostly-harmless/radiant/master/inst/examples/radiant_rcode.R) for a more extensive example.

Note: If you exit the app by stopping the process in R(studio) or closing the browser, rather than using `Quit > Quit`, calling analysis functions may produce an error if the data cannot be found.

## Documentation

Documentation and tutorials for Radiant are available at <http://mostly-harmless.github.io/radiant/> and through the help files in the Radiant interface (see the `?` icon on most pages and the Help menu in the navbar).

To help you get started you can also take a look at this [playlist](https://www.youtube.com/watch?v=e02LFmNysoM&list=PLNhtaetb48EfAAlfQMJsuvLCSLvcn_0BC).

## How-to install Radiant

- Required: [R](http://cran.cnr.berkeley.edu/), version 3.1.2 or later
- Required: A modern browser (e.g., Chrome, Safari, or Firefox). The latest version of internet explorer may work as well.
- Recommended: [Rstudio](http://www.rstudio.com/products/rstudio/download/)

Radiant is now available on [CRAN](http://cran.r-project.org/web/packages/radiant/index.html). To install the latest version of the package with complete documentation for off-line access use the [radiant_miniCRAN](https://github.com/mostly-harmless/radiant_miniCRAN) repository (created using [miniCRAN](https://github.com/andrie/miniCRAN)). Open R(studio) and copy-and-paste the commands below.

```r
options(repos = c(XRAN = 'http://mostly-harmless.github.io/radiant_miniCRAN/'))
install.packages('radiant')
```

Once all packages are installed use the commands below to start the app:

```r
library(radiant)
radiant('marketing')
```

`marketing` is the default app so you could also use the command `radiant()` to launch it. To start the `quant` app use:

```r
library(radiant)
radiant('quant')
```

See also the [installing Radiant](https://www.youtube.com/watch?v=AtB2SsmzBsk) video.

<iframe width="640" height="390" src="https://www.youtube.com/embed/AtB2SsmzBsk" frameborder="0" allowfullscreen></iframe>

## Creating a desktop launcher

You can create a launcher on your Desktop to make it easy to start Radiant. On Windows type `win_launcher()` in the R-console and press return. A file called `radiant.bat` will be created that you can double-click to start Radiant in your default browser.

On a Mac you can create a launcher for Radiant by typing `mac_launcher()` in the R-console. This will create a file called `radiant.command` on your Desktop. Double-click the icon to start Radiant in your default browser.

Radiant runs in a browser window. You will see data on diamond prices. To close the application click on `Quit` in the Navigation bar and then click the `Quit` button on the left of the screen. The Radiant process will stop and the browser window will close (or gray-out).

## Saving and loading state

To save your analyses save the state of the app to a file (Data > Manage). You can open this state file at a later time or on another computer to continue where you left off. You can also share the file with others that may want to replicate your analyses. As an example, load the state_file [`RadiantState.rda`](https://github.com/mostly-harmless/radiant/blob/master/inst/examples/RadiantState.rda?raw=true) in the `examples` folder. Go to `Data > View`, `Data > Visualize` to see some of the settings. There is also a report in `R > Report` that was created using the Radiant interface. The html file [`RadiantState.html`](https://github.com/mostly-harmless/radiant/blob/master/inst/examples/RadiantState.html?raw=true) contains the output.

A related feature in Radiant is that state is maintained if you accidentally navigate to another page, close (and reopen) the browser, and/or hit refresh. Use Quit > Reset to return to a clean/new state.

Loading and saving state also works with Rstudio. If you start Radiant from Rstudio and use Quit > Quit to stop the app, lists called `r_data` and `r_state` will be put into Rstudio's global workspace. If you start radiant again using `radiant()` it will use these lists to restore state. This can be convenient if you want to make changes to a data file in Rstudio and load it back into Radiant. Also, if you load a state file directly into Rstudio it will be used when you start Radiant to recreate a previous state.

**Technical note**: The way loading state works in Radiant is as follows: When an input is initialized in a Shiny app you set a default value in the call to, for example, numericInput. In Radiant, when a state file has been loaded and an input is initialized it looks to see if there is a value for an input of that name in a list called `r_state`. If there is, this value is used. The `r_state` list is created when saving state using `reactiveValuesToList(input)`. An example of a call to numericInput is given below where the `state_init` function from `radiant.R` is used to check if a value from `r_state` can be used. `sm_args$sm_comp_value` is the default value specified in the `single_mean` function call.

```r
numericInput("sm_comp_value", "Comparison value:", state_init('sm_comp_value',sm_args$sm_comp_value))
```

## Source code

Three (related) apps are included in the inst/ directory. `Base`, offers data loading, saving, viewing, visualizing, merging, and transforming tools. The `quant` app sources the code from base and extends it. Finally, the `marketing` app sources the code from `base` and `quant` and extends it with additional tools. The `quant` app focuses on (basic) quantitative analysis (e.g., comparing means, regression, etc.). The `marketing` app focuses on marketing analytics by adding clustering, principle component analysis, conjoint analysis, etc.

<!-- ## Online

If you really don't want to install Radiant on your computer (yet) you can tryout the app online at <http://vnijs.rady.ucsd.edu:3838/marketing>. Note that this is a test server only!

 -->
## License


Radiant is licensed under the <a href="http://www.tldrlegal.com/l/AGPL3" target="\_blank">AGPLv3</a>. The documentation and videos on this site and the radiant help files are licensed under the creative commons attribution, non-commercial, share-alike license <a href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">CC-NC-SA</a>.

As a summary, the AGPLv3 license requires, attribution, including copyright and license information in copies of the software, stating changes if the code is modified, and disclosure of all source code. Details are in the COPYING file.

If you are interested in using Radiant please email me at radiant@rady.ucsd.edu

&copy; Vincent Nijs (2015) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="https://github.com/mostly-harmless/radiant/blob/master/inst/base/www/imgs/80x15.png" /></a>
