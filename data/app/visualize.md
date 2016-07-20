> Visualize data

### Filter

Use the `Filter` box to select (or omit) specific sets of rows from the data. See the helpfile for <a href="/docs/data/view.html" target="_blank">_Data > View_</a> for details.

### Pause plotting

For larger datasets it can useful to click `Pause plotting` before selecting variables, entering filters, etc. When you are ready to generate a plot make sure that `Pause plotting` is un-checked. When `Pause plotting` is un-checked, any input change will automatically result in a new plot.

### Plot-type

Select the plot type you want. Choose histograms or density plots for one or more single-variable plots. For example, with the `diamonds` data loaded select `Histogram` and all (X) variables (use CTRL-a or CMD-a). This will create histograms for all variables in the dataset. Scatter plots are used to visualize the relationship between two variables. Select one or more variables to plot on the Y-axis and one or more variables to plot on the X-axis. If one of the variables is categorical (i.e., a {factor}) it should be specified as an X-variable. Line plots are similar to scatter plots but they connect-the-dots and are particularly useful for time-series data. Bar plots are used to show the relationship between a categorical (or integer) variable (X) and the (mean) value of a numeric variable (Y). If the Y-variable in a bar plot is categorical (i.e., a {factor}) the proportion of occurrence of the first-level in that variable is shown (e.g., if we select `color` from the `diamonds` data as the Y-variable each bar represents the proportion of observations with the value `D`). Box-plots are also used when we have a numeric Y-variable and a categorical X-variable. They are more informative than bar charts but also require a bit more effort to evaluate.

### Box plots

The upper and lower "hinges" of the box correspond to the first and third quartiles (the 25th and 75th percentiles) in the data. The middle hinge is the median value of the data. The upper whisker extends from the upper hinge (i.e., the top of the box) to the highest value in the data that is within 1.5 x IQR of the upper hinge. IQR is the inter-quartile range, or distance, between the 25th and 75th percentile. The lower whisker extends from the lower hinge to the lowest value in the data within 1.5 x IQR of the lower hinge. Data beyond the end of the whiskers could be outliers and are plotted as points (as suggested by Tukey).

In sum:
1. The lower whisker extends from Q1 to max(min(data), Q1 - 1.5 x IQR)
2. The upper whisker extends from Q3 to min(max(data), Q3 + 1.5 x IQR)

where Q1 is the 25th percentile and Q3 is the 75th percentile. You may have to read the two bullets above a few times before it sinks in. The plot below should help to explain the structure of the box plot.

<p align="center"><img src="figures/boxplot.png"></p>
[Source](http://en.wikipedia.org/wiki/File:Boxplot_vs_PDF.svg)

### Sub-plots and heat-maps

`Facet row` and `Facet column` can be used to split the data into different groups and create separate plots for each group.

If you select a scatter or line plot a `Color` drop-down will be shown. Selecting a `Color` variable will create a type of heat-map where the colors are linked to the values of the `Color` variable. Selecting a categorical variable from the `Color` dropdown for a line plot will split the data into groups and will show a line of a different color for each group.

### Line, loess, and jitter

To add a linear or non-linear regression line to a scatter plot check the `Line` and/or `Loess` boxes. If your data take on a limited number of values, `Jitter` can be useful to get a better feel for where most of the data points are located. `Jitter`-ing simply adds a small random value to each data point so they do not overlap completely in the plot(s).

### Axis scale

The relationship between variables depicted in a scatter plot may be non-linear. There are numerous transformations we might apply to the data so this relationship becomes (approximately) linear (see _<a href="/docs/data/transform.html" target="_blank">Data > Transform</a>_) and easier to estimate using, for example, _<a href="/docs/model/regress.html" target="_blank">Model > Estimate > Linear regression (OLS)</a>_. Perhaps the most common data transformation applied to business data is the (natural) logarithm. To see if log transformation(s) may be appropriate for your data check the `Log X` and/or `Log Y` boxes (e.g., for a scatter or bar plot).

By default the scale of the Y-axis is the same across sub-plots when using `Facet row`. To allow the Y-axis to be specific to each sub-plot click the `Scale-y` check-box.

### Flip axes

To switch the variables on the X- and Y-axis check the `Flip` box.

### Plot height and width

To make plots bigger or smaller adjust the values in the height and width boxes on the bottom left of the screen.

### Keep plots

The best way to keep/store plots is to generate a `visualize` command by clicking the report (<i title='Report results' class='fa fa-edit'></i>) icon on the bottom left of your screen. Alternatively, click the <i title='Download' class='fa fa-download'></i> icon on the top right of your screen to save a png-file to disk.

### Customizing plots in _R > Report_

To customize a plot first generate the `visualize` command by clicking the report (<i title='Report results' class='fa fa-edit'></i>) icon on the bottom left of your screen. The example below illustrates how to customize a command in the <a href="/docs/data/report.html" target="_blank">_R > Report_</a> tab. Notice that `custom` is set to `TRUE`.

```r
visualize("diamonds", yvar = "price", xvar = "carat", type = "scatter", custom = TRUE) +
  ggtitle("A scatterplot") + xlab("price in $")
```

**Some common customization commands:**

* Add a title: `+ ggtitle("my title")`
* Change label: `+ xlab("my X-axis label")` or `+ ylab("my X-axis label")`
* Remove legend: `+ theme(legend.position = "none")`
* Change legend title: `+ guides(color = guide_legend(title = "New title"))` or `+ guides(fill = guide_legend(title = "New title"))`
* Rotate tick labels: `+ theme(axis.text.x = element_text(angle = 90, hjust = 1))`
* Set plot limits: `+ ylim(15, 20)` or `+ xlim("VS1","VS2")`

See the ggplot2 documentation page for additional options <a href="http://docs.ggplot2.org/" target="_blank">http://docs.ggplot2.org</a>.
