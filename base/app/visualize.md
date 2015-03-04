> Visualize data

#### Filter

Use the `Filter` box to select (or omit) specific sets of rows from the data. See the helpfile for Data > View for details.

#### Plot-type

Select the plot type you want. Choose histograms or density for one or more single variable plots. For example, with the `diamonds` data loaded select `Histogram` and all (X) variables (use CTRL-A or CMD-A). This will create histograms for all variables in your dataset. Scatter plots are used to show the relationship, or lack thereof, between two variables. Select a variable to plot on the Y-axis and one or more variables to plot on the X-axis. Line plots are similar to scatter plots but they connect-the-dots and are particularly useful for time-series data. Bar plots are used to show the relationship between a categorical variable (X-axis) and a numeric variable (Y-axis). Box-plots are also used when you have a numeric Y-variable and a categorical X-variable. They are more informative than bar charts but also require a bit more effort to evaluate.

#### Box plots

The upper and lower "hinges" of the box correspond to the first and third quartiles (the 25th and 75th percentiles) in the data. The middle hinge is the median value of the data. The upper whisker extends from the upper hinge (i.e., the top of the box) to the highest value in the data that is within 1.5 x IQR of the upper hinge. IQR is the inter-quartile range, or distance, between the first and third quartiles. The lower whisker extends from the lower hinge to the lowest value in the data within 1.5 x IQR of the lower hinge. Data beyond the end of the whiskers could be outliers and are plotted as points (as suggested by Tukey).

In sum:
1. The upper whisker extends from Q3 to min(max(data), Q3+1.5*IQR)
2. The lower whisker extends from Q1 to max(min(data), Q1-1.5*IQR)

You may have to read the two bullets above a few times before it sinks in. The plot below should help to explain the structure of the box plot.

![Box-plot](figures/boxplot.png)
[Source](http://en.wikipedia.org/wiki/File:Boxplot_vs_PDF.svg)

#### Sub-plots and heat-maps

`Facet row` and `Facet column` can be used to split the data into different groups and create separate plots for each group.

If you select a scatter or line plot a `Color` drop-down will be shown. Selecting a `Color` variable will create a type of heat-map where the colors are linked to the values of the `Color` variable. Selecting a categorical variable from the `Color` dropdown for a line plot will split the data into groups and will show a line of a different color for each group.

#### Line, Loess, and Jitter

To add a linear or non-linear regression line to a scatter plot check the Line and/or Loess boxes. If your data take on a limited number of values checking Jitter can be useful to get a better feel for where most of the data points are located. Jitter-ing simply adds a small random value to each data point so they do not overlap completely in the plot(s).

#### Axis scale

The relationship between variables depicted in a scatter plot may be non-linear. There are numerous transformations we might apply to the data so this relationship becomes (approximately) linear (see Data > Transform) and easier to estimate. Perhaps the most common data transformation applied to business data is the (natural) log. To see if a log-linear or log-log transformation may be appropriate for your data check the `Log X` and/or `Log Y` boxes.

#### Flip axes

To switch the variable on the X- and Y-axis check the `Flip` box.

#### Plot height and Width

To make plots bigger or smaller adjust the values in the height and width boxes on the bottom left.
