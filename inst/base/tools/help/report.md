> Create a (reproducible) report

The report feature in Radiant should be used in conjunction with the <i title='Report results' class='glyphicon glyphicon-book'></i> icons shown on the bottom of the (left) side bar on all analysis pages. When that icon is clicked the command used to create the output is copied into the editor in the R > Report. By default Radiant will paste the code generated for the analysis you just completed at the bottom of the report. However, you can turn off that feature by clicking the `Manual paste (off)` button. The text in the button should now read `Manual paste (on)`. Click the button again to turn manual paste off again. With manual paste on the code is put in the clipboard when you click a book icon and you can paste it where you want in the R > Report editor window.

By clicking the `Update` button, the output from the analysis will be recreated on the right of the R > Report page. You can add text, bullets, headers, etc. around the code blocks to describe and explain the results using <a href="http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html" target="_blank">markdown</a>. To evaluate only a part of the report use the cursor to select a section and press CTRL-return (CMD-return on mac) to create the (partial) output.

As an example you can copy-and-paste the text below into the editor. You can also load and (R)markdown file into Radiant by clicking the `Choose File` button and selecting an .md or .Rmd file. Press the `Update` button to see the output.

Finally, you can save the Rmd file open in the editor by pressing the `Save Rmd` button or save the generated output by pressing the `Save HTML` button.

<pre>## Sample report

This is an example of the type of report you can write in Radiant.

* You can create
* bullet lists

1. And numbered
2. lists

### Math

You can even include math if you want:

$$y_t = \\alpha + \\beta x_t + \\epsilon_t$$.

To show the output press the `Update` button.

### Documenting analysis results in Radiant

Below is some code created in Radiant that will generate regression output
for the _diamonds_ data. These are histograms and a scatterplot
/ heatmap of the price of diamonds versus carats. The colors in the plot
reflect the clarity of the diamond.

```{r fig.width=7, fig.height=4}
result <- regression(dataset = 'diamonds', dep_var = 'price',
                     indep_var = 'carat')
summary(result)
plot(result, plots = 'hist')
```

```{r fig.width=7, fig.height=7}
visualize(dataset = 'diamonds', xvar = 'carat', yvar = 'price',
          type = 'scatter', color = 'clarity')
```
</pre>
