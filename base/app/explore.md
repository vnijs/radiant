> Summarize and explore your data

Generate summary statistics for one or more variables in your data. The most powerful feature in Explore is that you can easy describe the data _by_ one or more other variables. Where the _Pivot_ tab works best for frequency tables and to summarize a single numerical variable, the _Explore_ tab allows you to summarize multiple variables at the same time using various statistics.

For example, if we select `price` from the `diamonds` dataset we can see the number of observations (n), the mean, the median, etc. etc. However,  the mean price for each clarity level of the diamond can also be easily provided by choosing `clarity` as the Group by variable.

The created summary table can be stored in Radiant by clicking the `Store` button. This can be useful if you want to create plots using the summarized data. To download the table to _csv_ format click the download icon on the top-right.

You can select options from `Column variable` dropdown to switch between different column headers. Select either the `functions` (e.g., mean, median, etc), the variables (e.g., price, carat, etc), or the levels of the (first) `Group by` variable (e.g., Fair-Ideal).

![explore table](figures/explore.png)

## Functions

* `n` determines the number of observations, or rows, in the data or in a group if a `group_by` variable has been selected (`n` uses the `length` function in R)
* `n_distinct` determines the number of distinct values in a variable
* `n_missing` determines the number of missing values in a variable
* `cv` is the coefficient of variation (i.e., mean(x) / sd(x))
* `sd` and `var` calculate the sample standard deviation and variance
* `sdp` and `varp` calculate the population standard deviation and variance

### Filter

Use the `Filter` box to select (or omit) specific sets of rows from the data. See the helpfile for _Data > View_ for details.

### Pause explore

For larger datasets it can useful to click `Pause explore` before selecting categorical and numerical variables, entering filters, etc. When you are ready to generate the explore table make sure that `Pause explore` is not checked. When `Pause explore` is not checked, any changes to the inputs will automatically result in a new table.
