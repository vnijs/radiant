> Transform variables in data

### Transform command log

All transformations applied in the _Data > Transform_ tab can be _logged_. If, for example, you apply a `log` transformation to numeric variables the following code is generated and put in the _Transform command log_ window at the bottom of your screen when you click the `Store` button.

<pre>
## transform variable
r_data[["diamonds"]] <- mutate_each(r_data[["diamonds"]], funs(log), ext = "_log", price, carat)
</pre>

This is an important feature if you need to recreate your results or you want to re-run a report with new, but similar, data. Even more important is that there is a record of the steps taken to generate all results.

To add commands contained in the command log window to a report in _R > Report_ click the <i title='Report results' class='fa fa-edit'></i> icon.

### Filter

If a filter has been specified it will be ignored for (most) functions available in _Data > Transform_. To create a new dataset based on a filter navigate to the _Data > View_ tab and click the `Store` button. Alternatively, create new dataset based on a Filter select `Holdout sample` from the `Transformation type` dropdown.

### Hide summaries

For larger datasets, or when summaries are not needed, it can useful to click `Hide summaries`before selecting the transformation type and specifying how you want to change variables and data. If you want to see data and variable summaries make sure that `Hide summaries` is not checked.

### Type

When you select `Type` from the `Transformation type` drop-down another drop-down menu is shown that will allow you to change the type (or class) of one or more variables. For example, you can change a variable of type integer to a variable of type factor. Click the `Store` button to change variable(s) in the data set. A description of the transformations included in Radiant is provided below.

1. As factor: convert a variable to type factor (i.e., a categorical variable)
2. As number: convert a variable to type numeric
3. As integer: convert a variable to type integer
4. As character: convert a variable to type character (i.e., strings)
5. As date (mdy): convert a variable to a date if the dates are ordered as month-day-year
6. As date (dmy): convert a variable to a date if the dates are ordered as day-month-year
7. As date (ymd): convert a variable to a date if the dates are ordered as year-month-day
8. As date/time (mdy_hms): convert a variable to a date if the dates are ordered as month-day-year-hour-minute-second
9. As date/time (mdy_hm): convert a variable to a date if the dates are ordered as month-day-year-hour-minute
10. As date/time (dmy\_hms): See mdy\_hms
11. As date/time (dmy\_hm): See mdy\_hm
12. As date/time (ymd\_hms): See mdy\_hms
13. As date/time (ymd\_hm): See mdy\_hm

<!--
14. As time (hms): convert variable to class `period` if the time is organized as hour-minute-second
15. As time (hm): convert variable to class `period` if the time is organized as hour-minute
-->

### Transform

When you select `Transform` from the `Transformation type` drop-down another drop-down menu is shown that will allow you to apply common transformations to one or more variables in the data. For example, to take the (natural) log of a variable select the variable(s) you want to transform and choose `Log` from the `Apply function` drop-down. A new variable is created with the extension specified in the 'Variable name extension` text input (e.g,. `_log`). Make sure to press `return` after changing the extension. Click the `Store` button to add the variable(s) to the data set. A description of the transformation functions included in Radiant is provided below.

1. Ln: create a natural log-transformed version of the selected variable (i.e., log(x) or ln(x))
2. Square: multiply a variable by itself (i.e., x^2 or square(x))
3. Square-root: take the square-root of a variable (i.e., x^.5)
4. Absolute: Absolute value of a variable (i.e., abs(x))
5. Center: create a new variable with a mean of zero (i.e., x - mean(x))
6. Standardize: create a new variable with a mean of zero and standard deviation of one (i.e., (x - mean(x))/sd(x))
7. Invert: 1/x
8. Median split: create a new factor with two levels (Above and Below) that splits the variable values at the median
9. Deciles: create a new factor with 10 levels (deciles) that splits the variable values at the 10th, 20th, ..., 90th percentiles.

### Create

Choose `Create` from the `Transformation type` drop-down. This is the most flexible command to create new or transformed variables. However, it also requires some basic knowledge of R-syntax. A new variable can be any function of other variables in the (active) dataset. Some examples are given below. In each example the name to the left of the `=` sign is the name of the new variable. To the right of the `=` sign you can include other variable names and basic R-functions. After you type the command press `return` to create the new variable. If the result is as expected press `Store` to add it to the dataset.

**Note:** If one or more variables is selected from the list in `Select variables` they will be used to group the data before creating the new variable (see example 1. below). If this is not the intended result make sure that no variables are selected when creating the new variable

1. Create a new variable p that is equal to the mean of price. To calculate the mean of price per group (e.g., per level of clarity) simply select clarity from the `Select variables` list

	z = mean(x)

2. Create a new variable z that is the difference between variables x and y

	z = x - y

3. Create a new variable z that is a transformation of variable x but with mean equal to zero (note that this transformation is also available in the `Transform` drop-down as `Center`):

	z = x - mean(x)

4. Create a new `logical` variable z that takes on the value TRUE when x > y and FALSE otherwise

	z = x > y

5. Create a new `logical` z that takes on the value TRUE when x is equal to y and FALSE otherwise

	z = x == y

6. Create a variable z that is equal to x lagged by 3 periods

	z = lag(x,3)

7. Create a categorical variable with two levels

	z = ifelse(x < y, 'smaller', 'bigger')

8. Create a categorical variable with three levels. An alternative approach would be to use the `Recode` function described below

	z = ifelse(x < 60, '< 60', ifelse(x > 65, '> 65', '60-65'))

9. Convert an outlier to a missing value. For example, if we want to remove the maximum value from a variable called `sales` that is equal to 400 we could use an `ifelse` statement and enter the command below in the `Create` box. Press `return` and `Store` to add the new `sales_rc` variable. Note that if we had entered `sales` on the left-hand side of the `=` sign the original variable would have been overwritten

	sales_rc = ifelse(sales > 400, NA, sales)

10. Similarly, if a respondent with ID 3 provided information in the wrong scale on a survey (e.g., income in \$1s rather than in \$1000s) we could use an `ifelse` statement and enter the command below in the `Create` box. As before, press `return` and `Store` to add the new `sales_rc` variable

	income_rc = ifelse(ID == 3, income/1000, income)

11. If multiple respondents made the same scaling mistake (e.g., those with ID 1, 3, and 15) we again use `Create` and enter:

	income_rc = ifelse(ID %in% c(1, 3, 15), income/1000, income)

12. If you have a date in a format not available through the `Type` menu you can use the `parse_date_time` function. For a date formated as "2-1-14" you would specify the command below (note that this format will also be parsed correctly by the `mdy` function in the `Type` menu)

	date = parse\_date\_time(x, "%m%d%y")

13. Determine the time difference between two dates/times in seconds

	time\_diff = as\_duration(time2 - time1)

14. Extract the month from a date variable

	month = month(date)

15. Other attributes that can be extracted from a date or date-time variable are `minute`, `hour`, `day`, `week`, `quarter`, `year`, `wday` (for weekday). For `wday` and `month` it can be convenient to add `label = TRUE` to the call. For example, to extract the weekday from a date variable and use a label rather than a number

	weekday = wday(date, label = TRUE)

16. Calculate the distance between two locations using lat-long information

	trip\_distance = as_distance(lat1, long1, lat2, long2)

17. Calculate quintiles for a variable `recency` by using the `xtile` command as shown below. To create deciles replace "5" by "10".

	rec\_iq = xtile(recency, 5)

Note: For examples 7, 8, and 15 above you may need to change the new variable to type `factor` before using it for further analysis (see `Type` above)

### Bin

The `Bin` option is a convenience function for the `xtile` command mentioned above when you want to create multiple quitile/decile/... variables. To calculate quintiles enter "5" as the `Nr bins`. The `reverse` option replaces 1 by 5, 2 by 4, ..., 5 by 1. Choose an appropriate variable name extension for the new variables.

### Clipboard

It is possible to manipulate your data in a spreadsheet (e.g., Excel or Google sheets) and copy-and-paste the data back into Radiant. If you don't have the original data in a spreadsheet already use the clipboard feature in _Data > Manage_ so you can paste it into the spreadsheet or click the download icon on the top right of your screen in the _Data > View_ tab. Apply your transformations in the spreadsheet program and then copy the new variable(s), with a header label, to the clipboard (i.e., CTRL-C on windows and CMD-C on mac). Select `Clipboard` from the `Transformation type` drop-down and paste the new data into the `Paste from spreadsheet` box. It is key that new variable(s) have the same number of observations as the data in Radiant. To add the new variables to the data click `Store`.

> **Note:** Using the clipboard feature for data transformation is discouraged because it is not reproducible.

### Normalize

Choose `Normalize` from the `Transformation type` drop-down to standardize one or more variables. For example, in the diamonds data we may want to express price of a diamond per-carat. Select `carat` as the normalizing variable and `price` in the `Select variable(s)` box. You will see summary statistics for the new variable (e.g., `price_carat`) in the main panel. Store changes by clicking the `Store` button.


### Recode

To use the recode feature select the variable you want to change and choose `Recode` from the `Transformation type` drop-down. Provide one or more recode commands, separated by a `;`, and press return to see the newly created variable. Note that you can specify the names for the recoded variable in the `Recoded variable name` input box (press return to submit changes). Finally, click `Store` to add the new variable to the data. Some examples are given below.

1. Values below 20 are set to 'Low' and all others to 'High'

	lo:20 = 'Low'; else = 'High'

2. Values above 20 are set to 'High' and all others to 'Low'

	20:hi = 'High'; else = 'Low'

2. Values 1 through 12 are set to 'A', 13:24 to 'B', and the remainder to 'C'

	1:12 = 'A'; 13:24 = 'B'; else = 'C'

3. Collapse age categories for a cross-tab analysis. In the example below '<25' and '25-34' are recoded to '<35', '35-44' and '35-44' are recoded to '35-54', and '55-64' and '>64' are recoded to '>54'

	'<25' = '<35'; '25-34' = '<35'; '35-44' = '35-54'; '45-54' = '35-54'; '55-64' = '>54'; '>64' = '>54'

4. To exclude a particular value (e.g., an outlier in the data) for subsequent analyses we can recode it to a missing value. For example, if we want to remove the maximum value from a variable called `sales` that is equal to 400 we would (1) select the variable `sales` in the `Select variable(s)` box and enter the command below in the `Recode` box. Press `return` and `Store` to add the recoded variable to the data

	400 = NA

5. To recode specific numeric values (e.g., carat) to a new value (1) select the variable `carat` in the `Select variable(s)` box and enter the command below in the `Recode` box to set the value for carat to 2 in all rows where carat is currently larger than or equal to 2. Press `return` and `Store` to add the recoded variable to the data

	2:hi = 2

**Note:** Never use a `=` symbol in a label when using the recode function (e.g., 50:hi = '>= 50') as this will cause an error.

### Rename

Choose `Rename` from the `Transformation type` drop-down, select one or more variables, and enter new names for them in the rename box shown. Separate each name by a `,`. Press return to see the variables with their new names on screen and press `Store` to alter the variable names in the original data.

### Replace

Choose `Replace` from the `Transformation type` drop-down if you want to replace existing variables in the data with new ones created using, for example, Create, Transform, Clipboard, etc.. Select one or more variables to overwrite and the same number of replacement variables. Press `Store` to alter the data.

### Reorder or remove levels

If a (single) variable of type `factor` is selected in `Select variable(s)`, choose `Reorder/Remove levels` from the `Transformation type` drop-down to reorder and/or remove levels. Drag-and-drop levels to reorder them or click the $\times$ to remove them. Press `Store` to commit the changes. To temporarily exclude levels from the data use the `Filter` box (see the help file linked in the `Data > View` tab).

### Reorder or remove columns

Choose `Reorder/Remove columns` from the `Transformation type` drop-down. Drag-and-drop variables to reorder them in the data. To remove a variable click the $\times$ next to the label. Press `Store` to commit the changes.

### Remove missing values

Choose `Remove missing` from the `Transformation type` drop-down to eliminate rows with one or more missing values. If all variables are selected a row with a missing values in **any** column will be removed. If one or more variables are selected only those rows will be removed with missing values for the selected variables. Press `Store` to change the data. If missing values were present you will see the number of observations in the data summary change (i.e., the value of _n_ changes).

### Remove duplicates

It is common to have one or more variables in a dataset that **should** have only unique values (i.e., no duplicates). Customers id's, for example, should be unique unless the dataset contains multiple orders for the same customer. In that case the combination of customer id **and** order id should be unique. To remove duplicate select one or more variables to determine _uniqueness_. Choose `Remove duplicates` from the `Transformation type` drop-down and check how the summary statistics change. Press `Store` to change the data. If there are duplicate rows you will see the number of observations in the data summary change (i.e., the value of _n_ and _n\_distinct_ will change).

### Show duplicates

If there are duplicates in the data use `Show duplicates` to get a better sense for the data points that have the same value in multiple rows. If you want to explore duplicates using the _View_ tab make sure to `Store` them in a different dataset (i.e., make sure **not** to overwrite the data you are working on). If you choose to show duplicates based on all columns in the data only one of the duplicate rows will be shown. These rows are **exactly** the same so showing 2 or 3 isn't helpful. If, however, we look for duplicates based on a subset of the available variables Radiant will generate a dataset with **all** rows that are deemed similar.

### Training variable

To create a variable that can be used to randomly filter a dataset for analysis (or training) and holdout, select `Training variable` from the `Transformation type` dropdown. Specify either the number of observations to use for training (i.e., `Size` > 1) or a proportion of observations to select (i.e., `Size` > 1). The new variable will have a value "1" for training and "0" holdout.

### Holdout sample

To create a holdout sample based on (the reverse of) a filter select `Holdout sample` from the `Transformation type` dropdown. By default the opposite of the active filter is used. For example, is analysis is conducted on all observations where `date < "2014-12-13"` then the holdout sample will be based on the filter `date >= "2014-12-13"` if the `Reverse filter` box is checked.

<!-- r_data[["diamonds"]] <- mutate(r_data[["diamonds"]], date = as_ymd("2012-1-1") + as.integer(runif(30,1,4*365)) %>% sample(3000, replace = TRUE) %>% sort) -->

### Gather columns

Combine multiple columns into one column. If you have the `diamonds` dataset loaded, select `cut` and `color` in the `Select variable(s)` box after selecting `Gather columns` from the `Transformation type` dropdown. This will create new variables `key` and `value`. `key` has two values: `cut` and `color` while `value` captures all values that were in `cut` and `color`.

### Spread column

Spread one column into multiple columns. The opposite operation for `gather`.

### Table-to-data

Turn a frequency table into a dataset. The number of rows will equal the sum of all frequencies.

### Expand grid

Create a dataset with all combinations of values for a selection of variables. Useful to generate datasets for prediction in, for example, _Regression > Linear regression (OLS)_ or _Regression > Logistic regression (GLM)_. Suppose you wanted to create a dataset with all possible combinations of values for `cut` and `color` of a diamond. By selecting `Expand grid` from the `Transformation type` dropdown and `cut` and `color` in the `Select variable(s)` box we can see in the screenshot below that there are 35 combinations (i.e., `cut` has 5 unique values and `color` has 7 unique values so 5 x 7 combinations are possible). Choose a name for the new dataset (e.g., diamonds_expand) and click the `Store` button to add it to the `Datasets` dropdown.

![expand](figures/expand_grid.png)
