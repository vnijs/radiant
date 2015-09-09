> Transform variables in data

### Transform command log

All transformations applied in the _Data > Transform_ tab can be logged. If, for example, you apply a log transformation to a numeric variable the following code is generated and put in the _Transform command log_ window at the bottom of your screen when you click the `Store` button.

<pre>
## transform variable
r_data[["diamonds"]] <- mutate_each(r_data[["diamonds"]], funs(log), ext = "_log", price)
</pre>

This is an important feature if you need to recreate your results at some point in the future or want to re-run a report with new, but similar, data. Even more important is that there is a record of the steps taken to generate a result.

To add commands contained in the command log window to a report in _R > Report_ click the <i title='Report results' class='fa fa-edit'> icon.

### Filter

Filter functionality must be turned off when transforming variables. If a filter is active the transform functions will show a warning message. Either remove the filter statement or un-check the `Filter` check-box. Alternatively, navigate to the Data > View tab and click the `Store` button to store the filtered data. Then return to the Transform tab to make the desired variable changes.

### Type

When you select `Type` from the `Transformation type` drop-down another drop-down menu is shown that will allow you to change the type (or class) of one or more variables. For example, you can change a variable of type integer to a variable of type factor. Click the `Store` button to change variable(s) in the data set. A description of the transformations included in Radiant is provided below.

1. As factor: convert a variable to type factor (i.e., a categorical variable)
2. As number: convert a variable to type numeric
3. As integer: convert a variable to type integer
4. As character: convert a variable to type character (i.e., strings)
5. As date (mdy): convert a variable to a date if the dates are ordered as month-day-year
6. As date (dmy): convert a variable to a date if the dates are ordered as day-month-year
7. As date (dmy): convert a variable to a date if the dates are ordered as year-month-day
8. As date/time (ymd_hms): convert a variable to a date if the dates are ordered as year-month-day-hour-minute-second
9. As time (hms): convert variable to class `period` if the time is organized as hour-minute-second
10. As time (hm): convert variable to class `period` if the time is organized as hour-minute

### Transform

When you select `Transform` from the `Transformation type` drop-down another drop-down menu is shown that will allow you to apply common transformations to one or more variables in your data. For example, to take the (natural) log of a variable select the variable(s) you want to transform and choose `Log` from the `Apply function` menu. A new variable is created with the extension specified in the 'Variable name extension` text input (e.g,. `log_`). Make sure to press `return` after changing the extension. Click the `Store` button to add the variable(s) to the data set. A description of the transformation functions included in Radiant is provided below.

1. Log: create a log-transformed version of the selected variable (i.e., log(x))
2. Square: multiply a variable by itself (i.e., x^2)
3. Square-root: take the square-root of a variable (i.e., x^.5)
4. Center: create a new variable with a mean of zero (i.e., x - mean(x))
5. Standardize: create a new variable with a mean of zero and standard deviation of one (i.e., (x - mean(x)/sd(x)))
6. Invert: 1/x
7. Median split: create a new factor with two levels (Above and Below) that splits the variable values at the median
8. Deciles: create a new factor with 10 levels (deciles) that splits the variable values at the 10th, 20th, ..., 90th percentiles.

### Create

Choose `Create` from the `Transformation type` drop-down. This is the most flexible command to create new or transformed variables. However, it also requires some basic knowledge or R-syntax. A new variable can be any function of other variables in the (active) data. Some examples are given below. In each example the name to the left of the `=` sign is the name of the new variable. To the right of the `=` sign you can include other variable names and basic R-functions. After you have typed the command press return to create the new variable and press `Store` to add it to the dataset.

1. Create a new variable z that is the difference between variables x and y in the data

	z = x - y

2. Create a new variable z that is a transformation of variable x but with mean equal to zero (note that this transformation is also available in the Transform drop-down):

	z = x - mean(x)

3. Create a new `logical` variable z that takes on the value TRUE when x > y and FALSE otherwise

	z = x > y

4. Create a new `logical` z that takes on the value TRUE when x is equal to y and FALSE otherwise

	z = x == y

5. Create a variable z that is equal to x lagged by 3 periods

	z = lag(x,3)

6. Create a categorical variable with two levels

	z = ifelse(x < y, 'smaller', 'bigger')

7. Create a categorical variable with three levels. An alternative approach would be to use the `Recode` function described below

	z = ifelse(x < 60, '< 60', ifelse(x > 65, '> 65', '60-65'))

8. Convert an outlier to a missing value. For example, if we want to remove the maximum value from a variable called `sales` that is equal to 400 we could use an `ifelse` statement and enter the command below in the `Create` box. Press return and `Store` to add new variable `sales_rc` variable. Note that we had entered `sales` on the left-hand side of the `=` sign the orginal variable would have been overwritten

  sales_rc = ifelse(sales > 400, NA, sales)

9. Determine the time difference between two dates/times in seconds

	time\_diff = as\_duration(time2 - time2)

10. Extract the month from a date variable

	month = month(date)

Other attributes that can be extract are `minute`, `hour`, `day`, `week`, `year`, `wday` (for weekday). For `wday` and `month` is can be convenient to add `label = TRUE` to the call.

11. Extract the weekday from a date variable with a label rather than a number

	weekday = wday(date, label = TRUE)

Note: For examples 6 and 7 above you may want to change the type of the new variable to type `factor` before using it for further analysis (see `Type` above)

### Recode

To use the recode feature select the variable you want to change and choose `Recode` from the `Transformation type` drop-down. Provide one or more recode commands (separate the commands by a `;`) and press return to see the newly created variable. Note that you can specify the names for the recode variable in the `Recoded variable name` input box (press return submit changes). Finally, click `Store` to add the new variable to the data. Some examples are given below.

1. Values below 20 are set to 'Low' and all others to 'High'

	lo:20 = 'Low'; else = 'High'

2. Values above 20 are set to 'High' and all others to 'Low'

	20:hi = 'High'; else = 'Low'

2. Values 1 through 12 are set to 'A', 13:24 to 'B', and the remainder to 'C'

	1:12 = 'A'; 13:24 = 'B'; else = 'C'

3.	To collapse age categories for a cross-tab analysis. In the example below '<25' and '25-34' are recoded to '<35', '35-44' and '35-44' are recoded to '35-54', and '55-64' and '>64' are recoded to '>54'

	'<25' = '<35'; '25-34' = '<35'; '35-44' = '35-54'; '45-54' = '35-54'; '55-64' = '>54'; '>64' = '>54'

4. To exclude a particular value (e.g., an outlier in the data) from the data from subsequent analyses we can recode it to a missing value. For example, if we want to remove the maximum value from a variable called `sales` that is equal to 400 we would (1) select the variable `sales` in the `Select column(s)` box and enter the command below in the `Recode` box. Press return and `Store` to add the recoded variable to the data

	400 = NA

**Note:** Never use a `=` symbol in a label (e.g., 50:hi = '>= 50') as this will cause errors.

### Rename

Choose `Rename` from the `Transformation type` drop-down, select one or more variables, and enter new names for them in the rename box shown. Separate each name by a `,`. Press return to see the variables with their new names on screen and  press `Store` to alter the variable names in the original data.

### Replace

Choose `Replace` from the `Transformation type` drop-down if you want to replace existing variables in the data with new ones created using Create, Transform, Clipboard, etc.. Select one or more variables to overwrite and the same number of replacement variables. Press `Store` to alter the original data.

### Clipboard

It is possible to manipulate your data in a spreadsheet (e.g., Excel or Google sheets) and copy-and-paste variables back into Radiant. If you do not have the original data in a spreadsheet already use the clipboard feature in _Data > Manage_ so you can paste it into the spreadsheet. Apply your transformations in the spreadsheet program and then copy the new variable(s), with a header label, to the clipboard (i.e, CTRL-C on windows and CMD-C on mac). Select `Clipboard` from the `Transformation type` drop-down and paste your new data into the `Paste from spreadsheet` box. It is key that the number of observations for the new variable(s) are the same as in the data in Radiant. To add the new variables to the data click `Store`.

> **Note:** Using the clipboard feature for data transformation is discouraged because it is not reproducible.

### Normalize

Choose `Normalize` from the `Transformation type` drop-down to standardize one or more variables. For example, in the diamonds data we may want to express price of a diamond per-carat. Select `carat` as the normalizing variable and `price` in the `Select variable(s)` box. You will see summary statistics for the new variable (e.g., `price_carat`) in the main panel. Store changes by clicking the `Store` button.

### Reorder or remove columns

Choose `Reorder/Remove columns` from the `Transformation type` drop-down. Drag-and-drop variables to reorder them in the data. To remove a variable click the x next to the label. Press `Store` to commit the changes. Note that this action cannot be undone. If you want the original variables back you will have to reload the data through the `Data > Manage` tab.

### Reorder or remove levels

If a (single) variable of type `factor` is selected in `Select variable(s)`, choose `Reorder/Remove levels` from the `Transformation type` drop-down to reorder and/or remove levels. Drag-and-drop levels to reorder them or click the x to remove them. Press `Store` to commit the changes. Note that this action cannot be undone. If you want the original variable back you will have to reload the data through the _Data > Manage_ tab. To temporarily exclude levels from the data use the `Filter` (see the help file linked in the `Data > View` tab).

### Remove missing values

Choose `Remove missing` from the `Transformation type` drop-down to eliminate all rows with one or more missing values. If no variables are selected a row with a missing values in **any** column will be removed. If one or more variables are selected only those rows will be removed with missing values for the selected variables. Press `Store` to change the data. If missing values were present you will see the number of observations in the data summary change (i.e., the value of _n_ changes). Note that this action cannot be undone. If you want these rows back you will have to reload the data through the _Data > Manage_ page.

### Remove duplicates

Certain variables should have only unique values (i.e., no duplicates). Customers id's, for example, should be unique unless we a dataset contains multiple orders for the same customer. In that case the combination if customer id **and** order id should be unique. To remove duplicate rows make sure either no or all variables in the data are selected. To remove rows with duplicates across one or more variables select the variables or interest. Choose `Remove duplicates` from the `Transformation type` drop-down and evaluate how the summary statistics change. Press `Store` to change the data. If there are duplicate rows you will see the number of observations in the data summary change (i.e., the value of _n_ and _n\_distinct_ will change). Note that this action cannot be undone. If you want these rows back you will have to reload the data through the _Data > Manage_ page.
