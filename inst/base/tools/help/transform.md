> Modify type, change, normalize, create, add, reorder, and remove variables in the data.

#### Type

When you select `Type` from the `Transformation type` drop-down another drop-down menu is shown that will allow you to change the type or class of one or more variables in your data. For example, you can change a variable of type factor or character to a variable of type date. Click the `Save changes` button to change variable(s) in the data set. A description of the transformations included in Radiant is provided below.

1. As factor: convert a variable to type factor (i.e., as.factor(x))
2. As number: convert a variable to type numeric (i.e., as.numeric(x))
3. As integer: convert a variable to type integer (i.e., as.integer(x))
4. As character: convert a variable to type character (i.e., as.character(x))
5. As date (mdy): R will, by default, read dates as factors. Use this function if the dates are ordered as month-day-year
6. As date (dmy): R will, by default, read dates as factors. Use this function if the dates are ordered as day-month-year
7. As date (ymd): R will, by default, read dates as factors. Use this function if the dates are ordered as year-month-day
8. As date/time (ymd_hms): R will, by default, read dates as factors. Use this function if the dates are ordered as year-month-day-hour-minute-second

#### Change

When you select `Change` from the `Transformation type` drop-down another drop-down menu is shown that will allow you do apply common transformations to one or more variables in your data. For example, to take the (natural) log of a variable select the variable you want to change and choose `Log` from the `Apply function` menu. A new variable is created with the prefix `log_`. Click the `Save changes` button to add the variable(s) to the data set. A description of the transformation functions included in Radiant is provided below.

1. Log: create a log-transformed version of the selected variable (i.e., log(x))
2. Square: multiply a variable by itself (i.e., x^2)
3. Square-root: take the square-root of a variable (i.e., x^.5)
4. Center: create a new variable with a mean of zero (i.e., x - mean(x))
5. Standardize: create a new variable with a mean of zero and standard deviation of one (i.e., (x - mean(x)/sd(x)))
6. Invert: 1/x
7. Median split: create a new factor with two levels (Above and Below) that splits the variable values at the median
8. Deciles: create a new factor with 10 levels (deciles) that splits the variable values at the 10th, 20th, ..., 90th percentiles.

#### Normalize

Choose `Normalize` from the `Transformation type` drop-down standardize one or more variables. For example, in the diamonds data we may want to express price of a diamond per-carat. Select `carat` as the normalizing variable and `price` in the `Select variable(s)` box. Then click the `Save changes` button.

#### Create

Choose `Create` from the `Transformation type` drop-down. This is the most flexible command to create new or transformed variables. However, it also requires some knowledge or R-syntax. A new variable can be any function of other variables in the data. Some examples are given below. In each example the name to the left of the `=` sign is the name of the new variable. On the right of the `=` sign you can include other variable names and basic R-functions. After you have typed the command press CTRL-return (or CMD-return on mac) to create the new variable and press `Save changes` to add it to the data.

1. Create a new variable z that is the difference between variables x and y in the data

	z = x - y

2. Create a new variable z that is a transformation of variable x but with mean equal to zero (note that this transformation is available in the Change drop-down):

	z = x - mean(x)

3. Create a new factor z that takes on the value TRUE when x > y and FALSE otherwise

	z = x > y

4. Create a new factor z that takes on the value TRUE when x is equal to y and FALSE otherwise

	z = x == y

5. Create a variable z that is equal to x lagged by 3 periods

	z = lag(x,3)

6. Create a categorical variable with two levels

	z = ifelse(x < y, 'x < y', 'x < y')

7. Create a categorical variable with three levels

	z = ifelse(x < 60, 'x < 60', ifelse(x > 65, 'x > 65', '60-65'))

Note: For 6 and 7 you may need to convert the new variable to a factor for analysis (see `Type` above)

#### Clipboard

It is possible to manipulate your data in Excel and copy-and-paste a new variable back into R. If you do not have the original data in Excel use the clipboard feature in Data > Manage to save the data to the clipboard so you can paste it into Excel. Apply your transformations in Excel and then copy the new variable, with a header label, to the clipboard in Excel (i.e, CTRL-C on windows and CMD-C on mac). Select `Clipboard` from the `Transformation type` dropdown and paste your new data into the `Paste from Excel` box. It is key that the number of observations for the new variable is the same as in the original data. The new variable will be shown on screen. To add the variable to the data click `Save changes`.

#### Recode

To use the recode feature select the variable you want to change and choose `Recode` from the `Transformation type` dropdown. Provide one or more recode commands (separate the commands by a `;`) and press CTRL-return (or CMD-return on mac) to see the newly created variable. Click `Save changes` to add the new variable to the data. Some examples are given below.

1. All values below 20 are set to 'Low' and all others to 'High'

	lo:20 = 'Low'; else = 'High'

2. Values 1 through 12 are set to 'A', 13:24 to 'B', and the remainder to 'C'

	1:12 = 'A'; 13:24 = 'B'; else = 'C'

3.	To collapse age categories for a cross-tab analysis. In the example below '<25' and '25-34' are recoded to '<35', '35-44' and '35-44' are recoded to '35-54', and '55-64' and '>64' are recoded to '>54'

	'<25' = '<35'; '25-34' = '<35'; '35-44' = '35-54'; '45-54' = '35-54'; '55-64' = '>54'; '>64' = '>54'

4. To exclude a particular value (e.g., an outlier in the data) from the data from subsequent analyses we can recode it to a missing value. For example, if we want to remove the maximum value from a variable called `sales` that is equal to 400 we would (1) select the variable `sales` in the `Select column(s)` box and enter the command below in the `Recode box`. Press CTRL-return (or CMD-return on mac) and `Save changes` to add the recoded variable to the data.

	400 = NA

#### Rename

Choose `Rename` from the `Transformation type` dropdown, select one or more variables and enter new names for them in the rename box shown. Separate each name by a `,`. Press CTRL-return (or CMD-return on mac) to see the variables with their new names on screen and  press `Save changes` to alter the variable names in the original data.

#### Remove columns

Choose `Remove columns` from the `Transformation type` dropdown and select one or more variables to remove. Press `Save changes` to remove the variables from the original data. Note that this action cannot be undone. If you want the original variables back you will have to reload the data through the Data > Manage page.

#### Remove missing

Choose `Remove missing` from the `Transformation type` dropdown to eliminate all rows with one or more missing values. Press `Save changes` to change the data. If missing values were present you will see the number of observations in the data summary change (i.e., the value of _n_ changes). Note that this action cannot be undone. If you want these rows back you will have to reload the data through the Data > Manage page.

#### Filter

 There are several ways to select a subset of the data to view. The `Filter` box on the left (click the checkbox first) can be used with `>` and `<` signs and you can also combine subset commands. For example, `x > 3 & y == 2` would show only those rows for which the variable `x` has values larger than 3 **and** for which `y` has values equal to 2. Note that in R `=` is used to _assign_ a value and `==` to evaluate if the value of a variable is equal to some other value. In contrast `!=` is used to determine if a variable is _unequal_ to some value. You can also use expression that have an **or** condition. For example, to select rows where `Salary` is larger than $100,000 or smaller than $20,000 use `Salary < 20000 | Salary > 100000`. `|` is the symbol for **or**. The table below gives an overview of common operators. Type your statement in the `Filter`  box and press CTRL-return (or CMD-return on mac) to see the result on screen or an error below the box if the expression is invalid. It is important to note that filters are _presistent_. A filter entered in any of the Data-tabs will also be applied to other tabs and to the analyses conducted through any of the other menus in Radiant. To remove a filter you have to (1) erase it and press CTRL-return (or CMD-return on mac) or (2) uncheck the `Filter` checkbox.


| Operator    |     | Description               |
| ----------- | --- |:------------------------- |
| <	          |     | less than                 |
| <=			    |     | less than or equal to     |
| > 			    |     | greater than              |
| >=	 	      |     | greater than or equal to  |
| ==	 		    |     | exactly equal to          |
| !=	 		    |     | not equal to              |
| x &#124; y	|     | x OR y                    |
| x & y	      |     | x AND y                   |


</br>Choose `Filter` from the `Transformation type` dropdown to select (or omit) specific sets of rows from the data. You should see the number of observations in the data summary change (i.e., the value of n changes). Press `Save changes` to keep only the rows you want in the data. Note that this action cannot be undone. If you want these rows back you will have to reload the data through the Data > Manage page.
