Modify type, change, create, add, reorder, and remove variables in the data.

#### Type

When you select Type from the 'Transformation type' drop-down another drop-down menu is shown that will allow you to change the type or class of one or more variables in your data. For example, to change a variable of type factor or character to a variable of type date. Click the 'Save changes' button to change variable(s) in the data set. A description of the type transformations included in Radiant is provided below.

1. As factor: convert a variable to type factor (i.e., as.factor(x))
2. As number: convert a variable to type numeric (i.e., as.numeric(x))
3. As integer: convert a variable to type integer (i.e., as.integer(x))
4. As character: convert a variable to type character (i.e., as.character(x))
5. As date (mdy): R will, by default, read dates as factors. Use this function if the dates are ordered as month-day-year
6. As date (dmy): R will, by default, read dates as factors. Use this function if the dates are ordered as day-month-year
7. As date (ymd): R will, by default, read dates as factors. Use this function if the dates are ordered as year-month-day

#### Change

When you select Change from the 'Transformation type' drop-down another drop-down menu is shown that will allow you do apply common transformations to one or more variables in your data. For example, to take the (natural) log of a variable select the variable you want to change and choose Log from the 'Apply function' menu. A new variable is created with the prefix 'log.'. Click the 'Save changes' button to add the variable(s) to the data set. A description of the transformation functions included in Radiant is provided below.

1. Log: create a log-transformed version of the selected variable (i.e., log(x))
2. Square: multiply a variable by itself (i.e. x^2) 
3. Square-root: take the square-root of a variable (i.e., x^.5)
4. Center: create a new variable with a mean equal to zero (i.e., x - mean(x))
5. Standardize: create a new variable with a mean equal to zero and standard deviation equal to 1 (i.e., (x - mean(x)/sd(x)))
6. Invert: 1/x
7. Median split: create a new factor with two levels (Above and Below) that splits the variable values at the median
8. Deciles: create a new factor with 10 levels (deciles) that splits the variable values at the 10th, 20th, ..., 90th percentiles.

#### Create

Choose Create from the 'Transformation type' drop-down. This is the most flexible command to create new or transformed variables. However, it also requires some knowledge or R syntax. A new variable can be any function of other variables in the data. Some examples are given below. In each example the name to the left of the '=' sign is the name of the new variable. On the right of the '=' sign you can include other variable names and basic R functions. After you have typed the command press return to create the new variable and press 'Save changes' to add it to the data.

1. Create a new variable z that is the difference between variables x and y in the data

	z = x - y

2. Create a new variable z that is a transformation of variable x but with mean equal to zero (note that this transformation is available in the Change drop-down):

	z = x - mean(x)

3. Create a new factor z that takes on the value TRUE when x > y and FALSE otherwise

	z = x > y

4. Create a new factor z that takes on the value TRUE when x is equal to y and FALSE otherwise

	z = x == y

5. Create a variable z that is equal to x lagged by 3 periods

	z = shift(x,3)

#### Clipboard

It is possible to manipulate your data in Excel and copy-and-paste a new variable back into R. If you do not have the original data in Excel format use the 'clipboard' feature in Data > Manage to save the data to the clipboard so you can paste it into Excel. Apply your transformations in Excel and then copy the new variable, with a header label, to the clipboard from Excel (i.e, CTRL-C on Windows and CMD-C on Mac). Select 'Clipboard' from the 'Transformation type' dropdown and paste your new data into the 'Paste from Excel' box shown. It is key that the number of observations for the new variable is the same as in the original data. The new variable will be  shown on screen. To add the variable to the data click 'Save changes'.

#### Recode

To use the recode feature select the variable you want to change and choose 'Recode' from the 'Transformation type' dropdown. Provide one or more recode commands (separate the commands by a ';') and press return to see the newly created variable. Click 'Save changes' to add the new variable to the data. Some recode command examples are given below.

1. All values below 20 are set to 'Low' and all others to 'High'

	lo:20 = 'High'; else = 'High'

2. Values 1 through 12 are set to 'A', 13:24 to 'B', and the remainder to 'C'

	1:12 = 'A'; 13:24 = 'B'; else = 'C'

3.	The transformation commands used for the Tulsa-Age cross-tab:

	'<25' = '<35'; '25-34' = '<35'; '35-44' = '35-54'; '45-54' = '35-54'; '55-64' = '>54'; '>64' = '>54'

4. To exclude a particular value (e.g., an outlier in the data) from subsequent analyses we can recode it to a missing value. If we want to remove the maximum value from a variable called __sales__ that is equal to 400 we would (1) select the variable __sales__ in the 'Select column(s)' box and enter the command below in the 'Recode box'. Press return and 'Save changes' to add the recoded variable to the data. 

	400 = NA

#### Rename

Choose 'Rename' from the 'Transformation type' dropdown, select one or more variables and enter new names for them in the rename box shown. Separate each name by a ','. Press return to see the variables with their new names on screen and  press 'Save changes' to alter the variable names in the original data.

#### Remove columns

Choose 'Remove columns' from the 'Transformation type' dropdown and select one or more variables to remove. Press 'Save changes' to remove the variables from the original data. Note that this action cannot be undone. If you want to the original variables back you will have to reload the data through the Data > Manage page.

#### Remove missing

Choose 'Remove missing' from the 'Transformation type' dropdown to remove missing values. Press 'Save changes' to remove all rows with missing values from the data. If missing values were present you will see the number of observations in the data summary change (i.e., the value of n changes). Note that this action cannot be undone. If you want these rows back you will have to reload the data through the Data > Manage page.

#### Subset

Choose 'Subset' from the 'Transformation type' dropdown to select (or omit) specific sets of rows from the data. You can use > and < signs and even combine filter commands. For example, x > 3 & y == 2 would select only those rows for which the variable x has values larger than 3 __and__ for which y has values equal to 2. Type your filter statement in the Subset box and press enter to see the result on screen. You should see the number of observations in the data summary change (i.e., the value of n changes). Press 'Save changes' to keep only the rows you want in the data. Note that this action cannot be undone. If you want these rows back you will have to reload the data through the Data > Manage page.

&copy; Vincent Nijs (2014) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a>
