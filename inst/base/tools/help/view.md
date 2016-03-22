> Show data in table form

### Datasets

Choose one of the datasets from the `Datasets` dropdown. Files are loaded into Radiant through the Manage tab.

### Filter

There are several ways to select a subset of the data to view. The `Filter` box on the left (click the check-box first) can be used with `>` and `<` signs and you can also combine subset commands. For example, `x > 3 & y == 2` would show only those rows for which the variable `x` has values larger than 3 **AND** for which `y` has values equal to 2. Note that in R, and most other programming languages, `=` is used to _assign_ a value and `==` to evaluate if the value of a variable is exactly equal to some other value. In contrast `!=` is used to determine if a variable is _unequal_ to some value. You can also use expressions that have an **OR** condition. For example, to determine when `Salary` is smaller than \$100,000 **OR** larger than \$20,000 use `Salary > 20000 | Salary < 100000`. `|` is the symbol for **OR** and `&` is the symbol for **AND**

It is also possible to filter using dates. For example, to choose only those dates before June 1st, 2014 enter `date < "2014-6-1"` into the filter box and press return.

You can also use string matching to select rows. For example, type `grepl("ood", cut)` to select rows with `Good` or `Very good` cut. This search is case sensitive by default. For case insensitive search you would use `grepl("GOOD", cut, ignore.case = TRUE)`. Type your statement in the `Filter`  box and press return to see the result on screen or an error below the box if the expression is invalid.

It is important to note that these filters are _persistent_. A filter entered in one of the Data-tabs will also be applied to other tabs and to any analysis conducted through the Radiant menus. To deactivate a filter un-check the `Filter` check-box. To remove a filter simply erase it.

<table class='table table-condensed table-hover' style='width:60%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> Operator </th>
   <th style="text-align:left;"> Description </th>
   <th style="text-align:left;"> Example </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> `<` </td>
   <td style="text-align:left;"> less than </td>
   <td style="text-align:left;"> `price < 5000` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> `<=` </td>
   <td style="text-align:left;"> less than or equal to </td>
   <td style="text-align:left;"> `carat <= 2` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> `>` </td>
   <td style="text-align:left;"> greater than </td>
   <td style="text-align:left;"> `price > 1000` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> `>=` </td>
   <td style="text-align:left;"> greater than or equal to </td>
   <td style="text-align:left;"> `carat >= 2` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> `==` </td>
   <td style="text-align:left;"> exactly equal to </td>
   <td style="text-align:left;"> `cut == 'Fair'` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> `!=` </td>
   <td style="text-align:left;"> not equal to </td>
   <td style="text-align:left;"> `cut != 'Fair'` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> `|` </td>
   <td style="text-align:left;"> x OR y </td>
   <td style="text-align:left;"> `price > 10000 | cut == 'Premium'` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> `&` </td>
   <td style="text-align:left;"> x AND y </td>
   <td style="text-align:left;"> `carat < 2 & cut == 'Fair'` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> `%in%` </td>
   <td style="text-align:left;"> x is one of y </td>
   <td style="text-align:left;"> `cut %in% c('Fair', 'Good')` </td>
  </tr>
</tbody>
</table>

### Pause view

For larger datasets it can useful to click `Pause view` before selecting variables, entering filters, etc. When you are ready to generate the view table make sure that `Pause view` is not checked. When `Pause view` is not checked, any changes to the inputs will automatically result in a new table.

### Select columns

By default all columns in the data are shown. Click on any variable to focus on it alone. To select several variables use the SHIFT and ARROW keys on your keyboard. On a mac the CMD key can also be used to select multiple variables. The same effect is achieved on windows using the CTRL key. To select all variable use CTRL-A (or CMD-A on mac).

### Browse the data

By default only 10 rows of are shown at one time. You can change this setting through the `Show ... entries` dropdown. Press the `Next` and `Previous` buttons at the bottom-right of the screen to navigate through the data.

### Sort

Click on a column header in the table to sort the data by the values of that variable. Clicking again will toggle between sorting in ascending and descending order. To sort on multiple columns at once press shift and then click on the 2nd, 3rd, etc. column to sort by.

### Column filters and Search

For variables that have a limited number of different values (i.e., a factor) you can select the levels to keep from the column filter below the variable name. For example, to filter on rows with ideal cut click in the box below the `cut` column header and select `Ideal` from the dropdown menu shown. You can also type a string into these column filters followed by return. Note that matching is case-insensitive. In fact, typing `eal` would produce the same result because the search will match any part of a string. Similarly, you can type a string to select observations for character variables (e.g., street names).

For numeric variables the column filter boxes have some special features that make them almost as powerful as the `Filter` box. For numerical and integer variables you can use `...` to indicate a range. For example, to select `Price` values between \$500 and \$2000 dollars type `500 ... 2000` and press return. The range is inclusive of the values typed. Furthermore, if we want to filter on `carat` `0.32 ...` will show only diamonds with carat values larger than or equal to 0.32. Numeric variables also have a slider that you can use to define the range of values to keep.

If you want to get _really_ fancy you can use the search box on the top right to search across all columns in the data using **regular expressions**. For example, to find all rows that have an entry in _any_ column ending with the number 72 type `72$` (i.e., the `$` sign is used to indicate the end of an entry). For all rows with entries that start with 60 use `^60` (i.e., the `^` is used to indicate the first character in an entry). Regular expressions are incredibly powerful for search but this is a _big_ topic area. To learn more about regular expressions see this <a href="http://www.regular-expressions.info/tutorial.html" target="_blank">tutorial</a>.

### Store filtered data

It is important to note that column sorting, column filters, and search are **not** persistent. To store these settings for use in other parts of Radiant press the `Store` button. You can store the data and settings under a different dataset name by changing the value in the text input to the left of the `Store` button. This feature can also be used to select a subset of variables to keep. Just select the ones you want to keep and press the `Store` button. For more control over the variables you want to keep or remove and to specify their order in the dataset use the `Data > Transform` tab.
