> Show data in table form

#### Datasets

Choose one of the datasets from the drop-down menu. Files were loaded into Radiant through the Manage tab.

#### Select columns

By default all columns in the data are shown. Click on any variable to focus on it alone. To select several variables use the SHIFT and ARROW keys on your keyboard. On a mac the CMD key can also be used to select multiple variables. The same effect is achieved on windows using the CTRL key. To select all variable use CTRL-A (or CMD-A on mac).

#### Browse the data

By default only 10 rows of are shown. You can change this setting through the `records per page` drop-down. Press the `Next` and `Previous` buttons at the bottom-right of the screen to _scroll_ through the data.

#### Sort

Click on a column header in the table to sort the data by the values of that variable. Clicking again will toggle between sorting in ascending and descending order. To sort on multiple columns at once press shift and then click on the 2nd, 3rd, etc. column to sort by.

#### Filter

There are several ways to select a subset of the data to view. The `Filter` box on the left (click the checkbox first) can be used with `>` and `<` signs and you can also combine subset commands. For example, `x > 3 & y == 2` would show only those rows for which the variable `x` has values larger than 3 **and** for which `y` has values equal to 2. Note that in R `=` is used to _assign_ a value and `==` to evaluate if the value of a variable is equal to some other value. In contrast `!=` is used to determine if a variable is _unequal_ to some value. You can also use expressions that have an **or** condition. For example, to select rows where `Salary` is larger than $100,000 or smaller than $20,000 use `Salary < 20000 | Salary > 100000`. `|` is the symbol for **or**. The table below gives an overview of common operators. Type your statement in the `Filter`  box and press enter to see the result on screen or an error below the box if the expression is invalid. It is important to note that filters are _presistent_. A filter entered in any of the Data-tabs will also be applied to other tabs and to the analyses conducted through any of the other menus in Radiant. To remove a filter you have to (1) erase it and press enter or (2) uncheck the `Filter` checkbox.


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

### Search

For variables that have a limited number of different values (e.g., a factor) you can type a value in the text box below the column for one, or more, variables. For example, to filter on rows with Ideal cut, type `ideal` (or `Ideal`) in the box below that column. Matching in the filter fields is case-insensitive. In fact, typing `eal` would produce the same result because the search will match any part of a string.

The search boxes have some special features that make them almost as powerful as the `Filter` box. For numerical and integer variables you can use `,` to indicate range. For example, to select `Price` values between $500 and $2000 dollars type `500,2000`. The range is inclusive of the values typed. Further, `0.32,` will show only diamonds with carat values larger than or equal to 0.32.

If you want to get _really_ fancy you can use the search box on the top right to search across all columns using regular expressions. For example, to find all rows that have an entry in any column ending with the number 72 type `72$` (i.e., the `$` sign is used to indicate the end of an entry). For all rows with entries that start with 60 use `^60` (i.e., the `^` is used to indicate the first character in an entry). Regular expressions are incredibly powerful for search but this is a _big_ topic area. To learn the basics of regular expressions see this <a href="http://www.regular-expressions.info/tutorial.html" target="_blank">tutorial</a>.
