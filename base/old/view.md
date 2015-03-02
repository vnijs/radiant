---
title: Data > View
---

***

> Show the available data in table form

### Datasets

Choose one of the datasets from the drop-down menu. These files were loaded into Radiant through the Manage tab.

### Select columns

By default all columns in the data are shown. Click on any variable to focus on it alone. To select several variables use the SHIFT and ARROW keys on your keyboard. On a mac the CMD key can also be used to select multiple variables. The same effect is achieved on windows using the CTRL key. To select all variable use CTRL-A (or CMD-A on Mac).

### Browse the data

By default only 10 data rows are shown. You can change this setting through the `records per page` drop-down. Press the `Next` and `Previous` buttons at the bottom-right of the screen to move through pages of the data.

### Sort

Click on a column header in the table to sort the data by the values of that variable. Clicking again will toggle between sorting in ascending and descending order. To sort on multiple columns at once press shift and then click on the 2nd, 3rd, etc. column you would like to sort by.

### Subset

There are several ways to select a subset of the data to view. The Subset box on the left can be used with > and < signs and you can also combine subset commands. For example, `x > 3 & y == 2` would show only those rows for which the variable x has values larger than 3 **and** for which y has values equal to 2. Note that in R `=` is used to _assign_ a value and `==` to evaluate if the value of a variable is equal to some other value. In constrast `!=` is used to determine if variable is _not equal_ to some value. You can also use expression that have an **or** condition. For example, to select rows where Salary is larger than $100,000 or smaller than $20,000 you could use `Salary < 20000 | Salary > 100000`. `|` is the symbol for **or**. The table below gives an overview of operators you can use. Type your statement in the Subset box and press enter to see the result on screen.

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

## Search

For variables that have a limited number of different values (e.g., a factor) you can type a value in the text box below the column for one, or more, variables. For example, to filter on rows with Ideal cut, type `ideal` in the box below that column. Matching in the filter fields is case-insensitive. In fact, just typing `eal` would produce the same result because the search will match any part of a string.

The search boxes have some special features that make them almost as powerful as the Subset box. For numerical or integer variables you can use `,` to indicate range. For example, to select Price values between $500 and $2000 dollars you would type `500,2000`. The range is inclusive of the values typed. The `,` can also be used as a substitute for > or < in the Subset box. For example, `0.32,` will show only diamonds with carat values larger than or equal to 0.32.

If you want to get _really_ fancy you can use the search box in the top right to search across all columns shown using regular expressions. For example, to find all rows that have an entry with the number 72 at the end in one or more of the columns type `72$` (i.e., the `$` sign is used to indicate the end of an entry). For all rows with entries that start with 10 use `^60` (i.e., the `^` is used to indicate the first character in an entry). Regular expressions are incredibly powerful for search but this is a _big_ topic area. To learn the basics of regular expressions see <http://www.regular-expressions.info/tutorial.html>.
