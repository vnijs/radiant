---
title: Data > Explore
---

***

> Summarize and plot the data

Create various summary statistics for one or more variables in your data. The most powerful feature in Explore is that you can easy describe the data _by_ some other variable and create corresponding plots.

If we select price from the diamonds dataset we can see the number of observations (n), the mean, the median, etc. etc. However,  the mean price per clarity level of the diamond can also be easily provided by choosing clarity as the Group by variable.

#### Subset

Choose 'Subset' from the 'Transformation type' dropdown to select (or omit) specific sets of rows from the data. You can use > and < signs and even combine filter commands. For example, x > 3 & y == 2 would select only those rows for which the variable x has values larger than 3 __and__ for which y has values equal to 2. Type your filter statement in the Subset box and press enter to see the result on screen. You should see the number of observations in the data summary change (i.e., the value of n changes).
