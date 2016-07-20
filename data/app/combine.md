> Combine two datasets

There are six _join_ (or _merge_) options available in Radiant from the [dplyr](http://www.rdocumentation.org/packages/dplyr) package developed by Hadley Wickham and Romain Francois.

The examples below are adapted from the [Cheatsheet for dplyr join functions](http://stat545-ubc.github.io/bit001_dplyr-cheatsheet.html) by [Jenny Bryan](http://www.stat.ubc.ca/~jenny/) and focus on three small datasets, `superheroes`, `publishers`, and `avengers`, to illustrate the different _join_ types and other ways to combine datasets in R and Radiant. The data are also available in csv format through the links below:

* <a href="https://github.com/radiant-rstats/docs/blob/gh-pages/examples/superheroes.csv" target = "_blank">superheroes.csv</a>
* <a href="https://github.com/radiant-rstats/docs/blob/gh-pages/examples/publishers.csv" target = "_blank">publishers.csv</a>
* <a href="https://github.com/radiant-rstats/docs/blob/gh-pages/examples/avengers.csv" target = "_blank">avengers.csv</a>

<table class='table table-condensed table-hover' style='width:70%;'>
<caption>Superheroes</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hellboy </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Dark Horse Comics </td>
  </tr>
</tbody>
</table>

<table class='table table-condensed table-hover' style='width:30%;'>
<caption>Publishers</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> yr_founded </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Image </td>
   <td style="text-align:left;"> 1992 </td>
  </tr>
</tbody>
</table>

In the screen-shot of the _Data > Combine_ tab below we see the two datasets. The tables share the variable _publisher_ which is automatically selected for the join. Different join options are available from the `Combine type` dropdown. You can also specify a name for the combined dataset in the `Data name` text input box.

<p align="center"><img src="figures/join_superheroes_publishers.png"></p>

<br>

### Inner join (superheroes, publishers)

If x = superheroes and y = publishers:

> An inner join returns all rows from x with matching values in y, and all columns from both x and y. If there are multiple matches between x and y, all match combinations are returned.

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> yr_founded </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
</tbody>
</table>

In the table above we lose _Hellboy_ because, although this hero does appear in `superheroes`, the publisher (_Dark Horse Comics_) does not appear in `publishers`. The join result has all variables from `superheroes`, plus _yr\_founded_, from `publishers`. We can visualize an inner join with the venn-diagram below:

<p align="center"><img src="figures/inner_join.png"></p>

The R(adiant) commands are:

```r
# Radiant
combinedata("superheroes", "publishers", by = "publisher", type = "inner_join")

# R
inner_join(superheroes, publishers, by = "publisher")
```

<br>

### Left join (superheroes, publishers)

> A left join returns all rows from x, and all columns from x and y. If there are multiple matches between x and y, all match combinations are returned.

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> yr_founded </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hellboy </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Dark Horse Comics </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

The join result contains `superheroes` with variable `yr_founded` from `publishers`. _Hellboy_, whose publisher does not appear in `publishers`, has an `NA` for _yr_founded_. We can visualize a left join with the venn-diagram below:

<p align="center"><img src="figures/left_join.png"></p>

The R(adiant) commands are:

```r
# Radiant
combinedata("superheroes", "publishers", by = "publisher", type = "left_join")

# R
left_join(superheroes, publishers, by = "publisher")
```

<br>

### Right join (superheroes, publishers)

> A right join returns all rows from y, and all columns from y and x. If there are multiple matches between y and x, all match combinations are returned.

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> yr_founded </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Image </td>
   <td style="text-align:left;"> 1992 </td>
  </tr>
</tbody>
</table>

The join result contains all rows and columns from `publishers` and all variables from `superheroes`. We lose _Hellboy_, whose publisher does not appear in `publishers`. _Image_ is retained in the table but has `NA` values for the variables _name_, _alignment_, and _gender_ from `superheroes`. Notice that a join can change both the row and variable order so you should not rely on these in your analysis. We can visualize a right join with the venn-diagram below:

<p align="center"><img src="figures/right_join.png"></p>

The R(adiant) commands are:

```r
# Radiant
combinedata("superheroes", "publishers", by = "publisher", type = "right_join")

# R
right_join(superheroes, publishers, by = "publisher")
```

<br>

### Full join (superheroes, publishers)

> A full join combines two datasets, keeping rows and columns that appear in either.

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> yr_founded </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hellboy </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Dark Horse Comics </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Image </td>
   <td style="text-align:left;"> 1992 </td>
  </tr>
</tbody>
</table>

In this table we keep _Hellboy_ (even though _Dark Horse Comics_ is not in `publishers`) and _Image_ (even though the publisher is not listed in `superheroes`) and get variables from both datasets. Observations without a match are assigned the value NA for variables from the _other_ dataset. We can visualize a full join with the venn-diagram below:

<p align="center"><img src="figures/full_join.png"></p>

The R(adiant) commands are:

```r
# Radiant
combinedata("superheroes", "publishers", by = "publisher", type = "full_join")

# R
full_join(superheroes, publishers, by = "publisher")
```

### Semi join (superheroes, publishers)

> A semi join keeps only columns from x. Whereas an inner join will return one row of x for each matching row of y, a semi join will never duplicate rows of x.

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
</tbody>
</table>

We get a similar table as with `inner_join` but it contains only the variables in `superheroes`. The R(adiant) commands are:

```r
# Radiant
combinedata("superheroes", "publishers", by = "publisher", type = "semi_join")

# R
semi_join(superheroes, publishers, by = "publisher")
```

<br>

### Anti join (superheroes, publishers)

> An anti join returns all rows from x without matching values in y, keeping only columns from x

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Hellboy </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Dark Horse Comics </td>
  </tr>
</tbody>
</table>

We now get **only** _Hellboy_, the only superhero not in `publishers` and we do not get the variable _yr\_founded_ either. We can visualize an anti join with the venn-diagram below:

<p align="center"><img src="figures/anti_join.png"></p>

<br>

### Dataset order

Note that the order of the datasets selected may matter for a join. If we setup the _Data > Combine_ tab as below the results are as follows:

<p align="center"><img src="figures/join_publishers_superheroes.png"></p>

<br>

### Inner join (publishers, superheroes)

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> yr_founded </th>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
  </tr>
</tbody>
</table>

Every publisher that has a match in `superheroes` appears multiple times, once for each match. Apart from variable and row order, this is the same result we had for the inner join  shown above.

<br>

### Left and Right join (publishers, superheroes)

Apart from row and variable order, a left join of `publishers` and `superheroes` is equivalent to a right join of `superheroes` and `publishers`. Similarly, a right join of `publishers` and `superheroes` is equivalent to a left join of `superheroes` and `publishers`.

<br>

### Full join (publishers, superheroes)

As you might expect, apart from row and variable order, a full join of `publishers` and `superheroes` is equivalent to a full join of `superheroes` and `publishers`.

<br>

### Semi join (publishers, superheroes)

<table class='table table-condensed table-hover' style='width:30%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> yr_founded </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> 1939 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DC </td>
   <td style="text-align:left;"> 1934 </td>
  </tr>
</tbody>
</table>

With semi join the effect of switching the dataset order is more clear. Even though there are multiple matches for each publisher only one is shown. Contrast this with an inner join where "If there are multiple matches between x and y, all match combinations are returned." We see that publisher _Image_ is lost in the table because it is not in `superheroes`.

<br>

### Anti join (publishers, superheroes)

<table class='table table-condensed table-hover' style='width:30%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> yr_founded </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Image </td>
   <td style="text-align:left;"> 1992 </td>
  </tr>
</tbody>
</table>

Only publisher _Image_ is retained because both _Marvel_ and _DC_ are in `superheroes`. We keep only variables in `publishers`.

<br>

### Additional tools to combine datasets (avengers, superheroes)

When two datasets have the same columns (or rows) there are additional ways in which we can combine them into a new dataset. We have already used the `superheroes` dataset and will now try to combine it with the `avengers` data. These two datasets have the same number of rows and columns and the columns have the same names.

In the screen-shot of the _Data > Combine_ tab below we see the two datasets. There is no need to select variables to combine the datasets here. Any variables in `Select variables` are ignored in the commands below. Again, you can specify a name for the combined dataset in the `Data name` text input box.

<p align="center"><img src="figures/combine_avengers_superheroes.png"></p>

<br>

### Bind rows

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Thor </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Iron Man </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hulk </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hawkeye </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black Widow </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Captain America </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hellboy </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Dark Horse Comics </td>
  </tr>
</tbody>
</table>

If the `avengers` dataset were meant to extend the list of superheroes we could just stack the two datasets, one below the other. The new datasets has 14 rows and 4 columns. Due to a coding error in the `avengers` dataset (i.e.., _Magneto_ is *not* an _Avenger_) there is a duplicate row in the new combined dataset. Something we probably don't want.

The R(adiant) commands are:

```r
# Radiant
combinedata("avengers", "superheroes", type = "bind_rows")

# R
bind_rows(avengers, superheroes)
```

<br>

### Bind columns

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Thor </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Iron Man </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> Storm </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hulk </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> Mystique </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hawkeye </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> Batman </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black Widow </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> Joker </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Captain America </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> Catwoman </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
   <td style="text-align:left;"> Hellboy </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Dark Horse Comics </td>
  </tr>
</tbody>
</table>

If the dataset had different columns for the same superheroes we could combine the two datasets, side by side. In radiant you will see an error message if you try to bind these columns because they have the same name. Something that we should always avoid. The method can be useful if we *know* the order of the row ids of two dataset are the same but the columns are all different.

<br>

### Intersect

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>

  </tr>
</tbody>
</table>

A good way to check if two datasets with the same columns have duplicate rows is to choose `intersect` from the `Combine type` dropdown. There is indeed one row that is identical in the `avengers` and `superheroes` data (i.e., _Magneto_).

The R(adiant) commands are the same as shown above, except you will need to replace `bind_rows` by `intersect`.

<br>

### Union

<table class="kable_wrapper">
<tbody>
  <tr>
   <td> 

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>
   <td style="text-align:left;"> Thor </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Iron Man </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hulk </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hawkeye </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black Widow </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Captain America </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Magneto </td>
  </tr>
</tbody>
</table>

 </td>
   <td> 

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bad </td>
  </tr>
</tbody>
</table>

 </td>
   <td> 

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> female </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
</tbody>
</table>

 </td>
   <td> 

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
</tbody>
</table>

 </td>
   <td> 

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>
   <td style="text-align:left;"> Magneto </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Storm </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mystique </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Batman </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Joker </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Catwoman </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hellboy </td>
  </tr>
</tbody>
</table>

 </td>
   <td> 

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>
   <td style="text-align:left;"> bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> good </td>
  </tr>
</tbody>
</table>

 </td>
   <td> 

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> female </td>
  </tr>
  <tr>
   <td style="text-align:left;"> female </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
  <tr>
   <td style="text-align:left;"> female </td>
  </tr>
  <tr>
   <td style="text-align:left;"> male </td>
  </tr>
</tbody>
</table>

 </td>
   <td> 

<table class='table table-condensed table-hover' style='width:70%;'>
<tbody>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DC </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dark Horse Comics </td>
  </tr>
</tbody>
</table>

 </td>
  </tr>
</tbody>
</table>

A `union` of `avengers` and `superheroes` will combine the datasets but will omit duplicate rows (i.e., it will keep only one _copy_ of the row for _Magneto_). Likely what we want here.

The R(adiant) commands are the same as shown above, except you will need to replace `bind_rows` by `union`.

<br>

### Setdiff

<table class='table table-condensed table-hover' style='width:70%;'>
 <thead>
  <tr>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> alignment </th>
   <th style="text-align:left;"> gender </th>
   <th style="text-align:left;"> publisher </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Thor </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Iron Man </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hulk </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Hawkeye </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Black Widow </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> female </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Captain America </td>
   <td style="text-align:left;"> good </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Magneto </td>
   <td style="text-align:left;"> bad </td>
   <td style="text-align:left;"> male </td>
   <td style="text-align:left;"> Marvel </td>
  </tr>
</tbody>
</table>

Finally, a `setdiff` will keep rows from `avengers` that are _not_ in `superheroes`. If we reverse the inputs (i.e., choose `superheroes` from the `Datasets` dropdown and `superheroes` from the `Combine with` dropdown) we will end up with all rows from `superheroes` that are not in `avengers`. In both cases the entry for _Magneto_ will be omitted.

The R(adiant) commands are the same as shown above, except you will need to replace `bind_rows` by `setdiff`.

<br>

--------
For additional discussion see the chapter on relational data in <a href="http://r4ds.had.co.nz/relational-data.html" target="_blank">R for data science</a>.
