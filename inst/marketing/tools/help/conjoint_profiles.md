> Generate an efficient set of product profiles for conjoint analysis

To setup a conjoint study from scratch we need to determine the attributes and attributes levels that should be included. Once that has been done we would typically need to generate a fractional factorial design of conjoint profiles. This is a subset of all possible profiles that could be generated for the set of attributes and levels that were selected.

Put the attrbutes and levels in a text file and load it into Radiant. An example of the required format is shown below. This information is available in file format [here](https://github.com/mostly-harmless/radiant/blob/master/examples/profiles-movie.txt)

```r
price = c("$10","$13","$16")
sight = c("Staggered","Not Staggered")
comfort = c("Average seat without cupholder","Average seat with cupholder","Large seat with cupholder")
audio.visual = c("Small screen with plain sound","Large screen with plain sound","Large screen with digital sound")
food = c("No food","Hot dogs and popcorn","Gourmet food")
```

&copy; Vincent Nijs (2015) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="imgs/80x15.png" /></a>
