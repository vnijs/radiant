> Generate a set of product profiles for conjoint analysis

To setup a conjoint study we first need to determine the attributes and attributes levels that should be included. Once that has been done we would typically generate a fractional factorial design of conjoint profiles. This is a subset of all possible profiles that could be generated for the set of attributes and levels that were selected.

Put the attributes and levels in a text file and load it into Radiant. An example of the required format is shown below. You can also download the input as a text file <a href="https://github.com/vnijs/radiant/blob/master/inst/examples/profiles-movie.txt" target="_blank">here</a>.

```r
price = c("$10","$13","$16")
sight = c("Staggered","Not Staggered")
comfort = c("Average seat, no cupholder","Average seat, cupholder","Large seat, cupholder")
audio.visual = c("Small screen, plain sound","Large screen, plain sound","Large screen, digital sound")
food = c("No food","Hot dogs and popcorn","Gourmet food")
```

The output from Radiant is shown below. Note that the full list of 162 profiles was omitted from the output for brevity.
The correlation matrix shown indicates that for any two attributes the correlations are zero. In the fractional factorial design, profiles are deliberately selected such that all attributes are uncorrelated (i.e., orthogonal).

![conjoint profiles](figures_marketing/conjoint_profiles.png)
