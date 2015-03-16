> Use simple random sampling to select respondents from a list

To use the sampling tool you will need a data set with a variable of type character. Entries should be unique (i.e., no duplicates). A dataset that fits these requirements is bundled with Radiant. On the Data > Manage tab click the examples radio button and then click `Load examples`. Select `rndnames` from the `Datasets` dropdown.

`Names` is the relevant column in this dataset and it will automatically be used as the base for sampling. Now simply choose the sample size you want and a list of names of the desired length will be created.

How does it work? Each person in the data is assigned a random number between 0 and 1 from a uniform distribution. Rows are then sorted on that random number and the $n$ people from the list with the highest score are selected for the sample. By using a random number every respondent has the same probability of being in the sample. For example, if we need a sample of 10 people from the 100 included in the rndnames dataset each individual has a 10% chances of being included in the sample.

![Summary](http://mostly-harmless.github.io/radiant/quant/figures_quant/sampling.png)

The full list of 100 people is the `sampling frame`. Ideally, this is a comprehensive list of _all_ sampling units (e.g., customers or companies) in your target market.

How do you determine the appropriate value for _n_? See the sample size tool in the `Sample` menu.
