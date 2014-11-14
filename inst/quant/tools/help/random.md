> Use simple random sampling to select respondents from a list

To use the sampling tool you will need a data set with a variable of type character. Entries should be unique (i.e., no duplicates). A dataset that fits this requirement is included with Radiant. On the Data > Manage tab click the examples radio button and then click `Load examples`. Select `rndnames` from the datasets dropdown. 

`Names` is the relevant column in this dataset and it will automatically be used as the base for sampling. Now simply choose the sample size you want and a list of names of the desired length will be created.

How does it work? Each person in the data is assigned a random number between 0 and 1 from a uniform distribution. Rows are then sorted on that random number and the $n$ people from the list with the highest score are selected for the sample. By using a random number every respondent has the same probability of being in the sample. For example, if we need a sample of 10 people from the 100 included in the rndnames dataset each individual has a 10% chances of being included in the sample. 

The full list of 100 people used here is what we would call the `sampling frame`. Ideally, this is a comprehensive list of all people or companies in the target market of interest. 

How do you determine the appropriate value for $n$? See the sample size tool in the Random menu.


&copy; Vincent Nijs (2014) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a>
