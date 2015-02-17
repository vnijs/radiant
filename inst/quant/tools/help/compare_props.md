> Compare proportions for two or more groups in the data

The the compare proportions test is used to evaluate the frequency of occurrence of some event differs across groups. We can perform either a one-tailed test (i.e., less than or greater than) or two-tailed test (see 'Alternative hypothesis'). We often use one-tailed tests because we want to evaluate if the available data provide evidence that a variable or effect is larger (or smaller) in one sample than another.

#### Example

We will use a dataset that describes the survival status of individual passengers on the Titanic. The titanic data frame does not contain information from the crew, but it does contain actual ages of (some of) the passengers. The principal source for data about Titanic passengers is the Encyclopedia Titanic. One of the original sources is Eaton & Haas (1994) Titanic: Triumph and Tragedy, Patrick Stephens Ltd, which includes a passenger list created by many researchers and edited by Michael A. Findlay.

- survived = a factor with levels `Yes` and `No`
- pclass = Passenger Class (1st, 2nd, 3rd). This is a proxy for socio-economic status (SES) 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower

Suppose we want to test if the proportion of people that survived the sinking of the Titanic differs by passenger class. To test this hypothesis we select `pclass` as the grouping variable select `survived` as variable for which we will calculate proportions. Radiant will do a pair-wise comparison of survival probabilities across the three levels of passenger class. Without an explicit hypothesis for the direction of the effect we can use a two-sided test.

![Summary](figures_quant/summary_compare_props.png)

Because the p-values are smaller than the conventional level of significance (i.e. < 0.05) for each pair-wise comparison we can reject the null hypothesis based on the available data. The results suggest that 1st class passengers were more likely to survive the sinking than either the 2nd or 3rd class passengers. In turn, the 2nd class passengers were more likely to survive than those in 3rd class.

In addition to the numerical output provided in the Summary tab we can also evaluate the hypothesis visually (see Plot tab). The settings in the side-panel are the same as before. The tab displays a bar chart of the survival proportion with confidence interval (black) and standard error (blue) bars. Consistent with the results shown in the Summary tab there is clear separation between the proportions across the levels of passenger class. We can also choose to plot the data as a simple bar-chart of the survival counts.

![Plot](figures_quant/plot_compare_props.png)

&copy; Vincent Nijs (2015) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="imgs/80x15.png" /></a>
