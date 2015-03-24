> Compare proportions for two or more groups in the data

The compare proportions test is used to evaluate if the frequency of occurrence of some event, behavior, intention, etc. differs across groups. We can perform either a one-tailed test (i.e., less-than or greater-than) or two-tailed test (see `Alternative hypothesis`). A one-tailed test is useful if we want to evaluate if the available data suggest that, for example, the proportion of dropped calls is larger (or smaller) for one wireless provider compared to others.

#### Example

We will use a dataset that describes the survival status of individual passengers on the Titanic. The principal source for data about Titanic passengers is the Encyclopedia Titanic. One of the original sources is Eaton & Haas (1994) Titanic: Triumph and Tragedy, Patrick Stephens Ltd, which includes a passenger list created by many researchers and edited by Michael A. Findlay. Lets focus on two variables in the database:

- survived = a factor with levels `Yes` and `No`
- pclass = Passenger Class (1st, 2nd, 3rd). This is a proxy for socio-economic status (SES) 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower

Suppose we want to test if the proportion of people that survived the sinking of the Titanic differs across passenger classes. To test this hypothesis we select `pclass` as the grouping variable and calculate proportions for `survived`. Radiant will do a pair-wise comparison of survival probabilities across the three passenger class levels. Unless we have an explicit hypothesis for the direction of the effect we should use a two-sided test. Our first alternative hypothesis would be 'The proportion of survivors is different for 1st vs 2nd class passengers'.

![Summary](http://vnijs.github.io/radiant/quant/figures_quant/compare_props_summary.png)

Because the p-values are smaller than the conventional level of significance (i.e. < 0.05) for each pair-wise comparison we can reject the null hypothesis that the proportions are equal based on the available data. The results suggest that 1st class passengers were more likely to survive the sinking than either the 2nd or 3rd class passengers. In turn, the 2nd class passengers were more likely to survive than those in 3rd class.

In addition to the numerical output provided in the Summary tab we can also evaluate the hypothesis visually (see Plot tab). The settings in the side-panel are the same as before. The tab displays a bar chart of the survival proportion with confidence interval (black) and standard error (blue) bars. Consistent with the results shown in the Summary tab there is clear separation between the proportions across passenger classes. We can also choose to plot the data using a bar-chart of the survival counts.

![Plot](http://vnijs.github.io/radiant/quant/figures_quant/compare_props_plot.png)
