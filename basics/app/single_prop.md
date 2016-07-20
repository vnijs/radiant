> Compare a single proportion to the population proportion

The single proportion (or one-sample) binomial test is used to compare a proportion of responses or values in a sample of data to a (hypothesized) proportion in the population from which our sample data are drawn. This is important because we seldom have access to data for an entire population. The hypothesized value in the population is specified in the `Comparison value` box.

We can perform either a one-sided test (i.e., `less than` or `greater than`) or a two-sided test (see the `Alternative hypothesis` dropdown). We use one-sided tests to evaluate if the available data provide evidence that a sample proportion is larger (or smaller) than the comparison value (i.e., the population value in the null-hypothesis).

## Example

A car manufacturer conducted a study by randomly sampling and interviewing 1,000 consumers in a new target market. The goal of the study was to determine if consumers would consider purchasing this brand of car.

Management has already determined that the company will enter this segment. However, if brand preference is lower than 10% additional resources will be committed to advertising and sponsorship in an effort to enhance brand awareness amongst the target consumers. In the sample, 93 consumers exhibited what the company the company considered strong brand liking.

You can find information on the responses by survey participants in the **consider.rda** data set. The data set contains two variables, `id` and `consider`.

Our null-hypothesis is that the proportion of consumers that would consider the car brand for a future purchase is equal to 10%. Select the `consider` variable from the `Variable` dropdown. To evaluate the proportion of `yes` responses in the sample select `yes` from the `Choose level` dropdown.

Choose the `Less than` option from the `Alternative hypothesis` drop-down to determine if the available data provides sufficient evidence to reject the null-hypothesis in favor of the alternative that the proportion of consumers that will consider the brand is **less than 10%**.

<p align="center"><img src="figures_basics/single_prop_summary.png"></p>

The first two blocks of output show basic information about the test (e.g.,. the null and alternative hypothesis) and summary statistics (e.g., the proportion of "yes" responses). The final row of output shows the following:

* `diff` is the difference between the sample proportion (0.093) and the comparison value (0.1)
* `ns` is the number of _successes_. This is the number we can compare to a binomial-distribution with paramters $n = 1000$ and $p = 0.10$.
* `p.value` is the probability of finding a value as extreme or more extreme than `diff` if the null hypothesis is true
* `0% 95%` show the 95% confidence interval around the sample proportion (0 to 0.11). These numbers provide a range within which the true population mean is likely to fall

### Testing

There are three approaches we can use to evaluate the null hypothesis. We will choose a significance level of 0.05.<sup>1</sup> Of course, each approach will lead to the same conclusion.

#### p.value

Because the p.value is **larger** than the conventional significance level ($0.249 > 0.05$) we **cannot** reject the null hypothesis and **do not suggest** that management should commit resources to increase brand awareness.

We can also obtain the p.value by using the probability calculator in the _Basics_ menu. Enter the number of successes in the data (93) as the lower bound (value) for a binomial-distribution with $n = 1000$ and $p = 0.1$. The p.value is the probability of observing a number of successes as or more extreme than the 93 we got in our sample. We see that $P(X <= 93) = 0.249$ which is the same value we got from _Basics > Proportions > Single proportion_.

<p align="center"><img src="figures_basics/single_prop_prob_calc_v.png"></p>

#### confidence interval

Because the `comparison value` **is** contained in the confidence interval (i.e., $0 < 0.1 < 0.11$) we **cannot** reject the null hypothesis and **do not suggest** that management should commit resources to increase brand awareness.

#### number of successes

We can obtain the critical value by using the probability calculator in the _Basics_ menu. For a binomial-distribution with $n = 1000$ and $p = 0.1$ the critical value is 85. We have to enter 0.05 as the lower probability bound because the alternative hypothesis is `Less than`.<sup>2</sup>

<p align="center"><img src="figures_basics/single_prop_prob_calc_p.png"></p>

Because the number of successes (i.e., the number of "yes" responses) **is** larger than the critical value (93 vs 85) we **cannot** reject the null hypothesis and **do not suggest** that management should commit resources to increase brand awareness.

<!--
In addition to the numerical output provided in the _Summary_ tab we can visualize the data in the _Plot_ tab. The settings in the side-panel are the same as before. The black lines in the histogram show the sample mean (solid) and the confidence interval around the sample mean (dashed). The red line shows the comparison value (i.e., unit sales under the null-hypothesis). Because the red line does **not** fall within the confidence interval (1897 to Inf.) we reject the null-hypothesis in favor of the alternative.

<p align="center"><img src="figures_basics/single_proportion_plot.png"></p>
-->

### _Stats speak_

This is a **single proportion** test of the null hypothesis that the true population **proportion** is equal to **0.1**. Using a significance level of 0.05, we **cannot** reject the null hypothesis, and **cannot** conclude that the true population **proportion** is **less** than **0.1**.

The p.value for this test is **0.249**. This is the probability of observing a sample **proportion** (or **number of successes**) that is as or more extreme than the sample value we estimated from the data if the null hypothesis is true. In this case, it is the probability of observing a sample **proportion** (**number of successes**) that is less than (or equal to) **0.093** (**93**) if the true population **proportion** is **0.1**.

The 95% confidence bound is **0.11**. If repeated samples were taken and the 95% confidence bound computed for each one, the true population proportion would be below that bound in 95% of the samples

<sup>1</sup> The **significance level**, often denoted by $\alpha$, is the highest probability you are willing to accept of rejecting the null hypothesis when it is actually true. A commonly used significance level is 0.05 (or 5%)

<sup>2</sup> $1 - \alpha$ is called the **confidence level**. A commonly used confidence level is 0.95 (or 95%)
