> Determine the required sample size for comparisons of means or proportions

Leave one of the inputs blank to determine its value. By default the sample size input is left empty and the required sample size is calculated. Note that only one input can be left blank.

### Input

* Sample size: Number of respondents required
* Confidence level: 1 - significance level (e.g, .95 = 1 - .05).<sup>1, 2</sup>
* Power: 1 - $\beta$ (e.g, .8 = 1 - .2).<sup>3</sup>
* Ratio: Relative sample size for group 1 (control) and group 2 (test)

### Input for a comparison of means

* Delta: Difference between group means that we hope to detect
* Std. deviation: Assumed standard deviation

### Input for a comparison of proportions

* Proportion 1: Assumed proportion in group 1 (e.g., .1)
* Proportion 2: Proportion 1 plus the difference we hope to detect (e.g., .1 + .05 = .15)

**Note:** The (matching) ratio of the control group sample size (_n1_) to the test group sample size (_n2_) is 1 by default (i.e., groups are of the same size). If no value is provided, a value of 1 is assumed.

<sup>1</sup> The **significance level**, often denoted by $\alpha$, is the highest probability you are willing to accept of rejecting the null hypothesis when it is actually true. A commonly used significance level is 0.05 (or 5%)

<sup>2</sup> $1 - \alpha$ is called the **confidence level**. A commonly used confidence level is 0.95 (or 95%)

<sup>3</sup> Beta ($\beta$), is the probability of accepting the null hypothesis when it is actually false. The power of a test is calculated as 1 - $\beta$. A commonly used power level is 0.8 (or 80%)
