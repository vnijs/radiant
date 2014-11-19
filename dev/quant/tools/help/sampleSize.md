> Determine the required sample size to test a mean or proportion calculated from sample data

#### Example 1

We would like to start an ISP and need to estimate the average Internet usage of households in one week for our business plan and model. How many households must we randomly select to be 95 percent sure that the sample mean is within 1 minute of the population mean . Assume that a previous survey of household usage has shown  = 6.95 minutes.

**Answer:**

Since we are interested in estimating the mean internet usage in the population of interest we select sample size calculation for a  Mean. In the screen shot below we entered 1 (minute) as the Acceptable error and 6.95 as the initial estimate of the Sample standard deviation. The Confidence level is 95% so we choose a z-value of 1.96.

![Example 1b](figures_quant/sampleSize_ex1a.png)

As you can see the required sample size is equal to 186, i.e., we need 186 valid responses from our target population to make an inference of the population average with the required Acceptable Error, Confidence level, and Sample standard deviation. This number assumes a Incidence and Response rate of 100%. Suppose that only 75% of the household in out market have access to internet at their home. In addition the anticipated response rate is 20%. What would be the required number of household to contact?

![Example 1a](figures_quant/sampleSize_ex1b.png)

In the screen shot above the incidence rate is set to 75% and response rate to 20%. The required number of valid responses is the same as before (186), however the number of contact requests is now equal to 186 / .75 / .2 = 1240.

<a href="http://www.isixsigma.com/tools-templates/sampling-data/how-determine-sample-size-determining-sample-size/" target="_blank">Source</a>

#### Example 2

Suppose that you wish to investigate whether or not the true prevalence of HIV antibody in a population is 10%. You plan to take a random sample of the population to estimate the prevalence. You would like 95% confidence that the true proportion in the population will fall within the error bounds calculated from your sample.

Let's say that the population size is 5000, the intial estimate of the prevalence is 10% with an acceptable error of 4%.

Population Value = 5000
Expected proportion = 10%
Acceptable error = 4%

**Answer:**

Because we want to estimate the prevalence of HIV antibodies in the population of interest we select sample size calculation for a Proportion. In the screen shot below we entered 4% as the Acceptable error and 10% as the initial estimate of the proportion (p). The Confidence level is 95% so we choose a z-value of 1.96.

![Example 2a](figures_quant/sampleSize_ex2a.png)

As you can see the required sample size is equal to 217, i.e., we need 217 valid responses from our target population to make an inference of the population average with the required Acceptable Error and Confidence level. This number again assumes an Incidence and Response rate of 100%. The example suggests the population of interest has only 5000 people so it may be worthwhile to apply a correction for population size.

![Example 2b](figures_quant/sampleSize_ex2b.png)

In the screen shot above we clicked 'Yes' to apply the population correction and then entered 5000 as the population size. The required number of valid responses drops only slightly from 217 to 208.

<a href="http://bphc.hrsa.gov/policiesregulations/performancemeasures/patientsurvey/calculating.html" target="_blank">Source</a>

&copy; Vincent Nijs (2014) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="imgs/80x15.png" /></a>
