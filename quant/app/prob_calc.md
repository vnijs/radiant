> Probability calculator

Calculate probabilities or values based on the _Binomial_, _Chi-squared_, _Discrete_, _F_, _Normal_, _t_, or _Uniform_ distribution.

## Testing batteries

Suppose a consumer reports (CR) wants to test manufacturer claims about battery life. The manufacturer claims that more than 90% of their batteries will power a laptop for at least 12 hours of continues use. CR sets up 20 identical laptops with the manufacturer's batteries. If the manufacturer's claims are accurate, what is the probability that 15 or more laptops are still running after 12 hours?

The description of the problem suggests we should select `Binomial` from the `Distribution` dropdown. To find the probability, select `Values` as the `Input type` and enter `15` as the `Upper bound`. In the output below we can see that the probability is 0.989. The probability that exactly 15 laptops are still running after 12 hours is 0.032.

![Prob-calc - batteries](figures_quant/prob_calc_batteries.png)

## Demand for headphones

A manufacturer wants to determine the appropriate inventory level for headphones required to achieve a 95% <a href="https://en.wikipedia.org/wiki/Service_level" target="_blank">service level</a>. Demand for the headphones obeys a normal distribution with a mean of 5000 and a standard deviation of 800.

First select `Normal` from the `Distribution` dropdown. To find the required number of headphones to hold in inventory choose `Normal` from the `Distribution` dropdown and then select `Probability` as the `Input type`. Finally, enter `.95` as the `Upper bound`. In the output below we see the number of units to stock is 4316.

![Prob-calc - headphones](figures_quant/prob_calc_headphones.png)

## Hypothesis testing

You can also use the probability calculator to determine a `p.value` or a `critical value` for a statistical test. See the helpfiles for `Single mean`, `Single proportion`, `Compare means`, `Compare proportions`, `Cross-tabs` in the `Base` menu and `Linear (OLS)` in the `Regression` menu for details.
