> Probability calculator

Calculate probabilities or values based on the _Binomial_, _Chi-squared_, _Discrete_, _F_, _Normal_, _t_, or _Uniform_ distribution.

## Testing batteries

Suppose a consumer reports (CR) wants to test manufacturer claims about battery life. The manufacturer claims that more than 90% of their batteries will power a laptop for at least 12 hours of continues use. CR sets up 20 identical laptops with the manufacturer's batteries. If the manufacturer's claims are accurate, what is the probability that 15 or more laptops are still running after 12 hours?

The description of the problem suggests we should select `Binomial` from the `Distribution` dropdown. To find the probability, select `Values` as the `Input type` and enter `15` as the `Upper bound`. In the output below we can see that the probability is 0.989. The probability that exactly 15 laptops are still running after 12 hours is 0.032.

![Prob-calc - batteries](figures_quant/prob_calc_batteries.png)

## Demand for headphones

A manufacturer wants to determine the appropriate inventory level for headphones required to achieve a 95% <a href="https://en.wikipedia.org/wiki/Service_level" target="_blank">service level</a>. Demand for the headphones obeys a normal distribution with a mean of 3000 and a standard deviation of 800.

First select `Normal` from the `Distribution` dropdown. To find the required number of headphones to hold in inventory choose `Normal` from the `Distribution` dropdown and then select `Probability` as the `Input type`. Finally, enter `.95` as the `Upper bound`. In the output below we see the number of units to stock is 4316.

![Prob-calc - headphones](figures_quant/prob_calc_headphones.png)

## Cups of ice cream

A **discrete** random variable can take on a limited (finite) number of possible values. The **probability distribution** of a discrete random variable lists these values and their probabilities. For example, the number of cups of ice cream a customer buys could be described as follows:

* 40% of customers buy 1 cup;
* 30% of customers buy 2 cups;
* 20% of customers buy 3 cups;
* 10% of customers buy 4 cups.

The number of cups a customer buys is a (discrete) random variable and the assigned probabilities describe the probability distribution. We can use the probability distribution of a random variable to calculate its **mean** or **expected value** as follows;

$$
  E(C) = \mu_C = 1 \times 0.40 + 2 \times 0.30 + 3 \times 0.20 + 4 \times 0.10 = 2\,,
$$

where $C$ is the number of cups purchased and $\mu_C$ is the mean number of cups purchased. This means that we can _expect_ a customer to buy 2 cups. The variance of Iceskimo customer consumption is calculated as follow:

$$
  Var(C) = (1 - 2)^2 \times 0.4 + (2 - 2)^2 \times 0.3 + (3 - 2)^2 \times 0.2 + (4 - 2)^2 \times 0.1 = 1\,.
$$

To get the mean and standard deviation of the discrete probability distribution above, as well as the probability a customer will buy 2 or more cups (0.6), specify the following in the probability calculator.

![Prob-calc - icecream](figures_quant/prob_calc_icecream.png)

## Hypothesis testing

You can also use the probability calculator to determine a `p.value` or a `critical value` for a statistical test. See the helpfiles for `Single mean`, `Single proportion`, `Compare means`, `Compare proportions`, `Cross-tabs` in the `Base` menu and `Linear regression (OLS)` in the _Regression_ menu for details.
