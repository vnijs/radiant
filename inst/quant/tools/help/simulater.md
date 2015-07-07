> Simulate data for decision analysis

Select the types of variables to use in the analysis from the `Select types` dropdown.

List the constants to include in the analysis in the `Constant variables` text area (e.g., "cost 3"). If there are multiple variables to include simply press `return` on your keyboard and write out the information for the next variable. You can also separate constants using a `;`.

List the uniformly distributed random variables to include in the analysis in the `Uniform variables` text area (e.g., "demand 1000 2000"). Note that the first number is the **minimum** and the second is the **maximum** value.

As for constant variables, if there are multiple uniformly distributed variables to include simply press `return` (or use `;`) and write out the information for the next variable.

List the normally distributed random variables in the `Normal variables` text area (e.g., "demand 2000 1000"). Note that the first number is the **mean** and the second the **standard deviation**.

List the random variables with a discrete distribution in the `Discrete variables` text area (e.g., "price 5 .3 8 .7)" where **for each pair of numbers the first is the value and the second the probability**). Note that the probabilities should sum to 1.

To perform a calculation using the variables specified in the various text input boxes create a formula in the `Formula` input box (e.g., "profit = demand * (price - cost)"). Note that you can enter multiple formulas. If, for example, you would also like to calculate the margin for each simulation press `return` (or use `;`) after the first formula and type "margin = price - cost".

The value shown in the `# runs` input determines the number of simulation runs that will be used. To repeat a simulation with the same randomly generated values enter a number into `Random seed` input.

To save the simulated data for further analysis in Radiant specify a name in the `Sim name` input box. You can then investigate the simulated data by choosing the specified name from the `Datasets` dropdown in any of the other Data tabs.

When all desired inputs have been specified press the `Simulate` button to generate output.

In the screen shot shown below `var_cost` and `fixed_cost` are specified as constants. `demand` is a normally distributed with a mean of 1000 and a standard deviation of 100. `price` is a discrete random variable that is set to $5 (30% probability) or $8 (70% probability). There are two formulas. The first updates the demand variable and specifies how demand levels change as a function of price. The second formula specifies the profit function.

![sim output](figures_quant/simulater.png)

Because we specified a name in the `Sim name` box the data are available as `sim1` within Radiant (see screen shots below). To use the data in Excel go to the Manage tab and save the data to a csv file or use the clipboard feature. For more information see the help file for the Manage tab.

![sim output](figures_quant/simulater_view.png)
![sim output](figures_quant/simulater_viz.png)

The state file for the example above is available for download <a href="https://github.com/vnijs/radiant/blob/master/inst/examples/sim_state.rda?raw=true">here</a>
