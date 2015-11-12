> Simulation for decision analysis

Start by selecting the types of variables to use in the analysis from the `Select types` dropdown. Available types include Constant, Binomial, Discrete, Normal, and Uniform.

### Constant

List the constants to include in the analysis in the `Constant variables` input. You can either enter names and values directly into the text area (e.g., "cost 3") or enter a name ("cost") and a value (5) in the `Name` and `Value` input respectively and then press the <i title='Add variable' href='#' class='fa fa-plus-circle'></i> icon. Press the <i title='Remove variable' href='#' class='fa fa-plus-circle'></i> icon to remove a variable. Note that only variables listed in the (larger) text-input boxes will be included in the simulation.

### Binomial

Add random variables with a binomial distribution using the `Binomial variables` inputs. Start by specifying a `Name` ("crash"), the number of trials (n) (e.g., 20) and the probability (p) of a "success" (.01). Then press the <i title='Add variable' href='#' class='fa fa-plus-circle'></i> icon. Alternatively, enter (or remove) input directly in text area (e.g., crash 20 .01).

### Discrete

Define random variables with a discrete distribution using the `Discrete variables` inputs. Start by specifying a `Name` ("price"), the values (6 8), and their associated probabilities (.3 .7). Then press the <i title='Add variable' href='#' class='fa fa-plus-circle'></i> icon. Alternatively, enter (or remove) input directly in text area (e.g., price 6 8 .3 .7). Note that the probabilities must sum to 1. If not, a message will be displayed and the simulation cannot be run.

### Normal

To include normally distributed random variables in the analysis select `Normal` from the `Select types` dropdown and use `Normal variables` inputs. For example, enter a `Name` ("demand"), the `Mean` (1000) and the standard deviation (`St.dev.`, 100). Then press the <i title='Add variable' href='#' class='fa fa-plus-circle'></i> icon. Alternatively, enter (or remove) input directly in text area (e.g., "demand 1000 2000").

### Uniform

To include uniformly distributed random variables in the analysis select `Uniform` from the `Select types` dropdown. Provide parameters in the `Uniform variables` inputs. For example, enter a `Name` ("cost"), the `Min` (10) and the `Max` (15) value. Then press the <i title='Add variable' href='#' class='fa fa-plus-circle'></i> icon. Alternatively, enter (or remove) input directly in text area (e.g., "cost 10 15").

### Sequence

To include a sequence of values select `Sequence` from the `Select types` dropdown. Provide the minimum and maximum values in the `Sequence variables` inputs. For example, enter a `Name` ("trend"), the `Min` (1) and the `Max` (1000) value. Note that the number of 'steps' is determined by the number of simulations. Then press the <i title='Add variable' href='#' class='fa fa-plus-circle'></i> icon. Alternatively, enter (or remove) input directly in text area (e.g., "trend 1 1000").

### Formula

To perform a calculation using the generated variables create a formula in the `Formula` input box in the main panel (e.g., "profit = demand * (price - cost)"). Note that you can enter multiple formulas. If, for example, you would also like to calculate the margin in each simulation press `return` (or use `;`) after the first formula and type "margin = price - cost".

### Running the simulation

The value shown in the `# sims` input determines the number of simulation _draws_. To rerun a simulation with the same randomly generated values, specify a number in the `Set random seed` input (e.g., 1234).

To save the simulated data for further analysis specify a name in the `Simulated data` input box. You can then investigate the simulated data by choosing the data with the specified name from the `Datasets` dropdown in any of the _Data_ tabs (e.g., _Data > View_, _Data > Visualize_, or _Data > Explore_).

When all required inputs have been specified press the `Simulate` button to run the simulation.

In the screen shot below `var_cost` and `fixed_cost` are specified as constants. `demand` is normally distributed with a mean of 1000 and a standard deviation of 100. `price` is a discrete random variable that is set to $5 (30% probability) or $8 (70% probability). There are two formulas in the `Simulation formulas` text-input. The first updates the demand variable to ensure the demand level is a function of price. The second formula specifies the profit function.

![sim output](figures_quant/simulater.png)

Because we specified a name in the `Simulated data` box the data are available as `simdat` within Radiant (see screen shots below). To use the data in Excel click the download icon on the top-right of the screen in the _Data > View_ tab or go to the _Data > Manage_ tab and save the data to a csv file (or use the clipboard feature). For more information see the help file for the _Data > Manage_ tab.

![sim output](figures_quant/simulater_view.png)

The state file for the example in the screenshot above is available for download <a href="https://vnijs.github.io/radiant/examples/sim_help_state.rda">here</a>
