> Evaluate model performance using Lift and Gains charts

To download the table as a csv-files click on the top download button on the right of your screen. To download the plots at a png file click the lower download icon on the right of your screen.

#### Response variable

The outcome, or response, variable of interest. This should be binary variable, either a factor or an integer.

#### Choose level

The level in the response variable that is considered a _success_. For example, purchase or buyer is equal to "yes".

#### Predictor

Select one or more variables that can be used to _predict_ the chosen level in the response variable. This could be a variable, an RFM index, or predicted values from a model (e.g., a logistic regression estimated using _Regression > GLM_).

#### # quantiles

The number of buckets generate.

#### # quantiles

The number of buckets generate by `method`.

#### Show results for

If a `filter` is active (e.g., set in the _Data > View_ tab) generate results for `All` data, `Training` data, `Validation` data, or `Both` training and validation data. If no filter is active calculations are applied to all data.

#### Plots

Generate a Lift chart, a Gains chart, or both.
