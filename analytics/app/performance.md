> Evaluate model performance using Lift and Gains charts

To download the table as a csv-files click on the top download button on the right of your screen. To download the plots at a png file click the lower download icon on the right of your screen.

#### Response variable

The outcome, or response, variable of interest. This should be binary variable, either a factor or an integer.

#### Choose level

The level in the response variable that is considered a _success_. For example, purchase or buyer is equal to "yes".

#### Predictor

Select one or more variables that can be used to _predict_ the chosen level in the response variable. This could be a variable, an RFM index, or predicted values from a model (e.g., a logistic regression estimated using _Regression > GLM_).

#### Plots

Generate a Lift chart, a Gains chart, or both.

#### Method

`xtile` will generate buckets of similar size, depending on the variation in the predictor variable(s). `ntile` will attempt to create buckets that are exactly equal. However, this does mean that customers with the same score on the `predictor` could be placed in different buckets.

#### # quantiles

The number of buckets generate by `method`.


