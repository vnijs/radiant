> Evaluate model performance using Lift and Gains charts

To download the table as a csv-files click on the top download button on the right of your screen. To download the plots at a png file click the lower download icon on the right of your screen.

#### Response variable

The outcome, or response, variable of interest. This should be binary variable, either a factor or an integer with two value (i.e., 0 and 1).

#### Choose level

The level in the response variable that is considered a _success_. For example, purchase or buyer is equal to "yes".

#### Predictor

Select one or more variables that can be used to _predict_ the chosen level in the response variable. This could be a variable, an RFM index, or predicted values from a model (e.g., from a logistic regression estimated using _Regression > Logistic regression (GLM)_ or a Neural Network estimated using _Model > Neural Network (ANN)_).

#### # quantiles

The number of buckets to create.

#### Margin & Cost

To use the `Profit` and `ROME` (Return on Marketing Expenditures) charts, enter the `Margin` for each sale and the estimated `Cost` per contact (e.g., mailing costs or opportunity cost of email or text). For example, if the margin on a sale is \$10 (excluding the contact cost) and the contact cost is \$1 enter 10 and 1 in the `Margin` and `Cost` input windows.

#### Show results for

If a `filter` is active (e.g., set in the _Data > View_ tab) generate results for `All` data, `Training` data, `Validation` data, or `Both` training and validation data. If no filter is active calculations are applied to all data.

#### Plots

Generate Lift, Gains, Profit, and/or ROME charts. The profit chart displays the

## Example

The Gains and Profit charts below show little evidence of overfitting and suggest that targeting approximately 72% of customers would maximize profits.

![profit and gains](figures_analytics/perf_profit_gain.png)

The prediction used in the screen shot above was derived from a logistic regression on the `dvd` data. The data is available through the _Data > Manage_ tab (i.e., choose `Examples` from the `Load data of type` drop-down and press `Load examples`). The model was estimated using _Regression > Logistic regression (GLM)_. The predictions shown below were generated in the _Predict_ tab.

![logistic](figures_analytics/perf_logistic.png)
