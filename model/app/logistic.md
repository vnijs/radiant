> Estimate a Logistic regression for classification

### Functionality

To estimate a logistic regression we need a binary response variable and one or more explanatory variables. We also need specify the level of the response variable we will count as as _success_ (i.e., the `Choose level:` dropdown). In the example data file `titanic`, success for the variable `survived` would be the level `Yes`.

To access this dataset go to _Data > Manage_, select `examples` from the `Load data of type` dropdown, and press the `Load examples` button. Then select the `titanic` dataset.

In the _Summary_ tab we can test if two or more variables together add significantly to the fit of a model by selecting variables in the `Variables to test` dropdown. This functionality can be very useful to test if the overall influence of a variable of type `factor` is significant.

Additional output that requires re-estimation:

* Standardize: Odds-ratios can be hard to compare if the explanatory variables are measured on different scales. By standardizing the data before estimation we can see which variables move-the-needle most. Note that a one-unit change is now equated to 2 x the standard deviation of the variable.
* Center: Replace all explanatory variables X by X - mean(X). This can be useful when trying to interpret interaction effects.
* Stepwise: A data-mining approach to select the best fitting model

Additional output that does not require re-estimation:

* VIF: Variance Inflation Factors and Rsq. These are measures of multi-collinearity among the explanatory variables
* Confidence intervals: Coefficient confidence intervals
* Odds: Odds-ratios with confidence intervals

We can use the _Predict_ tab to predict probabilities for different values of the explanatory variable(s) (i.e., a common use of Logistic regression models). First, select the type of input for prediction using the `Prediction` radio buttons. Choose either an existing dataset for prediction ("Data") or specify a command ("Command") to generate the prediction inputs. If you choose to enter a command you must specify at least one variable and one value to get a prediction. If you do not specify a value for each variable in the model either the mean value or the most frequent level will be used. It is only possible to predict outcomes based on variables used in the model (e.g., `age` must be one of the selected explanatory variables to predict survival probability for a 90 year old passenger).

* To predict the survival probability for a passenger in 3rd class use `pclass = "3rd"` and press enter
* To predict the survival probability for passengers aged between 0 and 90 at 10 year intervals type `age = seq(0, 90, 10)` and press enter
* To predict the survival probability for passengers per class and gender type `pclass = levels(pclass), sex = c("male","female")` and press enter

As an example of how to use data as input for prediction (e.g., predict the survival probabilities for 30 year old men and women in each of the passenger classes) you can use `titanic_pred`. Select `titanic` as the dataset for analysis and specify a model in _Model > Logistic regression (GLM)_ with `pclass`, `sex`, and `age` as explanatory variables. Choose `Data` from the `Prediction input` dropdown and select `titanic_pred` from the `Predict for profiles` dropdown to generate the predictions. Note that the variables in the datafile and in the model must be the same.

To generate predicted values for all cases in, for example, the `titanic` dataset select `Data` from the `Prediction input` dropdown then select the `titanic` dataset. You can also create a dataset for input in a spreadsheet and then paste it into Radiant through the _Data > Manage_ tab. You can also load csv data as input. For example, paste the following link `https://radiant-rstats.github.io/docs/examples/glm_pred.csv` file into Radiant through the _Data > Manage_ tab and try to generate the same predictions. Hint: Use `csv (url)` to load the data link above.

Once the desired predictions have been generated they can be saved to a csv file by clicking the download button button on the top right of the screen. To add predictions to the dataset, click the `Store` button.

### Example

As an example we will use a dataset that describes the survival status of individual passengers on the Titanic. The principal source for data about Titanic passengers is the Encyclopedia Titanic. One of the original sources is Eaton & Haas (1994) Titanic: Triumph and Tragedy, Patrick Stephens Ltd, which includes a passenger list created by many researchers and edited by Michael A. Findlay. Suppose we want to investigate which factors are most strongly associated with the chance of surviving the sinking of the Titanic. Lets focus on four variables in the database:

- survived = a factor with levels `Yes` and `No`
- pclass = Passenger Class (1st, 2nd, 3rd). This is a proxy for socio-economic status (SES) 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
- sex = Sex (female, male)
- age = Age in years

Select `survived` as the response variable and `Yes` in **Choose level**. Select `pclass`, `sex` and `age` as the explanatory variables. In the screenshot below we see that each of the coefficients is statistically significant (p.value < .05) and that the model has some predictive power (Chi-squared statistic < .05). Unfortunately the coefficients from a logit model are difficult to interpret. The `OR` column provides estimated odds-ratios. We see that the odds of survival were significantly lower for 2nd and 3rd class passengers compared to 1st class passenger. The odds of survival for males were also lower than for females. While the effect of age is statically significant, for each extra year in age the odds of survival are not as strongly affected (see also the standardized coefficient).

For each of the explanatory variables the following null and alternate hypotheses can be formulated for the odds ratios:

* H0: The odds-ratio associated with explanatory variable x is equal to 1
* Ha: The odds-ratio associated with explanatory variable x is not equal to 1

The odds-ratios from the logistic regression can be interpreted as follows:

- Compared to 1st class passengers, the odds of survival for 2nd class passengers was 72% lower, keeping all other variables in the model constant.
- Compared to 1st class passengers, the odds of survival for 3rd class passengers was 89.8% lower, keeping all other variables in the model constant.
- Compared to female passengers, the odds of survival for male passengers was 91.7% lower, keeping all other variables in the model constant.
- For an increase in passenger age of 1 year the odds of survival decreased by 0.34%, keeping all other variables in the model constant.

<p align="center"><img src="figures_model/logistic_summary.png"></p>

In addition to the numerical output provided in the _Summary_ tab we can also evaluate the link between `survival`, `class`, `sex`, and `age` visually (see _Plot_ tab). The settings in the side-panel are the same as before. In the screenshot below we see a coefficient (or rather an odds-ratio) plot with confidence intervals. The relative importance of gender and class compared to age clearly stands out. Note: click the check box for standardized coefficients (i.e., `standardize`) in the _Summary_ tab and see if your conclusion changes.

<p align="center"><img src="figures_model/logistic_plot.png"></p>

Probabilities, are more convenient for interpretation than coefficients or odds from a logit model. To see how survival probabilities change across passenger classes select `Command` from the `Prediction input` dropdown in the _Predict_ tab, type `pclass = levels(pclass)` in the **Prediction command** box, and press return.

<p align="center"><img src="figures_model/logistic_predict.png"></p>

The figure above shows that the probabilities drop sharply for 2nd and 3rd class passengers compared to 1st class passengers. For males of average age (approx. 30 yo in the sample) the survival probability was close to 50%. For 30 yo, male, 3rd class passengers this probability was closer to 9%.

```r
    age  sex pclass  pred
 29.881 male    1st 0.499
 29.881 male    2nd 0.217
 29.881 male    3rd 0.092
```

To see the effects of gender type `sex = levels(sex)` in the **Prediction command** box and press return. For average age females in 3rd class the survival probability was around 50%. For males with the same age and class characteristics the chance of survival was closer to 9%.

```r
    age pclass    sex  pred
 29.881    3rd female 0.551
 29.881    3rd   male 0.092
```

To see the effects for age type `age = seq(0, 100, 20)` in the **Prediction command** box and press return. For male infants in 3rd class the survival probability was around 22%. For 60 year old males in 3rd class the probability drops to around 3.5%. For the oldest males on board, the model predicts a survival probability close to 1%.

```r
 pclass  sex age  pred
    3rd male   0 0.220
    3rd male  20 0.124
    3rd male  40 0.067
    3rd male  60 0.035
    3rd male  80 0.018
    3rd male 100 0.009
```

For a more comprehensive overview of the influence of gender, age, and passenger class on the chances of survival we can generate a full table of probabilities by selecting `Command` from the `Prediction input` dropdown in the _Predict_ tab and selecting `Titanic` from the `Predict for profiles`. There are too many numbers to easily interpret in table form but the figure gives a clear overview of how survival probabilities change with `age`, `gender`, and `pclass`:

<p align="center"><img src="figures_model/logistic_predict_data.png"></p>
