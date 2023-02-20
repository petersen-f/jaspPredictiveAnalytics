Predictive Analytics
===
This module allows the user to perform probalistic time series forecasting.

### Assumptions
- no missing covariates or factors

### Input
---

#### Assignment Box
- Dependent Variable: Time series variable to be predicted (needed)
- Time: Time variable that each corresponds to the time stamp of each observation. Can be in the following formats: ['YYYY-MM-DD', 'YYYY/MM/DD', 'YYYY-MM-DD HH:MM:SS', 'YYYY/MM/DD HH:MM:SS'] (needed)")
- Covariates: Covariates to be used in the prediction model
- Factors: Factors to be used in the prediction model
- Training Indicator: Logical variable (only 0 or 1) indicating which cases should be used for training and verifying the models (= 1) and which cases should be predicted (= 0). This variable is necessary for making predictions when covariates and factors are supplied

### Time Series Descriptive
#### Error Bound Selection Method
- Data Based: Bounds are automatically selected based on the mean and how many standard deviations the data is away from this mean. 
    - σ threshold: Indicated the number of standard deviations used to calculate the bound. For example, if it is set to 2, all data that is more than 2 standard deviations away will be flagged as out-of-control
    - Trimmed mean: The mean that determines the bounds is calculated while discarding the upper and lower xx quantile of the sample.
    - Custom period: The mean is calculated only based on the indicated period. Useful when a specific period is available where the process is known to be in control. 
- Manual bounds: Bounds are manually set
  
- Upper/Lower bound: Determines separately where the upper and lower bound is. 

#### Control Plots
- Control chart: Basic control chart that plots time series and shows control limits and whether data is out of control.
  - Spread points equally: Ignores the timestamp of each data point and plots them sequentially as if they would have the same distance to each other.
  - Points/Line/Both: Displays data points either as points, a line or both at the same time.
  - Y-Axis Limits: Plot either shows all of the data available or focuses only on the control bounds which might cut off some outliers.
  - Enable grid: Shows grid on control chart
  - Custom plot focus: Adds a separate plot that only shows data in the indicated time period.
  - Reporting mode: Enables the reporting warning mode for the control chart.
    - Out-of-bound percent threshold: Sets the percent of data that needs to be out-of-bound to trigger a warning.

### Diagnostics
#### Tables
- Summary statistics: Table that shows summary statistics of the time series variable aggregated by whether the data is in or out of control.
- Outlier table: Table that shows all data points that are out of control and whether they exceeds the upper or lower bound.
  - Transpose table: Transposes the table so that the data points are shown in the columns and the variables are shown in the rows.
  - Custom table focus: Only shows outliers in the indicated time period.

#### Plots
- Histogram: Histogram of the time series variable and colours the data points based on whether it is in or out of control.
  - Autocorrelation function: Shows the autocorrelation function plot.
    - Lags: Maximum number of lags shown.
    - Partial autocorrelation function: Additionally shows the partial autocorrelation function plot.


### Feature Engineering
Options to create automatic time series features that might improve prediction accuracy of the models.
- Nr. of lags: Number of lags that are used to create lagged time series variables as features. In the out-of-sample predictions, the previous predictions are used to create the lagged variables to prevent data leakage.
- Automatic time-based features: Creates multiple features based on the time variable. For example, it will create a column that indicates the specific  month (1-12), day (1-31), hour (1-24), minute (1-60)etc. of the time stamp. This is useful for models that don't model seasonality automatically.
- Remove zero-variance variables: Removes variables that have zero variance.
- Remove variables that are stronger correlated than: Removes variables that are strongly correlated with each other. The threshold is set by the user.

### Forecast Evaluation
#### Evaluation Plan
This section allows for evaluating which of the selected models performs best in terms of predictive performance on the available data. This is helpful in determining which model should be used for future predictions. As the goal is to evaluate the predictive performance on unseen future observation, normal cross validation that randomly selects training and test data is not appropriate. Instead, the data is split in such a way that the training data is always before the test data. For example the model is trained on the first 100 observations and then the predictive performance is evaluated on the next 10 observations. Afterwards the data is shifted ahead by a certain amount of observations and the model process is repeated for multiple times. This is called rolling window cross validation. The predictive performance is evaluated on both deterministic and probabilistic metrics that are averaged across all slices.
- Training window: Indicates the number of observations that are used for training the model.
- Prediction window: Indicates the number of observations that are used for to assess the predictive performance of each model.
- Select slices from: Indicates whether the slices are selected from the start or the end of the data. 
- Maximum nr. of slices: Indicates the maximum number of slices that are used for the evaluation. The actual number of slices might be lower if the data is not long enough.
- Cumulative training: Indicates if the training window grows cumulative after each slice or not. Increases time needed for evaluation as models are trained on longer and longer time periods.
- Show evaluation plan: Plots the evaluation plan used for model evaluation.
  - Spread points equally: Ignores the timestamp of each data point and plots them sequentially as if they would have the same distance to each other.
  - Max slices shown: Maximum number of slices that are shown in the plot. Actual number can be higher.

#### Model Choice
Select models that are used for prediction and evaluated. The model family is indicated as the first part of the model name before the hyphen "-" and indicates the model specification afterwards. For example the model "linear regression - regression + lag" is a normal linear Bayesian regression model that uses the covariates and factors as input as well as the lagged variables created in the "Feature Engineering" section. The specific model families and model specifications are explained below.

##### Model Families
- linear regression: Basic Bayesian linear regression model that uses a spike and slab prior for variable selection.
- bsts: Bayesian structural time series model that decomposes time series into different hidden components. Models hidden state either as linear trend or auto-regressive process. Can also include regressors via spike and slab regression as an additional component.
- prophet: Bayesian time series model that automatically models appropriate seasonality via fourier orders, trend and change points. Can also include regressors as an additional component.
- xgboost: Gradient boosting model that uses a tree-based model to predict the time series variable. Does not automatically model seasonality. Not probalisitic.
- bart: Bayesian additive regression tree model that is similar to other "sum-of-tree" models but regularises the impact of each tree via a prior. Does not automatically model seasonality.

##### Model Specifications
- time: Only uses the time variable as input. Useful as a baseline model to compare against more complex models.
- regression: Only uses the covariates and factors as input as well as the automatic time-based features when enabled
- lag: Additionally uses the lagged time series variables as input.

#### Evaluation Metrics
The probabilistic metrics are only available for models that are probabilistic and assess prediction performance based on the whole predictive density taking into account the uncertainty around each forecast. The deterministic metrics are available for all models and only take into account the mean forecast. The metrics are explained below.

##### Probabilistic Metrics
- Continuous ranked probability score (CRPS): Display the CRPS.
- Dawid-Sebastiani score (DSS): Display the DSS.
- Log score (LS): Display the LS.
- Coverage: Display coverage.
- Bias: Display bias.
- Probaility integral transform (PIT): Display the PIT.

##### Probabilistic Metrics
- Mean absolute error (MAE): Display the MAE.
- Root mean squared error (RMSE): Display the RMSE.
- R-squared: Display the R-squared.

- PIT Binned Density Plots: Select the models for which the PIT binned density plots are shown.

### Prediction Plots
 - Models to plot: Select the models for which the out-of-sample predictions are shown that the evaluation metrics are calculated for.


### Bayesian Model Averaging
Ensemble Bayesian model averaging (eBMA) performs model averaging across the out-of-sample predictions of several models. The weights of each model are estimated based on the predictive performance of each model and computed for each slice. The goal is to quantify the uncertainty of the model selection and improve predictive performance. The weights of the previous slice are used to adjust the predictions for the current slice - evaluation metrics are calculated for the adjusted predictions.

- Perform BMA: Performs Bayesian model averaging
  - Method: Determines the algorithm used to calculate the weights of each model.
    - Expectation-Maximisation (EM): Uses the expectation-maximization algorithm to estimate the weights of each model. The default and faster. Sets more model weights to zero than Gibbs sampling.
    - Gibbs: Performs Gibbs sampling to estimate the weights of each model. Slower but produces full posterior distribution for each model. Sets fewer model weights to zero than EM.
  - Evaluation Method: Determines how the predictive performance of the eBMA predictions are evaluated
    - Next test slice: Here the model weights are computed on a whole slice and then used to adjust the predictions for the next slice for which the predictive performance is evaluated.
    - Same test slice: Here the model weights are computed on the first portion of the slice and then used to adjust the predictions for the second portion of the slice for which the predictive performance is evaluated.
      - Last xx percent of data: Determines on which percentage of the slice the performance of the eBMA method is tested on. For example if the value is 0.3 then the model weights are computed on the first 70% of the slice and then used to test the prediction for the remaining 30% of the slice.
- Tables:
  - Model weights: Shows the weights of each model for each slice. By default they are averaged across all slices.
    - Show per slice: Instead of averaging the weights across all slices, show the weights for each slice.


### Future Prediction
This section includes the functionality to predict the future of the time series. The number of data points that is used to train the model for the future predictions is by default set to the same training window that was used in the forecast evaluation. Also allows for warning message via the reporting mode when the indicated probability threshold for crossing the control bounds is exceeded.

- Model Selection: Choose the model that is used for the future predictions.
- Prediction horizon: Choose the number of time points that are predicted into the future.
- Training window: Choose the number of data points that are used to train the model for the future predictions.
  - Last xx data points: Specify the exact number.
  - All data points: Use all data points available to train the prediction model.
- Future prediction plot: Plots the predictions for the future.
  - Spread points equally: Ignores the timestamp of each data point and plots them sequentially as if they would have the same distance to each other.
- Reporting mode: Allows for warning message via the reporting mode when the indicated probability threshold for crossing the control bounds is exceeded.
  - Out-of-bound probability threshold: Set the probability threshold for crossing the control bounds that triggers a warning message. For example, if is set to 20% then a warning message is triggered when the 20th quantile of the predictive distribution is outside the control bounds.

### Advanced Options
 - Parrallel model computation: Select whether the models are computed in parallel. This can speed up the computation time but can also produce errors under windows.
 - Skip between training slices: Selects the time points that the training window is moved forward after each training window. By default, it is set to the prediction window.
### Output
---- 

<!---
Continuous ranked probability score (CRPS): Compares the predicted cumulative density function (CDF) to the actual CDF. Generalisation of the mean absolute error (MAE) to a whole predictive density. The lower the CRPS, the better the model. 
- Dawid-Sebastiani score (DSS): Compares the predictive density to the actual CDF only through its mean and its variance. The lower the DSS, the better the model.
- Log score (LS): 
- Coverage: Proportion of observations that are within the 95% prediction interval. The higher the coverage, the better the model.
- Bias: Indicates whether predictions are systematically too high or too low. If the bias is -1 then all predictions are smaller than the actual value. If the bias is 1 then all predictions are larger than the actual value. The closer the bias is to 0, the more unbiased the model.
- Probaility integral transform (PIT): 
-->
