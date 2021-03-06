# Advanced Data Science
This package is developed by Advanced Data Science (http://www.advanceddatascience.biz/).
Contact us for your business analytics and statistical needs.
We are experts in many forecasting methods.

The framework from which this was developed came from and owes itself greatly to Terrence Neumann's publication: http://rpubs.com/tdneumann/351073

# About this package
This package is provides a friendly method to perform Bayesian hyperparameter optimisation (for the prophet forecasting tool).

# How to use
To use this package two custom functions need to be written in the form of the template below.
The purpose of these functions is to allow the `BayesianOptimization()` function (from the *rBayesianOptimization* package) access to the hyperparameters which you seek to optimise.
Thereafter the call to the `BayesianOptimization()` function can be made. 
An example of this is also given.

## `prophet_configuration()` function
This function exposes at a minimum the hyperparameters that are being sought to optimise.
In this instance the hyperparameters are the `wfo`, `mfo` and `yfo` parameters and the `public_holidays` argument is passed through to the model.
It is not *necessary* to pass each argument in this manner providing that objects exist in your global environment.
(For instance if `public_holidays` existed in the global environment and was not an argument to the `prophet_configuration()` function it would pass straight to the `model` object.)

```R
prophet_configuration <- function(wfo, mfo, yfo, public_holidays){
  model <- prophet(
    #Typical configuration
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    holidays = public_holidays         #YOUR CONFIGURATION
    #n.changepoints = no_changepoints, #YOUR CONFIGURATION
    #holidays.prior.scale = holidays_prior_scale, #YOUR CONFIGURATION
    #seasonality.prior.scale = seasonality_prior_scale, #YOUR CONFIGURATION
    #mcmc.samples = ifelse(seasonal_uncertainty, mmc_samples, 0)
  )

  # YOUR CONFIGURATION
  model <-  add_seasonality(model,   name = 'monthly',
                                     period = 30.5,
                                     fourier.order = mfo)
  #2
  model <-  add_seasonality(model,   name = 'weekly',
                                     period = 7,
                                     fourier.order = wfo)
  #3
  model <- add_seasonality(model,    name = 'yearly',
                                     period = 365.25,
                                     fourier.order = yfo)
  # Note, you could have made these modifications earlier in the itital prophet call. It is your preference.                                   

  # The model object once all modification is complete should be returned.
  return(model)
  }
```

## `prophet_optimisation_function()` function
The `prophet_optimisation_function()` must make use of certain objects defined in your global environment.
The `rBayesianOptimization()` function can only take a function which exposes the hyperparameters (to be optimised) as arguments and no other arguments.
The purpose of the `prophet_optimisation_function()` is to create such a function.

Your configured `prophet_optimisation_function()` must contain only a call to the `prophet_rolling_crossvalidation()` function.
`prophet_rolling_crossvalidation()` is a function provided upon installation of this package.
To understand more about the versatile configuration of this function load the help documentation from the R console: `?prophet_rolling_crossvalidation()`.

The `prophet_configuration()` function is the first argument of the `prophet_rolling_crossvalidation()` function and it needs to be configured so that all required arguments (except the hyperparameters) are satisfied by objects *in the global environment*!
The other arguments for `prophet_configuration()` are marked with `#Your configuration`.
See the function help for details.


```R
prophet_optimisation_function <- function(wfo, mfo, yfo){
  prophethyperbayes::prophet_rolling_crossvalidation(
    prophet_configuration(wfo, mfo, yfo, public_holidays),
    data = prophet_data_full,   #Your configuration
    error_metric = "RMSE",      #Your configuration
    process_starting_row = 700, #Your configuration
    per_validation_period = 7   #Your configuration
    )
}
```

## Calling `BayesianOptimization()` for optimisation
For more detials see the help of the `BayesianOptimization()`. 
Here is an example of that function being called for the running example.
The `prophet_optimisation_function` is now passed as the first argument to the `BayesianOptimization()` function. 
Note that `prophet_optimisation_function` is passed *without* the round brackets.
The `bounds` argumentent (from `BayesianOptimization()`) follows next. 
Each of the hyperparameters that you seek to optimise is passed in a named list. 
The `L` suffix on these bounds denote that they are integer values.
Unless an intitial grid search is conducted then ensure that `init_grid_dt = NULL`. 
For more configuration of this argument see the help documentation for `BayesianOptimization()`.
The remaning two arguments are some suggested values from the `BayesianOptimization()` help details. 
These of course can be customised.
```R
prophet_bayes_optimised <- rBayesianOptimization::BayesianOptimization(
  prophet_optimisation_function,  
  bounds = list(wfo = c(5L, 10L),
                mfo = c(7L, 15L),
                yfo = c(1L, 5L)),
  init_grid_dt = NULL, init_points = 10, n_iter = 20)
```
The resulting `prophet_bayes_optimised` will contain the details about the optimal hyperparameters.

# Installation
Installing this package requires the *devtools* package.
Ensure that in R (or RStudio) it is installed else (i.e. `install.packages("devtools")`)
Then run the following lines in your R console:
``require(devtools)
install_github("JedStephens/prophethyperbayes", dependencies = TRUE)``

# Other resources of interest:
https://research.fb.com/efficient-tuning-of-online-systems-using-bayesian-optimization/
https://projecteuclid.org/download/pdfview_1/euclid.ba/1533866666
