# Advanced Data Science
This package is developed by Advanced Data Science (http://www.advanceddatascience.biz/).
Contact us for your business analytics and statistical needs.
We are experts in many forecasting forecasting methods.

# About this package
This package is provides a friendly method to perform Bayesian hyperparameter optimisation (for the prophet forecasting tool).

# How to use
To use this package two custom functions need to be written in the form of the template below.
The purpose of these functions is to allow the `BayesianOptimization()` (from the *rBayesianOptimization* package) access to the hyperparameters which you seek to optimise.

## `prophet_configuration()` function
This function exposes at a minimum the hyperparameters that are being sought to optimise.
In this instance the hyperparameters are the `wfo`, `mfo` and `yfo` parameters and the `public_holidays` argument is passed through to the model.
It is not *necessary* to pass each argument in this manner providing that objects exist in your global environment.
(For instance if `public_holidays` existed in the global environment and was not an argument to the `prophet_configuration()` function it would pass straight to the `model` object.)

``prophet_configuration <- function(wfo, mfo, yfo, public_holidays){
  model <- prophet(
    #Typical configuration
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    holidays = public_holidays
    #n.changepoints = no_changepoints,
    #holidays.prior.scale = holidays_prior_scale,
    #seasonality.prior.scale = seasonality_prior_scale,
    #mcmc.samples = ifelse(seasonal_uncertainty, mmc_samples, 0)
  )

  # EACH OF THESE ARE THE ELEMENTS FOR WHICH WE NEED TO OPTIMISE
  # 1
  model <- model %>% add_seasonality(name = 'monthly',
                                     period = 30.5,
                                     fourier.order = mfo)
  #2
  model <- model %>% add_seasonality(name = 'weekly',
                                     period = 7,
                                     fourier.order = wfo)
  #3
  model <- model %>% add_seasonality(name = 'yearly',
                                     period = 365.25,
                                     fourier.order = yfo)

  # The model object once all modification is complete should be returned.
  return(model)
  }``

## `prophet_configuration()` function
The `prophet_optimisation_function()` must make use of certain objects defined in your global environment.
The `rBayesianOptimization()` function can only take a function which exposes the hyperparameters (to be optimised) and no other arguments.
The purpose of the `prophet_optimisation_function()` is to create such a function.

The `prophet_optimisation_function()` must contain only a call to the `prophet_rolling_crossvalidation()` function. `prophet_rolling_crossvalidation()` is a function provided upon installation of this package.
To understand more about the versatile configuration of this function load the help documentation from the R console: `?prophet_rolling_crossvalidation()`.

The `prophet_configuration()` function is the first argument of the `prophet_rolling_crossvalidation()` function and it needs to be configured so that all required arguments (except the hyperparameters) are satisfied by objects *in the global environment*!
The other arguments for `prophet_configuration()` are marked with `#Your configuration`.
See the function help for detials.


``prophet_optimisation_function <- function(wfo, mfo, yfo){
  prophet_rolling_crossvalidation(
    prophet_configuration(wfo, mfo, yfo, public_holidays),
    data = prophet_data_full,   #Your configuration
    error_metric = "RMSE",      #Your configuration
    process_starting_row = 700, #Your configuration
    per_validation_period = 7   #Your configuration
    )
}``

# Installation
Installing this package requires the *devtools* package.
Ensure that in R (or RStudio) it is installed else (i.e. `install.packages("devtools")`)
Then run the following lines in your R console:
``require(devtools)
install_github("JedStephens/prophethyperbayes", dependencies = TRUE)``
