prophet_crossvalidated <- function(prophet_configuration_object, training_data, testing_data, metric = c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE")) {
  training_rows <- nrow(training_data)
  testing_rows  <- nrow(testing_data)
  # From the configuration object fit the model on training data.
  fitted_model <- prophet::fit.prophet(prophet_configuration_object, training_data)

  # Determine the difference between the date of the tranining data and testing data (by selecting on the last)
  periods <- as.integer(testing_data$ds[testing_rows] - training_data$ds[training_rows])
  #return(periods)
  # Perform a prediction for the duration of the incoming testing data.
  prediction <- predict(fitted_model, make_future_dataframe(fitted_model, periods = periods, include_history = FALSE))
  #return(list(prediction[, 'yhat'], testing_data$y[(training_rows+1):testing_rows]))
  # Determine the error over the out-of-sample period.
  error <- forecast::accuracy(prediction[, 'yhat'], testing_data$y[(training_rows+1):testing_rows])[ , metric]
  return(list(fitted_model = fitted_model, prediction = prediction, error = error))

}
