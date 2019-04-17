#' Prophet model fitting with forward out of sample error
#'
#' @param prophet_configuration_object The result of your configuration of the prophet_configuration() function template. (Described on the GitHub in the README)
#' @param training_data the data upon which the model trains. This must be a dataframe with at least the column ds (standing for date in POSXIT) and y which are the observed values
#' @param testing_data the data upon which predictions should be tested. This dataframe should include all the training data. i.e. it should just extend the training data by the values against which you seek to test. Importantly, this must be a dataframe with at least the column ds (standing for date in POSXIT) and y which are the observed values
#' @param metric your choice of: "ME", "RMSE", "MAE", "MPE", "MAPE", "MASE". Provide only one option as a character string.
#' @return a list with three named elements: the (1) fitted model, its (2) prediction (over the test data only) and (3) an error aggregation.
#' @export
prophet_forwardoose <- function(prophet_configuration_object, training_data, testing_data, metric = c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE")) {
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

  if(is.null(metric)){
    graphical <- data.frame(ds = testing_data$ds, observed = testing_data$y)
    # Add in a blank row.
    graphical$predicted <- c()
    graphical$trend <- c()

    graphical$predicted[(training_rows+1):testing_rows] <- prediction$yhat
    graphical$trend[(training_rows+1):testing_rows] <- prediction$trend
    return(list(fitted_model = fitted_model, prediction = prediction, graphical = graphical))

  }else{

  # Determine the error over the out-of-sample period.
  error <- forecast::accuracy(prediction[, 'yhat'], testing_data$y[(training_rows+1):testing_rows])[,metric]

  return(list(fitted_model = fitted_model, prediction = prediction, error = error))
  }
}
