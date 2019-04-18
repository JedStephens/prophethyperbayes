#' Prophet Rolling Mean Out of Sample Error (Hyndman's Evaluation on a Rolling Forecasting Origin)
#'
#' @description This function implements Hyndman's Evaluation on a rolling forecasting origin. (See https://robjhyndman.com/hyndsight/tscv/).
#' The arguments of the function are used to configure it.
#'
#' @param prophet_configuration_object The result of your configuration of the prophet_configuration() function template. (Described on the GitHub in the README)
#' @param data a data object containing all data (test and train). This must be a dataframe with at least the column ds (standing for date in POSXIT) and y which are the observed values
#' @param cores_used The numebr of cores upon which the paralised optimisation process is allowed to occur. (Leave at least one core open.)
#' @param error_metric your choice of: "ME", "RMSE", "MAE", "MPE", "MAPE", "MASE". Provide only one option as a character string.
#' @param aggregating_metric either the mean (specify "MEAN") or a vector of weights (which will calculate a weighted mean).
#' @param process_starting_row The row for which it is the first point in Hyndman's Evaluation on a rolling forecasting origin
#' @param per_validation_period The number of periods per each of the forward Hyndman's Evaluation on a rolling forecasting origin.
#'
#' @return A list with two metrics: Score (which is the value to be maximized) and Pred which is not currely being used.
#'
#' @export
prophet_rolling_crossvalidation <- function(prophet_configuration_object, data, cores_used,
                                            error_metric = c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE"),
                                            aggregating_metric = "MEAN",
                                            process_starting_row, per_validation_period){
  # Organise the cluster (Part #1)
  library(foreach)
  library(doParallel)
  #---

  observations <- nrow(data)
  number_of_validations <- floor((observations - 1 - process_starting_row)/per_validation_period)
  # Subtraction of 1 from the total number of observations so that it is uniquely different.
  cat("There are ", number_of_validations, "rolling forecast validations each of lenght", per_validation_period, ".\n")
  if(number_of_validations == 0){
    simpleError("No validation process can be started with zero rolling validations...")
    stop()
  }
  # Logical tests for the aggregating metric
  if(aggregating_metric[1] == "MEAN"){
    # The reason [1] is provided is to ensure checking on only the first argument of the vector
    aggregating_metric_interal <- "MEAN"
  }else if(is.numeric(aggregating_metric)){
    if(length(aggregating_metric) == number_of_validations){
      aggregating_metric_interal <- "WGHTAVG"
    }else{
      stop(simpleError("The weighted average metric is not the same size as the number of rolling forecast validations"))
    }
  }else{
      stop(simpleError("The chosen aggregating metric is not recognised"))
  }


  # Graphically Organised Section ---------------------------------------
  print(.helper_dot_graph(process_starting_row, per_validation_period, number_of_validations))



  # Organise the cluster (part #2)
  cl <- makeCluster(cores_used)
  registerDoParallel(cl)
  # --

  # Begin the evaluation
  #Create output baskets
  #error_measure_per_validation <- numeric(length = number_of_validations)

  #For each iteration
  # for (it in 1:number_of_validations) {
  #   train = data[1:(process_starting_row + (it-1)*per_validation_period),]
  #   test  = data[1:(process_starting_row + 1 + (it)*per_validation_period),]
  #   # Note that the plus 1 row just makes sure that test and train never have overlapping rows.
  #   result <- prophet_forwardoose(prophet_configuration_object,
  #                                    training_data = train,
  #                                    testing_data = test,
  #                                    error_metric)
  #   error_measure_per_validation[it] <- result$error
  # }

  error_measure_per_validation <- foreach(it=1:number_of_validations, .combine = c, .inorder = TRUE, .multicombine = TRUE) %dopar% ({
    train = data[1:(process_starting_row + (it-1)*per_validation_period),]
    test  = data[1:(process_starting_row + 1 + (it)*per_validation_period),]
    result <- prophet_forwardoose(prophet_configuration_object,
                                 training_data = train,
                                 testing_data = test,
                                 error_metric)
    # Aggregate this!
    result$error
  })
  # Organise the cluster (Part 3)
  stopCluster(cl)
  stopImplicitCluster()
  #End of iteration process

  # Aggregation process
  if(aggregating_metric_interal == "MEAN"){
    average_error <- mean(error_measure_per_validation)
  }else if(aggregating_metric_interal == "WGHTAVG"){
    average_error <- sum(error_measure_per_validation * aggregating_metric)/(sum(error_measure_per_validation))
  }
  # Desired output for the
  #rBayesianOptimization::BayesianOptimization() function
  return(list(Score = -average_error, Pred = 0))
}
