#' Title Suggested number of times the Bayesian Optimisization is to be repeated (`n_iter`)
#'
#' @param number_of_bounds The number of bounds passed to the `rBayesianOptimization::BayesianOptimization()` function
#'
#' @return a suggested number of Bayesian Optimisation points
#' @export
#'
#' @examples suggested_n_iter(3)
suggested_n_iter <- function(number_of_bounds){
  7*(number_of_bounds)
}
