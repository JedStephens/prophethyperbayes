#' Title Suggested number of initial points
#'
#' @param number_of_bounds The number of bounds passed to the `rBayesianOptimization::BayesianOptimization()` function
#'
#' @return a suggested number of initial points
#' @export
#'
#' @examples suggested_init_points(3)
suggested_init_points <- function(number_of_bounds){
  5*(2^(number_of_bounds-1))
}
