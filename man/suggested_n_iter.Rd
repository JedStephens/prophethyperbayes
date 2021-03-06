% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suggested_n_iter.R
\name{suggested_n_iter}
\alias{suggested_n_iter}
\title{Title Suggested number of times the Bayesian Optimisization is to be repeated (`n_iter`)}
\usage{
suggested_n_iter(number_of_bounds)
}
\arguments{
\item{number_of_bounds}{The number of bounds passed to the `rBayesianOptimization::BayesianOptimization()` function}
}
\value{
a suggested number of Bayesian Optimisation points
}
\description{
Title Suggested number of times the Bayesian Optimisization is to be repeated (`n_iter`)
}
\examples{
suggested_n_iter(3)
}
