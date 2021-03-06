% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_weight_fit.r
\name{calc_weight_fit}
\alias{calc_weight_fit}
\title{Calculate the fit of the simulated weights to the weights observed}
\usage{
calc_weight_fit(sim = NULL, field = NULL)
}
\arguments{
\item{sim}{data.frame The file GooseEnergeticsData.txt}

\item{field}{data.frame The observed weights}
}
\value{
numeric The calculated fit.
}
\description{
Calculate fit between field weights as least squares difference
}
