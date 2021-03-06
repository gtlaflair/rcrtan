% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dependability.R
\name{sd_pop}
\alias{sd_pop}
\title{Calculate standard deviation for the population}
\usage{
sd_pop(x, n)
}
\arguments{
\item{x}{A vector of total scores from a dichotomously score test.}

\item{n}{The number of people who took the test}
}
\description{
Calculate standard deviation for the population
}
\examples{
sd_pop(bh_item$Total, nrow(bh_item))
}
