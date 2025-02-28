% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{create_demo_preds}
\alias{create_demo_preds}
\title{Simulate a data.frame prediction interval to use in tests}
\usage{
create_demo_preds(n_orgUnit, n_months, n_quant = 2)
}
\arguments{
\item{n_orgUnit}{number of orgUnits}

\item{n_months}{number of months of analysis data, uses 3 months of assess}

\item{n_quant}{number of quantiles to use in prediction intervals}
}
\value{
data.frame of prediction intervals
}
\description{
Simulate a data.frame prediction interval to use in tests
}
