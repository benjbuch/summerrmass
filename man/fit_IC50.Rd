% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_IC50.R
\name{fit_IC50}
\alias{fit_IC50}
\title{Fitting IC50 curves}
\usage{
fit_IC50(
  x,
  formula,
  limits_lower = c(-10, 10),
  limits_upper = c(50, 150),
  limits_hill = c(-Inf, +Inf),
  limits_IC50 = c(0, 1000)
)
}
\arguments{
\item{x}{A data frame to be evaluated.}

\item{formula}{A formula using variables of \code{x}.}

\item{limits_lower, limits_upper, limits_hill, limits_IC50}{Upper and lower bounds
for the paramters to be fitted.}
}
\description{
Fits an IC50 curve using \code{\link[drc:drm]{drc::drm}}.
}
\details{
Fitted parameters are \code{"hill"}, \code{"lower"}, \code{"upper"}, and \code{"IC50"}.
The defaults for upper and lower limits assume percentage
}
