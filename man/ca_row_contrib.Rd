% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ca.R
\name{ca_row_contrib}
\alias{ca_row_contrib}
\title{Compute row contributions}
\usage{
ca_row_contrib(row_coords, X, eigs)
}
\arguments{
\item{row_coords}{row coordinates}

\item{X}{standardized matrix}

\item{eigs}{eigs computed by \code{ca_weighted_eigen}}
}
\value{
A dataframe of row contributions.
}
\description{
Return row contributions for each correspondence component
}
\examples{
library(booklet)

X_scaled <- mtcars[, c(2, 8:11)] |>
  ca_standardize()

eigs <- X_scaled |>
  ca_weighted_eigen()

eigs |>
  ca_row_coords() |>
  ca_row_contrib(X_scaled, eigs) |>
  head()
}
