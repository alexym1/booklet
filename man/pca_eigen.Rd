% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca_eig.R
\name{pca_eigen}
\alias{pca_eigen}
\alias{pca_weighted_eigen}
\title{Compute eigenvalues and eigenvectors}
\usage{
pca_eigen(X)

pca_weighted_eigen(
  X,
  weighted_row = rep(1, nrow(X))/nrow(X),
  weighted_col = rep(1, ncol(X))
)
}
\arguments{
\item{X}{X_active}

\item{weighted_row}{row weights}

\item{weighted_col}{column weights}
}
\value{
A list containing results of Single Value Decomposition (SVD).
}
\description{
Return eigenvalues and eigenvectors of a matrix
}
\details{
Standardization depends on what you need to perform factor analysis. We
implemented two types:
\itemize{
\item \code{pca_weighted_eigen}: This is the default method in FactoMineR to compute
eigvalues, eigvectors and U matrix.
\item \code{pca_eigen}: This is the standard method to compute eigenvalues, eigenvectors.
}
}
\examples{
library(booklet)

iris[, -5] |>
  pca_standardize_norm() |>
  pca_eigen()
}
