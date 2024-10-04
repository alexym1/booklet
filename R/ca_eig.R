#' Compute eigvalues and eigvectors for CA
#'
#' Return eigvalues and eigvectors of a matrix
#'
#' @param X X_active
#' @param weighted_row row weights
#' @param weighted_col column weights
#'
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[,-5] |>
#'   ca_standardize() |>
#'   ca_weighted_eigen()
#' @export
ca_weighted_eigen <- function(X, weighted_row = rowSums(X), weighted_col = colSums(X)){
  CA_scaled <- t(t(X / weighted_row) / weighted_col) - 1
  eigs <- pca_weighted_eigen(CA_scaled, weighted_row = weighted_row, weighted_col = weighted_col)
  return(eigs)
}
