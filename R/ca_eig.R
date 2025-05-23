#' Compute eigenvalues and eigenvectors for CA
#'
#' Return eigenvalues and eigenvectors of a matrix
#'
#' @param X X_active
#'
#' @returns A list containing results of Single Value Decomposition (SVD).
#'
#' @examples
#' library(booklet)
#'
#' mtcars[, c(2, 8:11)] |>
#'   ca_standardize() |>
#'   ca_weighted_eigen() |>
#'   head()
#' @export
ca_weighted_eigen <- function(X) {
  eigs <- pca_weighted_eigen(X[["CA_scaled"]], weighted_row = X[["weighted_row"]], weighted_col = X[["weighted_col"]])
  return(eigs)
}
