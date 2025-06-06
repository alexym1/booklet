#' Compute eigenvalues and eigenvectors
#'
#' Return eigenvalues and eigenvectors of a matrix
#'
#' @param X X_active
#' @param weighted_row row weights
#' @param weighted_col column weights
#'
#' @returns A list containing results of Single Value Decomposition (SVD).
#'
#' @details
#' Standardization depends on what you need to perform factor analysis. We
#' implemented two types:
#'
#' * \code{pca_weighted_eigen}: This is the default method in FactoMineR to compute
#' eigvalues, eigvectors and U matrix.
#'
#' * \code{pca_eigen}: This is the standard method to compute eigenvalues, eigenvectors.
#'
#' @examples
#' library(booklet)
#'
#' iris[, -5] |>
#'   pca_standardize_norm() |>
#'   pca_eigen()
#' @export
pca_eigen <- function(X) {
  svd_res <- svd(X)

  eigs <- list(values = svd_res$d^2, vectors = svd_res$v, U = svd_res$u)

  colnames(eigs$vectors) <- paste0("Dim.", 1:ncol(X))
  rownames(eigs$vectors) <- colnames(X)

  colnames(eigs$U) <- paste0("Dim.", 1:ncol(X))
  rownames(eigs$U) <- rownames(X)

  return(eigs)
}

#' @rdname pca_eigen
#' @export
pca_weighted_eigen <- function(X, weighted_row = rep(1, nrow(X)) / nrow(X), weighted_col = rep(1, ncol(X))) {
  svd_res <- svd(t(t(X) * sqrt(weighted_col)) * sqrt(weighted_row))
  V <- svd_res$v
  U <- svd_res$u

  mult <- sign(as.vector(crossprod(rep(1, nrow(V)), as.matrix(V))))
  mult[mult == 0] <- 1
  U <- t(t(U) * mult) / sqrt(weighted_row)
  V <- t(t(V) * mult) / sqrt(weighted_col)

  eigs <- list(values = svd_res$d^2, vectors = V, U = U)

  colnames(eigs$vectors) <- paste0("Dim.", 1:ncol(X))
  rownames(eigs$vectors) <- colnames(X)

  colnames(eigs$U) <- paste0("Dim.", 1:ncol(X))
  rownames(eigs$U) <- rownames(X)

  return(eigs)
}
