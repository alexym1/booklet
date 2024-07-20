#' Compute eigvalues and eigvectors
#'
#' Return eigvalues and eigvectors of a matrix
#'
#' @param X X_active
#'
#' @details
#' Standardization depends on what you need to perform factor analysis. We
#' implemented two types:
#'
#' * \code{get_weighted_eigen}: This is the default method in FactoMineR to compute
#' eigvalues, eigvectors.
#'
#' * \code{get_eigen}: This is the standard method to compute eigvalues, eigvectors.
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize_norm() |>
#'   get_eigen()
#' @export
get_eigen <- function(X) {
  covariance <- cov(X)
  eigs <- eigen(covariance)

  colnames(eigs[[2]]) <- paste0("Dim.", 1:ncol(X))
  rownames(eigs[[2]]) <- colnames(X)

  return(eigs)
}

#' @rdname get_eigen
#' @export
get_weighted_eigen <- function(X) {
  row.w <- rep(1, nrow(X))
  weights <- row.w / sum(row.w)

  svd_res <- svd(t(t(X) * sqrt(weights)))
  V <- svd_res$v
  U <- svd_res$u

  mult <- sign(as.vector(crossprod(rep(1, nrow(V)), as.matrix(V))))
  mult[mult == 0] <- 1
  U <- t(t(U) * mult)
  V <- t(t(V) * mult)
  U <- U / sqrt(weights)

  eigs <- list(values = svd_res$d^2, vectors = V, U = U)

  colnames(eigs[[2]]) <- paste0("Dim.", 1:ncol(X))
  rownames(eigs[[2]]) <- colnames(X)

  return(eigs)
}
