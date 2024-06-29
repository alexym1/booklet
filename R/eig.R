#' Compute eigvalues and eigvectors
#'
#' Return eigvalues and eigvectors of a matrix
#'
#' @param X X_active
#' @param weights an optional row weights (by default, a vector of 1 for uniform row weights); the weights are given only for the active individuals
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize_norm() |>
#'   get_eigen()
#'
#' @export
get_eigen <- function(X, weights = NULL) {
  if (!is.null(weights)) {
    svd_res <- svd(t(t(X) * sqrt(weights)))
    eigs <- list(values = svd_res$d^2, vectors = svd_res$v)
  } else {
    eigs <- X %>%
      cov() %>%
      eigen()
  }

  eigs[[2]] <- eigs %>%
    extract2(2) %>%
    set_rownames(colnames(X)) %>%
    set_colnames(paste0("Dim.", 1:ncol(X)))

  return(eigs)
}

#' @rdname get_eigen
#' @export
eigvalues <- function(X, weights = NULL) {
  vct_eigvalues <- get_eigen(X, weights) %>%
    extract2(1)

  return(vct_eigvalues)
}

#' @rdname get_eigen
#' @export
eigvectors <- function(X, weights = NULL) {
  mtx_eigvectors <- get_eigen(X, weights) %>%
    extract2(2)

  return(mtx_eigvectors)
}
