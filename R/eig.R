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

#' Compute eigvalues
#'
#' Return eigvalues of a matrix
#'
#' @param X X_active
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize_norm() |>
#'   eigvalues()
#'
#' @export
eigvalues <- function(X) {
  vct_eigvalues <- get_eigen(X) %>%
    extract2(1)

  return(vct_eigvalues)
}


#' Compute eigvectors
#'
#' Return eigvectors of a matrix
#'
#' @param X X_active
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize_norm() |>
#'   eigvectors()
#'
#' @export
eigvectors <- function(X) {
  mtx_eigvectors <- get_eigen(X) %>%
    extract2(2)

  return(mtx_eigvectors)
}
