#' Compute eigvalues and eigvectors
#'
#' Return eigvalues and eigvectors of a matrix
#'
#' @param X X_active
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize(type = "norm") |>
#'   get_eigen()
#'
#' @export
get_eigen <- function(X) {
  eigs <- X %>%
    cov() %>%
    eigen()

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
#'   standardize(type = "norm") |>
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
#'   standardize(type = "norm") |>
#'   eigvectors()
#'
#' @export
eigvectors <- function(X) {
  mtx_eigvectors <- get_eigen(X) %>%
    extract2(2)

  return(mtx_eigvectors)
}
