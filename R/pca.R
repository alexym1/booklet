#' Compute coordinates for individuals
#'
#' Return principal component for individuals
#'
#' @param X X_active
#' @param eigenvectors eigenvectors
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[, -5] |>
#'   standardize(type = "norm")
#'
#' vectors <- X_active |>
#'   eigvectors()
#'
#' X_active |>
#'   pca_ind_coords(vectors)
#'
#' @export
pca_ind_coords <- function(X, eigenvectors) {
  return(-1 * X %*% eigenvectors)
}


#' Compute individual squared cosines
#'
#' Return indivdual squared cosines for each principal component
#'
#' @param ind_coords individual coordinates
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[, -5] |>
#'   standardize(type = "norm")
#'
#' vectors <- X_active |>
#'   eigvectors()
#'
#' X_active |>
#'   pca_ind_coords(vectors) |>
#'   pca_ind_cos2()
#'
#' @export
pca_ind_cos2 <- function(ind_coords) {
  ind_coords <- -1 * ind_coords
  ind_cos2 <- ind_coords^2 / rowSums(ind_coords^2)
  return(ind_cos2)
}


#' Compute individual contributions
#'
#' Return indivdual contributions for each principal component
#'
#' @param ind_coords individual coordinates
#' @param eigenvalues eigenvalues
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[, -5] |>
#'   standardize(type = "norm")
#'
#' vectors <- X_active |>
#'   eigvectors()
#'
#' values <- X_active |>
#'   eigvalues()
#'
#' X_active |>
#'   pca_ind_coords(vectors) |>
#'   pca_ind_contrib(values)
#'
#' @export
pca_ind_contrib <- function(ind_coords, eigenvalues) {
  contrib <- sweep(ind_coords^2, 2, eigenvalues, FUN = "/")
  ind_contrib <- 100 * contrib / nrow(contrib)
  return(ind_contrib)
}


#' Compute variable coordinates
#'
#' Return variable coordinates
#'
#' @param eigenvalues eigenvalues
#' @param eigenvectors eigenvectors
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[, -5] |>
#'   standardize(type = "norm")
#'
#' vectors <- X_active |>
#'   eigvectors()
#'
#' X_active |>
#'   eigvalues() |>
#'   pca_var_coords(vectors)
#'
#' @export
pca_var_coords <- function(eigenvalues, eigenvectors) {
  var_coords <- -1 * (eigenvectors %*% (eigenvalues %>% sqrt() %>% diag()))
  colnames(var_coords) <- paste0("Dim.", 1:ncol(var_coords))
  return(var_coords)
}

#' @rdname pca_var_coords
#' @export
pca_var_cor <- function(eigenvalues, eigenvectors) {
  var_cor <- -1 * (eigenvectors %*% (eigenvalues %>% sqrt() %>% diag()))
  colnames(var_cor) <- paste0("Dim.", 1:ncol(var_cor))
  return(var_cor)
}


#' Compute variable squared cosines
#'
#' Return variable squared cosines
#'
#' @param var_coords variable coordinates
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[, -5] |>
#'   standardize(type = "norm")
#'
#' vectors <- X_active |>
#'   eigvectors()
#'
#' X_active |>
#'   eigvalues() |>
#'   pca_var_coords(vectors) |>
#'   pca_var_cos2()
#'
#' @export
pca_var_cos2 <- function(var_coords) {
  var_cos2 <- var_coords^2 / rowSums(var_coords^2)
  return(var_cos2)
}


#' Compute variable contributions
#'
#' Return variable contributions
#'
#' @param var_coords variable coordinates
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[, -5] |>
#'   standardize(type = "norm")
#'
#' vectors <- X_active |>
#'   eigvectors()
#'
#' X_active |>
#'   eigvalues() |>
#'   pca_var_coords(vectors) |>
#'   pca_var_contrib()
#'
#' @export
pca_var_contrib <- function(var_coords) {
  loadings_squared <- var_coords^2
  total_loadings_squared <- colSums(loadings_squared)
  var_contrib <- sweep(loadings_squared, 2, total_loadings_squared, FUN = "/") * 100
  return(var_contrib)
}
