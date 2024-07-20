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
#'   standardize_norm()
#'
#' eigs <- X_active |>
#'   get_eigen()
#'
#' X_active |>
#'   pca_ind_coords(eigs$vectors) |>
#'   head()
#' @export
pca_ind_coords <- function(X, eigenvectors) {
  return(as.matrix(X) %*% eigenvectors)
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
#'   standardize_norm()
#'
#' eigs <- X_active |>
#'   get_eigen()
#'
#' X_active |>
#'   pca_ind_coords(eigs$vectors) |>
#'   pca_ind_cos2() |>
#'   head()
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
#'   standardize_norm()
#'
#' eigs <- X_active |>
#'   get_eigen()
#'
#' X_active |>
#'   pca_ind_coords(eigs$vectors) |>
#'   pca_ind_contrib(eigs$values) |>
#'   head()
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
#'   standardize_norm()
#'
#' eigs <- X_active |>
#'   get_eigen()
#'
#' eigs$values |>
#'   pca_var_coords(eigs$vectors) |>
#'   head()
#' @export
pca_var_coords <- function(eigenvalues, eigenvectors) {
  var_coords <- eigenvectors %*% diag(sqrt(eigenvalues))
  colnames(var_coords) <- paste0("Dim.", 1:ncol(var_coords))
  return(var_coords)
}

#' @rdname pca_var_coords
#' @export
pca_var_cor <- function(eigenvalues, eigenvectors) {
  var_cor <- -1 * (eigenvectors %*% diag(sqrt(eigenvalues)))
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
#'   standardize_norm()
#'
#' eigs <- X_active |>
#'   get_eigen()
#'
#' eigs$values |>
#'   pca_var_coords(eigs$vectors) |>
#'   pca_var_cos2() |>
#'   head()
#' @export
pca_var_cos2 <- function(var_coords) {
  return(var_coords^2)
}


#' Compute variable contributions
#'
#' Return variable contributions
#'
#' @param var_cos2 variable coordinates
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[, -5] |>
#'   standardize_norm()
#'
#' eigs <- X_active |>
#'   get_eigen()
#'
#' eigs$values |>
#'   pca_var_coords(eigs$vectors) |>
#'   pca_var_cos2() |>
#'   pca_var_contrib() |>
#'   head()
#' @export
pca_var_contrib <- function(var_cos2) {
  var_contrib <- sweep(var_cos2, 2, colSums(var_cos2), FUN = "/") * 100
  return(var_contrib)
}
