#' Compute coordinates for individuals
#'
#' Return principal component for individuals
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param eigs eigs computed by \code{get_eigen} or \code{get_weighted_eigen}
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize_norm() |>
#'   get_weighted_eigen() |>
#'   pca_ind_coords() |>
#'   head()
#' @export
pca_ind_coords <- function(eigs) {
  ind_coords <- t(t(as.matrix(eigs[["U"]])) * sqrt(eigs[["values"]]))
  colnames(ind_coords) <- paste0("Dim.", 1:ncol(ind_coords))
  return(ind_coords)
}

#' @rdname pca_ind_coords
#' @export
pca_sup_ind_coords <- function(X, eigs) {
  return(as.matrix(X) %*% eigs[["vectors"]])
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
#' iris[, -5] |>
#'   standardize_norm() |>
#'   get_weighted_eigen() |>
#'   pca_ind_coords() |>
#'   pca_ind_cos2() |>
#'   head()
#' @export
pca_ind_cos2 <- function(ind_coords) {
  ind_cos2 <- ind_coords^2 / rowSums(ind_coords^2)
  return(ind_cos2)
}


#' Compute individual contributions
#'
#' Return indivdual contributions for each principal component
#'
#' @param ind_cos2 individual cos2
#' @param eigenvalues eigenvalues
#'
#' @details
#' If you want to compute the contributions of the individuals to the principal
#' components, you have to change the weighted_col argument to rep(1, nrow(ind_cos2)).
#'
#' @examples
#' library(FactoMineR2)
#'
#' eigs <- iris[, -5] |>
#'   standardize_norm() |>
#'   get_weighted_eigen()
#'
#' eigs |>
#'   pca_ind_coords(eigs$vectors) |>
#'   pca_ind_cos2() |>
#'   pca_ind_contrib(eigs) |>
#'   head()
#' @export
pca_ind_contrib <- function(ind_cos2, eigs, weighted_row = rep(1, nrow(ind_cos2)) / nrow(ind_cos2)) {
  contrib <- sweep(ind_cos2, 2, eigs[["values"]], FUN = "/")
  ind_contrib <- 100 * contrib / nrow(contrib)

  # ind_contrib <- t(t(ind_cos2 * weighted_row) / eigs[["values"]]) * 100
  return(ind_contrib)
}


#' Compute variable coordinates
#'
#' Return variable coordinates
#'
#' @param eigs eigs computed by \code{get_eigen} or \code{get_weighted_eigen}
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize_norm() |>
#'   get_weighted_eigen() |>
#'   pca_var_coords() |>
#'   head()
#' @export
pca_var_coords <- function(eigs) {
  var_coords <- t(t(as.matrix(eigs[["vectors"]])) * sqrt(eigs[["values"]]))
  colnames(var_coords) <- paste0("Dim.", 1:ncol(var_coords))
  return(var_coords)
}

#' @rdname pca_var_coords
#' @export
pca_var_cor <- function(eigs) {
  var_coords <- t(t(as.matrix(eigs[["vectors"]])) * sqrt(eigs[["values"]]))
  colnames(var_coords) <- paste0("Dim.", 1:ncol(var_coords))
  return(var_coords)
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
#' iris[, -5] |>
#'   standardize_norm() |>
#'   get_weighted_eigen() |>
#'   pca_var_coords() |>
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
#' eigs <- iris[, -5] |>
#'   standardize_norm() |>
#'   get_weighted_eigen()
#'
#' eigs |>
#'   pca_var_coords() |>
#'   pca_var_cos2() |>
#'   pca_var_contrib(eigs) |>
#'   head()
#' @export
pca_var_contrib <- function(var_cos2, eigs=NULL, weighted_col = rep(1, ncol(var_cos2))) {
  var_contrib <- t(t(var_cos2) / eigs[["values"]]) * weighted_col * 100
  return(var_contrib)
}
