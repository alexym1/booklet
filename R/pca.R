#' Compute coordinates for individuals
#'
#' Return principal component for individuals
#'
#' @param eigs eigs computed by \code{pca_eigen} or \code{pca_weighted_eigen}
#'
#' @returns A dataframe of individual coordinates.
#'
#' @examples
#' library(booklet)
#'
#' iris[, -5] |>
#'   pca_standardize_norm() |>
#'   pca_weighted_eigen() |>
#'   pca_ind_coords() |>
#'   head()
#' @export
pca_ind_coords <- function(eigs) {
  ind_coords <- t(t(as.matrix(eigs[["U"]])) * sqrt(eigs[["values"]]))
  return(as.data.frame(ind_coords))
}

#' Compute individual squared cosines
#'
#' Return individual squared cosines for each principal component
#'
#' @param ind_coords individual coordinates
#' @param weighted_col column weights
#'
#' @returns A dataframe of individual squared cosines.
#'
#' @examples
#' library(booklet)
#'
#' iris[, -5] |>
#'   pca_standardize_norm() |>
#'   pca_weighted_eigen() |>
#'   pca_ind_coords() |>
#'   pca_ind_cos2() |>
#'   head()
#' @export
pca_ind_cos2 <- function(ind_coords, weighted_col = rep(1, ncol(ind_coords))) {
  ind_cos2 <- ind_coords^2 / rowSums(t(t(ind_coords^2) * weighted_col))
  return(ind_cos2)
}


#' Compute individual contributions
#'
#' Return individual contributions for each principal component
#'
#' @param ind_coords individual coordinates
#' @param eigs eigs computed by \code{pca_eigen} or \code{pca_weighted_eigen}
#' @param weighted_row row weights
#'
#' @returns A dataframe of individual contributions.
#'
#' @details
#' If you want to compute the contributions of the individuals to the principal
#' components, you have to change the weighted_col argument to rep(1, nrow(ind_cos2)).
#'
#' @examples
#' library(booklet)
#'
#' eigs <- iris[, -5] |>
#'   pca_standardize_norm() |>
#'   pca_weighted_eigen()
#'
#' eigs |>
#'   pca_ind_coords() |>
#'   pca_ind_contrib(eigs) |>
#'   head()
#' @export
pca_ind_contrib <- function(ind_coords, eigs, weighted_row = rep(1, nrow(ind_coords)) / nrow(ind_coords)) {
  ind_contrib <- t(t(ind_coords^2 * weighted_row) / eigs[["values"]]) * 100
  return(as.data.frame(ind_contrib))
}


#' Compute variable coordinates
#'
#' Return variable coordinates
#'
#' @param eigs eigs computed by \code{pca_eigen} or \code{pca_weighted_eigen}
#'
#' @returns A dataframe of variable coordinates.
#'
#' @examples
#' library(booklet)
#'
#' iris[, -5] |>
#'   pca_standardize_norm() |>
#'   pca_weighted_eigen() |>
#'   pca_var_coords() |>
#'   head()
#' @export
pca_var_coords <- function(eigs) {
  var_coords <- t(t(as.matrix(eigs[["vectors"]])) * sqrt(eigs[["values"]]))
  return(as.data.frame(var_coords))
}

#' Compute variable correlation
#'
#' Return variable correlation
#'
#' @param eigs eigs computed by \code{pca_eigen} or \code{pca_weighted_eigen}
#'
#' @returns A dataframe of variable correlation.
#'
#' @examples
#' library(booklet)
#'
#' iris[, -5] |>
#'   pca_standardize_norm() |>
#'   pca_weighted_eigen() |>
#'   pca_var_cor() |>
#'   head()
#' @export
pca_var_cor <- function(eigs) {
  var_cor <- (eigs[["vectors"]] %*% diag(sqrt(eigs[["values"]])))
  colnames(var_cor) <- paste0("Dim.", 1:ncol(var_cor))
  return(as.data.frame(var_cor))
}


#' Compute variable squared cosines
#'
#' Return variable squared cosines
#'
#' @param var_coords variable coordinates
#'
#' @returns A dataframe of variable squared consines.
#'
#' @examples
#' library(booklet)
#'
#' iris[, -5] |>
#'   pca_standardize_norm() |>
#'   pca_weighted_eigen() |>
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
#' @param eigs eigs computed by \code{pca_eigen} or \code{pca_weighted_eigen}
#' @param weighted_col column weights
#'
#' @returns A dataframe of variable contributions.
#'
#' @examples
#' library(booklet)
#'
#' eigs <- iris[, -5] |>
#'   pca_standardize_norm() |>
#'   pca_weighted_eigen()
#'
#' eigs |>
#'   pca_var_coords() |>
#'   pca_var_cos2() |>
#'   pca_var_contrib(eigs) |>
#'   head()
#' @export
pca_var_contrib <- function(var_cos2, eigs, weighted_col = rep(1, ncol(var_cos2))) {
  var_contrib <- t(t(var_cos2) / eigs[["values"]]) * weighted_col * 100
  return(as.data.frame(var_contrib))
}
