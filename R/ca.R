#' Compute row coordinates
#'
#' Return Correspondence component for individuals
#'
#' @param eigs eigs computed by \code{ca_weighted_eigen}
#'
#' @examples
#' library(FactoMineR2)
#'
#' mtcars[,c(2,8:11)] |>
#'   ca_standardize |>
#'   ca_weighted_eigen() |>
#'   ca_row_coords() |>
#'   head()
#' @export
ca_row_coords <- function(eigs) {
  row_coords <- t(t(as.matrix(eigs[["U"]])) * sqrt(eigs[["values"]]))
  colnames(row_coords) <- paste0("Dim.", 1:ncol(row_coords))
  return(row_coords)
}

#' Compute row squared cosines
#'
#' Return row squared cosines for each correspondence component
#'
#' @param row_coords row coordinates
#' @param X standardized matrix
#' @param weighted_row row weights
#' @param weighted_col column weights
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_scaled <- mtcars[,c(2,8:11)] |>
#'   ca_standardize()
#'
#' X_scaled |>
#'   ca_weighted_eigen() |>
#'   ca_row_coords() |>
#'   ca_row_cos2(X_scaled) |>
#'   head()
#' @export
ca_row_cos2 <- function(row_coords, X, weighted_row = rowSums(X), weighted_col = colSums(X)) {
  CA_scaled <- t(t(X / weighted_row) / weighted_col) - 1
  row_cos2 <- row_coords^2 / rowSums(t(t(CA_scaled^2) * weighted_col))
  return(row_cos2)
}


#' Compute row contributions
#'
#' Return row contributions for each correspondence component
#'
#' @param row_coords row coordinates
#' @param X standardized matrix
#' @param weighted_row row weights
#' @param weighted_col column weights
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_scaled <- mtcars[,c(2,8:11)] |>
#'   ca_standardize()
#'
#' X_scaled |>
#'   ca_weighted_eigen() |>
#'   ca_row_coords() |>
#'   ca_row_contrib(X_scaled) |>
#'   head()
#' @export
ca_row_contrib <- function(row_coords, X, weighted_row = rowSums(X), weighted_col = colSums(X)) {
  CA_scaled <- t(t(X / weighted_row) / weighted_col) - 1
  row_contrib <- t(t(row_coords^2 * weighted_row) / eigs[["values"]])
  return(row_contrib)
}


#' Compute row contributions
#'
#' Return row contributions for each correspondence component
#'
#' @param row_coords row coordinates
#' @param X standardized matrix
#' @param weighted_row row weights
#' @param weighted_col column weights
#'
#' @examples
#' library(FactoMineR2)
#'
#' mtcars[,c(2,8:11)] |>
#'   ca_standardize() |>
#'   ca_row_inertia()
#' @export
ca_row_inertia <- function(X, weighted_row = rowSums(X), weighted_col = colSums(X)) {
  CA_scaled <- t(t(X / weighted_row) / weighted_col) - 1
  row_inertia <- weighted_row * rowSums(t(t(CA_scaled^2) * weighted_col))
  return(row_inertia)
}
