#' Compute row coordinates
#'
#' Return Correspondence component for individuals
#'
#' @param eigs eigs computed by \code{ca_weighted_eigen}
#'
#' @examples
#' library(FactoMineR2)
#'
#' mtcars[, c(2, 8:11)] |>
#'   ca_standardize() |>
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
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_scaled <- mtcars[, c(2, 8:11)] |>
#'   ca_standardize()
#'
#' X_scaled |>
#'   ca_weighted_eigen() |>
#'   ca_row_coords() |>
#'   ca_row_cos2(X_scaled) |>
#'   head()
#' @export
ca_row_cos2 <- function(row_coords, X) {
  row_cos2 <- row_coords^2 / rowSums(t(t(X[["CA_scaled"]]^2) * X[["weighted_col"]]))
  return(row_cos2)
}


#' Compute row contributions
#'
#' Return row contributions for each correspondence component
#'
#' @param row_coords row coordinates
#' @param X standardized matrix
#' @param eigs eigs computed by \code{ca_weighted_eigen}
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_scaled <- mtcars[, c(2, 8:11)] |>
#'   ca_standardize()
#'
#' eigs <- X_scaled |>
#'   ca_weighted_eigen()
#'
#' eigs |>
#'   ca_row_coords() |>
#'   ca_row_contrib(X_scaled, eigs) |>
#'   head()
#' @export
ca_row_contrib <- function(row_coords, X, eigs) {
  row_contrib <- t(t(row_coords^2 * X[["weighted_row"]]) / eigs[["values"]])
  return(row_contrib)
}


#' Compute row inertia
#'
#' Return row inertia for each correspondence component
#'
#' @param X standardized matrix
#'
#' @examples
#' library(FactoMineR2)
#'
#' mtcars[, c(2, 8:11)] |>
#'   ca_standardize() |>
#'   ca_row_inertia()
#' @export
ca_row_inertia <- function(X) {
  row_inertia <- X[["weighted_row"]] * rowSums(t(t(X[["CA_scaled"]]^2) * X[["weighted_col"]]))
  return(row_inertia)
}


#' Compute col coordinates
#'
#' Return Correspondence component for columns
#'
#' @param eigs eigs computed by \code{ca_weighted_eigen}
#'
#' @examples
#' library(FactoMineR2)
#'
#' mtcars[, c(2, 8:11)] |>
#'   ca_standardize() |>
#'   ca_weighted_eigen() |>
#'   ca_col_coords() |>
#'   head()
#' @export
ca_col_coords <- function(eigs) {
  col_coords <- t(t(as.matrix(eigs[["vectors"]])) * sqrt(eigs[["values"]]))
  colnames(col_coords) <- paste0("Dim.", 1:ncol(col_coords))
  return(col_coords)
}


#' Compute col squared cosines
#'
#' Return col squared cosines for each correspondence component
#'
#' @param col_coords row coordinates
#' @param X standardized matrix
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_scaled <- mtcars[, c(2, 8:11)] |>
#'   ca_standardize()
#'
#' X_scaled |>
#'   ca_weighted_eigen() |>
#'   ca_col_coords() |>
#'   ca_col_cos2(X_scaled) |>
#'   head()
#' @export
ca_col_cos2 <- function(col_coords, X) {
  col_cos2 <- col_coords^2 / colSums(X[["CA_scaled"]]^2 * X[["weighted_row"]])
  return(col_cos2)
}


#' Compute col contributions
#'
#' Return col contributions for each correspondence component
#'
#' @param col_coords row coordinates
#' @param X standardized matrix
#' @param eigs eigs computed by \code{ca_weighted_eigen}
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_scaled <- mtcars[, c(2, 8:11)] |>
#'   ca_standardize()
#'
#' eigs <- X_scaled |>
#'   ca_weighted_eigen()
#'
#' eigs |>
#'   ca_col_coords() |>
#'   ca_col_contrib(X_scaled, eigs) |>
#'   head()
#' @export
ca_col_contrib <- function(col_coords, X, eigs) {
  col_contrib <- t(t(col_coords^2 * X[["weighted_col"]]) / eigs[["values"]])
  return(col_contrib)
}


#' Compute col inertia
#'
#' Return col inertia for each correspondence component
#'
#' @param X standardized matrix
#'
#' @examples
#' library(FactoMineR2)
#'
#' mtcars[, c(2, 8:11)] |>
#'   ca_standardize() |>
#'   ca_col_inertia()
#' @export
ca_col_inertia <- function(X) {
  col_inertia <- X[["weighted_col"]] * colSums(X[["CA_scaled"]]^2 * X[["weighted_row"]])
  return(col_inertia)
}
