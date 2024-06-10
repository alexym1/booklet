#' Compute coordinates for individuals
#'
#' Return principal component for individuals
#'
#' @param X X_active
#' @param eigvectors eigenvectors
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[,-5] |>
#'   standardize(type = "norm")
#'
#' vectors <- X_active |>
#'   eigvectors()
#'
#' X_active |>
#'   pca_ind_coords(vectors)
#'
#' @export
pca_ind_coords <- function(X, eigvectors) {
  return(X %*% eigvectors)
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
#' X_active <- iris[,-5] |>
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
  ind_cos2 <- ind_coords^2 / rowSums(ind_coords^2)
  return(ind_cos2)
}


#' Compute individual contributions
#'
#' Return indivdual contributions for each principal component
#'
#' @param ind_coords individual coordinates
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[,-5] |>
#'   standardize(type = "norm")
#'
#' vectors <- X_active |>
#'   eigvectors()
#'
#' X_active |>
#'   pca_ind_coords(vectors) |>
#'   pca_ind_contrib()
#'
#' @export
pca_ind_contrib <- function(ind_coords) {
  ind_contrib <- 100 * (ind_coords^2) / colSums(ind_coords^2)
  return(ind_contrib)
}


#' Compute variable coordinates
#'
#' Return variable coordinates
#'
#' @param eigvalues eigvalues
#' @param eigvectors eigvectors
#'
#' @examples
#' library(FactoMineR2)
#'
#' X_active <- iris[,-5] |>
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
pca_var_coords <- function(eigvalues, eigvectors) {
  var_coords <- eigvectors %*% (eigvalues %>% sqrt() %>% diag)
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
#' X_active <- iris[,-5] |>
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
#' X_active <- iris[,-5] |>
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
  var_contrib <- 100 * var_coords^2 / colSums(var_coords^2)
  return(var_contrib)
}
