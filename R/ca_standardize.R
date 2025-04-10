#' Data standardization for CA
#'
#' Perform data standardization for multivariate exploratory data analysis.
#'
#' @param X Active or supplementary datasets
#' @param weighted_row row weights
#' @param type standardization for supplementary rows or cols
#'
#' @examples
#' library(booklet)
#'
#' mtcars[, c(2, 8:11)] |>
#'   ca_standardize() |>
#'   head()
#' @export
ca_standardize <- function(X, weighted_row = rep(1, nrow(X))) {
  new_CA <- as.matrix(X) * weighted_row / sum(X * weighted_row)
  CA_scaled <- t(t(new_CA / rowSums(new_CA)) / colSums(new_CA)) - 1
  CA_lst <- list(CA_scaled = CA_scaled, weighted_row = rowSums(new_CA), weighted_col = colSums(new_CA))
  return(CA_lst)
}

#' @rdname ca_standardize
#' @export
ca_standardize_sup <- function(X, type = c("row", "col"), weighted_row = rep(1, nrow(X))) {
  type <- match.arg(type)
  if (type == "row") {
    X_sup_scaled <- X / rowSums(X)
  } else if (type == "col") {
    X_sup_scaled <- t(t(X * weighted_row) / colSums(X))
  }
  return(X_sup_scaled)
}
