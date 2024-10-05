#' Data standardization for CA
#'
#' Perform data standardization for multivariate exploratory data analysis.
#'
#' @param X matrix
#' @param weighted_row row weights
#'
#' @examples
#' library(FactoMineR2)
#'
#' mtcars[, c(2, 8:11)] |>
#'   ca_standardize()
#' @export
ca_standardize <- function(X, weighted_row = rep(1, nrow(X))) {
  new_CA <- as.matrix(X) * weighted_row / sum(X * weighted_row)
  CA_scaled <- t(t(new_CA / rowSums(new_CA)) / colSums(new_CA)) - 1
  CA_lst <- list(CA_scaled = CA_scaled, weighted_row = rowSums(new_CA), weighted_col = colSums(new_CA))
  return(CA_lst)
}
