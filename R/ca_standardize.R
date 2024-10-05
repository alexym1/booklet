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
#' mtcars[,c(2,8:11)] |>
#'   ca_standardize()
#' @export
ca_standardize <- function(X, weighted_row = rep(1, nrow(X))){
  new_CA <- as.matrix(X) * weighted_row / sum(X * weighted_row)
  return(new_CA)
}
