#' Perform PCA with FactoMineR's style
#'
#' Return PCA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param ncp an integer, the number of components to keep (value set by default)
#' @param scale.unit a boolean, if TRUE (value set by default) then data are scaled to unit variance
#' @param ind_sup a vector indicating the indexes of the supplementary individuals
#' @param quanti_sup a vector indicating the indexes of the quantitative supplementary variables
#' @param weighted_col column weights
#'
#' @examples
#' library(FactoMineR)
#' library(FactoMineR2)
#' data(decathlon)
#'
#' X <- decathlon[, -c(11:13)]
#' res <- facto_pca(X, ncp = 5, ind_sup = 1, quanti_sup = 10)
#' @export
facto_mfa <- function(X, ncp = 5, ind_sup = NULL, quanti_sup = NULL, weighted_col = rep(1, ncol(X))) {

  X_scaled <- standardize(X, scale = TRUE)
  eig_1 <- get_weighted_eigen(X_scaled[,1:2])$values[1]
  eig_2 <- get_weighted_eigen(X_scaled[,3:4])$values[1]
  col.w = rep(c(1/eig_1, 1/eig_2), each = 2)
  row.w <- rep(1, nrow(X_scaled)) / nrow(X_scaled)
  res_pca <- facto_pca(X_scaled, ncp = 4, scale.unit = FALSE, weighted_col = col.w)

  class(res_pca) <- c("MFA", "list")

  return(res_pca)
}
