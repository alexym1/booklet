#' Perform PCA with FactoMineR's style
#'
#' Return PCA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param group a vector indicating the group of each variable
#' @param ncp an integer, the number of components to keep (value set by default)
#'
#' @examples
#' library(FactoMineR2)
#'
#' res <- facto_pca(X=iris[,-c(5)], ncp = 2)
#' @export
facto_mfa <- function(X, group, ncp = 2) {

  X <- iris[,-c(5)]
  X_scaled <- standardize(X, scale = TRUE)
  res_pca_1 <- facto_pca(X_scaled[,1:2], ncp = 2, scale.unit = TRUE)
  res_pca_2 <- facto_pca(X_scaled[,3:4], ncp = 2, scale.unit = TRUE)
  col.w = rep(c(1/res_pca_1$eig$eigenvalue[1], 1/res_pca_2$eig$eigenvalue[1]), each = 2)
  res_pca <- facto_pca(X_scaled, ncp = ncp, scale.unit = FALSE, weighted_col = col.w)

  class(res_pca) <- c("MFA", "list")

  return(res_pca)
}
