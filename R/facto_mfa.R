#' Perform MFA with FactoMineR's style
#'
#' Return MFA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param groups a vector indicating the group of each variable
#' @param ncp an integer, the number of components to keep (value set by default)
#'
#' @examples
#' library(FactoMineR2)
#'
#' res <- facto_mfa(X = iris[, -c(5)], groups = c(2, 2), ncp = 2)
#' @export
facto_mfa <- function(X, groups, ncp = 2) {
  if (length(groups) != ncp) {
    stop("The number of groups must be equal to the number of components")
  }

  X_scaled <- pca_standardize(X, scale = TRUE)

  weights <- list()
  start_idx <- 1
  for (grp in groups) {
    end_idx <- start_idx + grp - 1
    res_pca <- facto_pca(X_scaled[, start_idx:end_idx], ncp = ncp, scale.unit = TRUE)
    weights <- c(weights, 1 / res_pca$eig$eigenvalue[1])
    start_idx <- end_idx + 1
  }

  weighted_col <- do.call(c, weights)
  res_mfa <- facto_pca(X_scaled, ncp = ncp, scale.unit = FALSE, weighted_col = weighted_col)

  class(res_mfa) <- c("MFA", "list")

  return(res_mfa)
}
