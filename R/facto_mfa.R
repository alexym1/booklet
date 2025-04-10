#' Perform MFA with FactoMineR's style
#'
#' Return MFA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param groups a vector indicating the group of each variable
#' @param ncp an integer, the number of components to keep (value set by default)
#'
#' @importFrom stats setNames
#'
#' @examples
#' library(booklet)
#'
#' res <- facto_mfa(X = iris[, -c(5)], groups = c(2, 2), ncp = 2)
#' @export
facto_mfa <- function(X, groups, ncp = 2) {
  center_init <- colMeans(X)
  X_scaled <- pca_standardize(X, scale = TRUE)

  separate.analyses <- setNames(
    lapply(seq_along(groups), function(x) {
      list()
    }), paste0("Gr", seq_along(groups))
  )

  start_idx <- 1
  weighted_col <- NULL
  for (i in seq_along(groups)) {
    end_idx <- start_idx + groups[i] - 1
    separate.analyses[[i]] <- facto_pca(X_scaled[, start_idx:end_idx], ncp = ncp, scale.unit = TRUE)
    weights <- rep(1 / separate.analyses[[i]][["eig"]][["eigenvalue"]][1], groups[i])
    weighted_col <- c(weighted_col, weights)

    separate.analyses[[i]][["call"]][["X"]] <- X[, start_idx:end_idx]
    separate.analyses[[i]][["call"]][["centre"]] <- center_init[start_idx:end_idx]

    start_idx <- end_idx + 1
  }

  global_mfa <- facto_pca(X_scaled, ncp = ncp, scale.unit = FALSE, weighted_col = weighted_col)

  res_mfa <- list(
    separate.analyses = separate.analyses,
    global.pca = global_mfa,
    eig = global_mfa$eig
  )

  class(res_mfa) <- c("MFA", "list")

  return(res_mfa)
}
