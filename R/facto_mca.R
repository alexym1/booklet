#' Perform MCA with FactoMineR's style
#'
#' Return MCA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param ncp an integer, the number of components to keep (value set by default)
#' @param row_sup supplementary rows
#' @param col_sup supplementary columns
#' @param weighted_row row weights
#'
#'
#' @examples
#' library(FactoMineR2)
#'
#' res <- facto_mca(X = mtcars[, c(2, 8:11)], ncp = 2)
#' @export
facto_mca <- function(X, ncp = 5, row_sup = NULL, col_sup = NULL, weighted_row = NULL) {
  method <- match.arg(method)

  if (!is.null(col_sup)) {
    act <- (1:ncol(X))[-col_sup]
  } else {
    act <- 1:ncol(X)
  }

  Y <- X[, act]
  Z <- one_hot_encoding(Y)

  supplementary_colums <- NULL
  if (!is.null(quali.sup)) {
    new_Z <- one_hot_encoding(X[, col_sup])
    Z <- cbind.data.frame(Z, new_Z)
    supplementary_colums <- (ncol(new_Z) + 1):ncol(Z)
  }

  res_ca <- facto_ca(
    X = Z,
    ncp = min(ncp, ncol(Z) - length(act)),
    col_sup = supplementary_colums,
    row_sup = row_sup,
    weighted_row = weighted_row
  )

  res_mca <- list(
    eig = res_ca$eig,
    call = list(
      X = X,
      marge.col = NULL,
      marge.row = rep(1 / nrow(X), nrow(X)),
      ncp = ncp,
      row.w = rep(1, nrow(X)),
      excl = NULL,
      call = match.call(),
      Xtot = NULL,
      N = NULL,
      col.sup = NULL,
      quali = act
    ),
    ind = NULL,
    var = NULL,
    svd = NULL
  )

  # quali.sup
  # quanti.sup

  class(res_mca) <- c("MCA", "list")

  return(res_mca)
}
