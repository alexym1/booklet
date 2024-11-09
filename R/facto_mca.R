#' Perform MCA with FactoMineR's style
#'
#' Return MCA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param ncp an integer, the number of components to keep (value set by default)
#' @param method a character string, the method to use for the MCA either "indicator" or "burt"
#' @param row_sup supplementary rows
#' @param col_sup supplementary columns
#' @param weighted_row row weights
#'
#' @importFrom stats setNames
#'
#' @examples
#' library(FactoMineR2)
#'
#' res <- facto_mca(X=mtcars[,c(2,8:11)], groups = c(2, 2), ncp = 2)
#' @export
facto_mca <- function(X, ncp = 5, method=c("indicator", "burt"), row_sup=NULL, col_sup=NULL, weighted_row=NULL) {
  method <- match.arg(method)

  if (!is.null(col_sup)){
    act <- (1:ncol(X))[-col_sup]
  } else {
    act <- 1:ncol(X)
  }

  Y <- X[,act]

  is_quali <- which(!unlist(lapply(Y, is.numeric)))
  Y[,is_quali] <- lapply(Y[,is_quali, drop=FALSE], as.factor)

  Z_lst <- lapply(is_quali, function(column){
    one_hot_encoded <- model.matrix(~0+Y[,column])
    colnames(one_hot_encoded) <- levels(Y[,column])
    one_hot_encoded
  })

  Z <- do.call(cbind, Z_lst)

  res_ca <- switch(
    method,
    "indicator" = facto_ca(Z, ncp = min(ncp, ncol(Z)-length(act)), row_sup = row_sup, col_sup = col_sup, weighted_row = weighted_row),
    "burt" = facto_ca(Z, ncp = ncol(Z)-length(act), row_sup = row_sup, col_sup = col_sup, weighted_row = weighted_row)
  )

  res_mca <- list(
    eig = NULL,
    call = list(
      X = X,
      marge.col = NULL,
      marge.row = rep(1/nrow(X), nrow(X)),
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
