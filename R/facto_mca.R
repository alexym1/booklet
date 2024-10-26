#' Perform MCA with FactoMineR's style
#'
#' Return MCA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param ncp an integer, the number of components to keep (value set by default)
#' @param method a character string, the method to use for the MCA either "Indicator" or "burt"
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
facto_mca <- function(X, ncp = 2, method=c("Indicator", "burt"), row_sup=NULL, col_sup=NULL, weighted_row=NULL) {
  method <- match.arg(method)

  is_quali <- which(!unlist(lapply(X, is.numeric)))
  X[,is_quali] <- lapply(X[,is_quali, drop=FALSE], as.factor)

  Z_lst <- lapply(is_quali, function(column){
    one_hot_encoded <- model.matrix(~0+X[,column])
    colnames(one_hot_encoded) <- levels(X[,column])
    one_hot_encoded
  })

  Z <- do.call(cbind, Z_lst)
  new_ncp = min(ncp, ncol(Z) - length(is_quali))

  # Extract act + ind.act
  # res.mca <- CA(Ztot, ncp = min(ncp, ncol(Z) - length(act)), row.sup = ind.sup,
  # excl = excl, col.sup = col.sup, graph = FALSE, row.w = row.w)

  res_ca <- facto_ca(Z, ncp=ncp, row_sup=row_sup, col_sup=col_sup, weighted_row=weighted_row)

  res_mca <- list(
    eig = NULL,
    call = NULL,
    ind = NULL,
    var = NULL,
    svd = NULL
  )

  class(res_mca) <- c("MCA", "list")

  return(res_mca)
}
