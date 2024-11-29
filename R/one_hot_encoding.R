#' Perform One hot encoding
#'
#' Return a matrix with one hot encoding
#'
#' @param M a matrix
#'
#' @examples
#' library(FactoMineR2)
#'
#' new_matrix <- one_hot_encoding(iris)
#' @export
one_hot_encoding <- function(M){
  is_quali <- which(!unlist(lapply(M, is.numeric)))
  M[, is_quali] <- lapply(M[, is_quali, drop = FALSE], as.factor)

  M_lst <- lapply(is_quali, function(column) {
    one_hot_encoded <- model.matrix(~ 0 + M[, column])
    colnames(one_hot_encoded) <- levels(M[, column])
    one_hot_encoded
  })

  quali_vars <- do.call(cbind, M_lst)
  quanti_vars <- M[, !(1:ncol(M) %in% is_quali), drop = FALSE]
  new_M <- cbind(quanti_vars, quali_vars)

  return(new_M)
}
