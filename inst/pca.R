#' Principal Component Analysis (PCA)
#'
#' Perform PCA.
#'
#' @importFrom magrittr extract
#'
#' @param X standardized matrix
#' @param sup.ind supplementary individuals
#' @param sup.var supplementary variables
#'
#' @examples
#' library(factoMineR2)
#'
#' iris %>%
#'  standardize(type = "norm")
#'
#' @export
pca <- function(X, sup.ind = NULL, sup.var = NULL) {

  # X_active <- tryCatch(X[-sup.ind,], error = function(e) X)
  X_active <- tryCatch(X[, -sup.var], error = function(e) X)

  eigs <- X_active %>%
    get_eig()

  eig <- eigs %>%
    extract(1) %>%
    as.data.frame() %>%
    rename(eigenvalue = values) %>%
    mutate(
      `percentage of variance` = eigenvalue / sum(eigenvalue),
      `cumulative percentage of variance` = `percentage of variance` * 100
    )

  ind_coords <- X_active %*% eigs$vectors
  ind_cos2 <- ind_coords^2 / rowSums(ind_coords^2)
  ind_contrib <- 100 * (ind_coords^2) / colSums(ind_coords^2)

  var_coords <- (eigs %>% extract2(2)) %*% (eigs$values %>% sqrt() %>% diag)
  var_cos2 <- var_coords^2 / rowSums(var_coords^2)
  var_contrib <- 100 * var_coords^2 / colSums(var_coords^2)
  var_cor <- NULL


  res_pca <- list(
    eig = eig,
    ind = list(
      coord = ind_coords,
      cos2 = ind_cos2,
      contrib = ind_contrib
    ),
    var = list(
      coord = var_coords,
      cor = var_cor,
      cos2 = var_cos2,
      contrib = var_contrib
    )
  )

  # X_sup <- if (length(sup.ind) > 0) { X[sup.ind, ,drop = FALSE] } else {X}
  # X_sup <- if (length(sup.var) > 0) { X_sup[, sup.var] } else {X_sup}

  if(length(sup.ind) > 0) {
    sup_ind_coords <- X_sup %*% (eigs %>% extract2(2))
    sup_ind_cos2 <- sup_ind_coords ** 2 / rowSums(sup_ind_coords ** 2)

    res_pca$ind.sup <- list(
      coord = sup_ind_coords,
      cos2 = sup_ind_cos2
    )
  }

  if(length(sup.var) > 0){
    sup_var_coords <- NULL
    sup_var_cos2 <- NULL

    res_pca$var.sup <- list(
      coord = sup_var_coords,
      cos2 = sup_var_cos2
    )
  }

  return(res_pca)

}
