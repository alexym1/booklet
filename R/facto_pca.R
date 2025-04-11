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
#' library(booklet)
#'
#' res <- facto_pca(iris[, -5], ncp = 2, ind_sup = 1, quanti_sup = 1)
#' @export
facto_pca <- function(X, ncp = 5, scale.unit = TRUE, ind_sup = NULL, quanti_sup = NULL, weighted_col = NULL) {
  if (!is.null(ind_sup) & !is.null(quanti_sup)) {
    X_active <- X[-ind_sup, -quanti_sup]
  } else if (!is.null(ind_sup) & is.null(quanti_sup)) {
    X_active <- X[-ind_sup, ]
  } else if (is.null(ind_sup) & !is.null(quanti_sup)) {
    X_active <- X[, -quanti_sup]
  } else {
    X_active <- X
  }

  if (is.null(weighted_col)) {
    weighted_col <- rep(1, ncol(X_active))
  }

  ncp <- min(ncp, ncol(X_active))

  X_active_scaled <- pca_standardize(X_active, scale = scale.unit)

  eigs <- pca_weighted_eigen(X_active_scaled, weighted_col = weighted_col)
  eigvalues <- eigs$values

  df_eigs <- data.frame(
    eigenvalue = eigvalues,
    `percentage of variance` = eigvalues / sum(eigvalues) * 100,
    `cumulative percentage of variance` = cumsum(eigvalues / sum(eigvalues)) * 100
  )

  rownames(df_eigs) <- paste0("comp ", 1:nrow(df_eigs))

  ind_coords <- pca_ind_coords(eigs)
  ind_cos2 <- pca_ind_cos2(ind_coords, weighted_col = rep(1, ncol(ind_coords)))
  ind_contrib <- pca_ind_contrib(ind_coords, eigs, weighted_row = rep(1, nrow(ind_coords)) / nrow(ind_coords))

  lst_ind <- list(
    coord = ind_coords[, 1:ncp],
    cos2 = ind_cos2[, 1:ncp],
    contrib = ind_contrib[, 1:ncp]
  )

  var_coords <- pca_var_coords(eigs)
  var_cor <- pca_var_cor(eigs)
  var_cos2 <- pca_var_cos2(var_coords)
  var_contrib <- pca_var_contrib(var_cos2, eigs, weighted_col)

  lst_var <- list(
    coord = var_coords[, 1:ncp],
    cor = var_cor[, 1:ncp],
    cos2 = var_cos2[, 1:ncp],
    contrib = var_contrib[, 1:ncp]
  )

  weights <- rep(1, nrow(X_active)) / nrow(X_active)
  weighted_col <- rep(1, ncol(X_active))

  lst_eigs <- eigs
  lst_eigs[["values"]] <- sqrt(lst_eigs[["values"]])

  center <- colMeans(X_active)
  std <- sqrt(as.vector(crossprod(weights, as.matrix(X_active^2)) - center^2))

  res_pca <- list(
    eig = df_eigs,
    var = lst_var,
    ind = lst_ind,
    svd = lst_eigs,
    call = list(
      row.w = weights,
      col.w = weighted_col,
      scale.unit = scale.unit,
      ncp = ncp,
      centre = as.vector(center),
      ecart.type = std,
      X = X,
      row.w.init = rep(1, nrow(X_active)),
      call = match.call()
    )
  )

  if (!is.null(ind_sup)) {
    if (!is.null(quanti_sup)) {
      X_sup <- X[ind_sup, -quanti_sup, drop = FALSE]
    } else {
      X_sup <- X[ind_sup, ]
    }

    X_sup_scaled <- t(t(t(t(X_sup) - center)) / std)
    ind_sup_coords <- as.data.frame(as.matrix(X_sup_scaled) %*% eigs$vectors)

    res_pca$ind.sup <- list(
      coord = ind_sup_coords[, 1:ncp],
      cos2 = pca_ind_cos2(ind_sup_coords)[, 1:ncp]
    )

    res_pca$call$ind.sup <- ind_sup
  }

  if (!is.null(quanti_sup)) {
    if (!is.null(ind_sup)) {
      X_sup <- X[-ind_sup, quanti_sup, drop = FALSE]
    } else {
      X_sup <- X[, quanti_sup, drop = FALSE]
    }

    X_sup_scaled <- pca_standardize(X_sup, scale = scale.unit)
    var_sup_coords <- as.data.frame(t(X_sup_scaled * weights) %*% eigs$U)

    res_pca$quanti.sup <- list(
      coord = var_sup_coords[, 1:ncp],
      cor = var_sup_coords[, 1:ncp],
      cos2 = pca_var_cos2(var_sup_coords)[, 1:ncp]
    )

    res_pca$call$quanti.sup <- X_sup
  }

  class(res_pca) <- c("PCA", "list")

  return(res_pca)
}
