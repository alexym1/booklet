#' Perform PCA with FactoMineR's style
#'
#' Return PCA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param ncp an integer, the number of components to keep (value set by default)
#' @param scale.unit a boolean, if TRUE (value set by default) then data are scaled to unit variance
#' @param ind_sup a vector indicating the indexes of the supplementary individuals
#' @param quanti_sup a vector indicating the indexes of the quantitative supplementary variables
#'
#' @examples
#' library(FactoMineR)
#' library(FactoMineR2)
#' data(decathlon)
#'
#' X <- decathlon[, -c(11:13)]
#' res <- facto_pca(X, ncp = 5, ind_sup = 1, quanti_sup = 10)
#' @export
facto_pca <- function(X, ncp = 5, scale.unit = TRUE, ind_sup = NULL, quanti_sup = NULL) {
  if (!is.null(ind_sup) & !is.null(quanti_sup)) {
    X_active <- X[-ind_sup, -quanti_sup]
  } else if (!is.null(ind_sup) & is.null(quanti_sup)) {
    X_active <- X[-ind_sup, ]
  } else if (is.null(ind_sup) & !is.null(quanti_sup)) {
    X_active <- X[, -quanti_sup]
  } else {
    X_active <- X
  }

  X_active_scaled <- standardize(X_active, scale = scale.unit)

  eigs <- get_weighted_eigen(X_active_scaled)
  eigvalues <- eigs$values
  eigvectors <- eigs$vectors

  df_eigs <- data.frame(
    eigenvalue = eigvalues,
    `percentage of variance` = eigvalues / sum(eigvalues) * 100,
    `cumulative percentage of variance` = cumsum(eigvalues / sum(eigvalues))
  )

  rownames(df_eigs) <- paste0("comp ", 1:nrow(df_eigs))

  ind_coords <- pca_ind_coords(X_active_scaled, eigvectors)
  ind_coords <- ind_coords[, 1:ncp]

  lst_ind <- list(
    coord = ind_coords,
    cos2 = pca_ind_cos2(ind_coords),
    contrib = pca_ind_contrib(ind_coords, eigvalues[1:ncp])
  )

  var_coords <- pca_var_coords(eigvalues, eigvectors)
  var_coords <- var_coords[, 1:ncp]

  lst_var <- list(
    coord = var_coords,
    cor = var_coords,
    cos2 = pca_var_cos2(var_coords),
    contrib = pca_var_contrib(var_coords)
  )

  weights <- rep(1, nrow(X_active)) / nrow(X_active)

  res_pca <- list(
    eig = df_eigs,
    var = lst_var,
    ind = lst_ind,
    call = list(
      row.w = weights,
      scale.unit = scale.unit,
      ncp = ncp
    )
  )

  if (!is.null(ind_sup)) {
    center <- colMeans(X_active)
    std <- sqrt(as.vector(crossprod(weights, as.matrix(X_active^2)) - center^2))

    if(!is.null(quanti_sup)){
      X_sup <- X[ind_sup, -quanti_sup]
    } else {
      X_sup <- X[ind_sup, ]
    }

    X_sup_scaled <- (X_sup - center) / std
    ind_sup_coords <- pca_ind_coords(X_sup_scaled, eigvectors)

    res_pca$ind.sup <- list(
      coord = ind_sup_coords[, 1:ncp],
      cos2 = pca_ind_cos2(ind_sup_coords)[, 1:ncp]
    )
  }

  if (!is.null(quanti_sup)) {

    if(!is.null(ind_sup)){
      X_sup <- X[-ind_sup, quanti_sup, drop = FALSE]
    } else {
      X_sup <- X[, quanti_sup, drop = FALSE]
    }

    center <- as.vector(crossprod(weights, as.matrix(X_sup)))
    std <- sqrt(as.vector(crossprod(weights, as.matrix(X_sup^2)) - center^2))
    X_sup_scaled <- (X_sup - center) / std

    A <- t(X_sup_scaled * weights)
    U <- FactoMineR::svd.triplet(X_active_scaled)$U
    var_sup_coords <- A %*% U

    res_pca$var.sup <- list(
      coord = var_sup_coords,
      cor = var_sup_coords,
      cos2 = pca_var_cos2(var_sup_coords)
    )
  }

  class(res_pca) <- c("PCA", "list")

  return(res_pca)
}
