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
#' new_df <- apply(mtcars[, c(2, 8:11)], 2, as.character)
#' res <- facto_mca(X = as.data.frame(new_df), ncp = 2)
#' @export
facto_mca <- function(X, ncp = 5, row_sup = NULL, col_sup = NULL, weighted_row = NULL) {
  stopifnot("`X` must be a data.frame" = is.data.frame(X))

  if (!is.null(col_sup)) {
    act <- (1:ncol(X))[-col_sup]
  } else {
    act <- 1:ncol(X)
  }

  if (!is.null(row_sup)) {
    ind_act <- (1:nrow(X))[-row_sup]
  } else {
    ind_act <- 1:nrow(X)
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

  quali_idx <- which(unlist(lapply(X[,col_sup], is.factor)) | unlist(lapply(X[, col_sup], is.character)))

  if(length(quali_idx) == 0){
    quali_idx = NULL
  }

  quanti_idx <- which(unlist(lapply(X[,col_sup], is.numeric)) | unlist(lapply(X[, col_sup], is.integer)))

  if(length(quanti_idx) == 0){
    quanti_idx = NULL
  }

  Nj <- colSums(Z[ind_act,-supplementary_colums] * res_ca$call$row.w)
  N <- sum(Nj)/(ncol(X) - length(quali.sup) - length(quanti.sup))
  coeffs <- sqrt(Nj * ((N - 1)/(N - Nj)))

  res_mca <- list(
    eig = res_ca$eig,
    call = list(
      X = X,
      marge.col = res_ca$call$marge.col,
      marge.row = rep(1 / nrow(X), nrow(X)),
      ncp = ncp,
      row.w = rep(1, nrow(X)),
      excl = NULL,
      call = match.call(),
      Xtot = res_ca$call$Xtot,
      N = res_ca$call$N,
      row.sup = row_sup,
      col.sup = col_sup,
      quali = act,
      quali.sup = col_sup[as.vector(quali_idx)],
      quanti.sup = col_sup[as.vector(quanti_idx)]
    ),
    ind = list(
      coord = res_ca$row$coord,
      contrib = res_ca$row$contrib,
      cos2 = res_ca$row$cos2
    ),
    var = list(
      coord = res_ca$col$coord,
      contrib = res_ca$col$contrib,
      cos2 = res_ca$col$cos2,
      v.test = as.matrix(res_ca$col$coord * coeffs),
      eta2 = ca_col_eta2(contrib = res_ca$col$contrib, X = X[,act], eigs = res_ca$eig)
    ),
    svd = list(
      vs = res_ca$svd$values,
      V = res_ca$svd$vectors,
      U = res_ca$svd$U
    )
  )


  if(!is.null(row_sup)){
    res_mca$ind.sup <- list(
      coord = NULL,
      cos2 = NULL
    )
  }

  if(!is.null(quali_idx)){
    Nj <- colSums(Z[ind_act, supplementary_colums] * res_ca$call$row.w)
    coeffs <- sqrt(Nj * ((N - 1)/(N - Nj)))

    # X_qtl <- one_hot_encoding(X[ind_act, col_sup,drop=FALSE])
    # ni <- colSums(X_qtl * res_mca$call$row.w)

    # X_qtl <- X[ind_act, col_sup,drop=FALSE]
    # res_eta2 <- sapply(X_qtl, ca_quali_sup_eta2, x = res_mca$ind$coord, weights = res_mca$call$row.w)

    res_mca$quali.sup <- list(
      coord = res_ca$col.sup$coord,
      cos2 = res_ca$col.sup$cos2,
      v.test = as.matrix(res_ca$col.sup$coord * coeffs),
      eta2 = NULL # t(res_eta2)
    )
  }

  if(!is.null(quanti_idx)){
    U <- res_mca$svd$U
    df_quanti <- cbind.data.frame(U, X[,names(quanti_idx), drop = FALSE])
    weighted_cov <- cov.wt(df_quanti, cor=TRUE, wt = res_mca$call$row.w, method = "ML")
    coord_quanti_sup <- weighted_cov$cor[-(1:ncol(U)), 1:ncol(U), drop = FALSE]

    mca_var_sup_coords <- data.frame(t(coord_quanti_sup[1:ncp]))
    colnames(mca_var_sup_coords) <- paste0("Dim.", 1:ncp)
    rownames(mca_var_sup_coords) <- names(quanti_idx)

    res_mca$quanti.sup <- mca_var_sup_coords
  }

  class(res_mca) <- c("MCA", "list")

  return(res_mca)
}

ca_quali_sup_eta2 <- function(vec, x, weights) {
  compute_VB <- function(xx, tt, weights, ni) {
    col_sums <- colSums((tt * xx) * weights)
    return(sum((col_sums^2) / ni))
  }

  # tt <- one_hot_encoding(vec)
  tt <- tab.disjonctif(vec)
  ni <- colSums(tt * weights)
  VB_values <- lapply(as.data.frame(x), compute_VB, tt = tt, weights = weights, ni = ni)
  weighted_variance <- colSums(x * x * weights)
  return(unlist(VB_values) / weighted_variance)
}
