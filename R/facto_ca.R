#' Perform CA with FactoMineR's style
#'
#' Return CA results with FactoMineR's style
#'
#' @param X a data frame with n rows (individuals) and p columns (numeric variables)
#' @param ncp an integer, the number of components to keep (value set by default)
#' @param row_sup a vector indicating the indexes of the supplementary rows
#' @param col_sup a vector indicating the indexes of the supplementary cols
#' @param weighted_row row weights
#'
#' @examples
#' library(FactoMineR2)
#' res <- facto_ca(X = mtcars[, c(2, 8:11)], ncp = 2)
#' @export
facto_ca <- function(X, ncp = 5, row_sup = NULL, col_sup = NULL, weighted_row = NULL) {
  if (!is.null(row_sup) & !is.null(col_sup)) {
    X_active <- X[-row_sup, -col_sup]
  } else if (!is.null(row_sup) & is.null(col_sup)) {
    X_active <- X[-row_sup, ]
  } else if (is.null(row_sup) & !is.null(col_sup)) {
    X_active <- X[, -col_sup]
  } else {
    X_active <- X
  }

  ncp <- min(c(ncp, ncol(X_active) - 1, nrow(X_active) - 1))

  if (is.null(weighted_row)) {
    weighted_row <- rep(1, nrow(X_active))
  }

  X_active_scaled <- ca_standardize(X_active, weighted_row)

  eigs <- ca_weighted_eigen(X_active_scaled)
  eigvalues <- eigs$values

  df_eigs <- data.frame(
    eigenvalue = eigvalues,
    `percentage of variance` = eigvalues / sum(eigvalues) * 100,
    `cumulative percentage of variance` = cumsum(eigvalues / sum(eigvalues)) * 100
  )

  rownames(df_eigs) <- paste0("dim ", 1:nrow(df_eigs))

  row_coords <- ca_row_coords(eigs)
  rownames(row_coords) <- rownames(X_active_scaled[["CA_scaled"]])

  row_cos2 <- ca_row_cos2(row_coords, X_active_scaled)
  row_contrib <- ca_row_contrib(row_coords, X_active_scaled, eigs)
  row_inertia <- ca_row_inertia(X_active_scaled)

  lst_row <- list(
    coord = row_coords[, 1:ncp],
    cos2 = row_cos2[, 1:ncp],
    contrib = row_contrib[, 1:ncp],
    inertia = as.vector(row_inertia)
  )

  col_coords <- ca_col_coords(eigs)
  colnames(col_coords) <- paste0("Dim ", 1:ncol(col_coords))

  col_cos2 <- ca_col_cos2(col_coords, X_active_scaled)
  col_contrib <- ca_col_contrib(col_coords, X_active_scaled, eigs)
  col_inertia <- ca_col_inertia(X_active_scaled)

  lst_col <- list(
    coord = col_coords[, 1:ncp],
    cos2 = col_cos2[, 1:ncp],
    contrib = col_contrib[, 1:ncp],
    inertia = as.vector(col_inertia)
  )

  lst_eigs <- eigs
  lst_eigs[["values"]] <- sqrt(lst_eigs[["values"]])

  res_ca <- list(
    eig = df_eigs[1:ncp,],
    call = list(
      X = X_active,
      marge.col = X_active_scaled[["weighted_col"]],
      marge.row = X_active_scaled[["weighted_row"]],
      ncp = ncp,
      row.w = weighted_row,
      excl = NULL,
      call = match.call(),
      Xtot = X,
      N = sum(rep(1, nrow(X_active)) * rowSums(X_active)),
      row.sup = row_sup,
      col.sup = col_sup
    ),
    row = lst_row,
    col = lst_col,
    svd = lst_eigs
  )

  if (!is.null(row_sup)) {
    if (!is.null(col_sup)) {
      X_sup <- X[row_sup, -col_sup]
    } else {
      X_sup <- X[row_sup, ]
    }

    X_row_sup <- ca_standardize_sup(X_sup, type = "row")
    row_sup_coords <- ca_row_sup_coords(X_row_sup, eigs)
    colnames(row_sup_coords) <- paste0("Dim ", 1:ncol(row_sup_coords))

    row_sup_cos2 <- ca_row_sup_cos2(row_sup_coords, X_row_sup, X_active_scaled)

    res_ca$row.sup <- list(
      coord = row_sup_coords[, 1:ncp],
      cos2 = row_sup_cos2[, 1:ncp]
    )
  }

  if (!is.null(col_sup)) {
    if (!is.null(row_sup)) {
      X_sup <- X[-row_sup, col_sup, drop = FALSE]
    } else {
      X_sup <- X[, col_sup, drop = FALSE]
    }

    X_col_sup <- ca_standardize_sup(X_sup, type = "col", weighted_row)
    col_sup_coords <- ca_col_sup_coords(X_col_sup, eigs)
    colnames(col_sup_coords) <- paste0("Dim ", 1:ncol(col_sup_coords))

    col_sup_cos2 <- ca_col_sup_cos2(col_sup_coords, X_col_sup, X_active_scaled)

    res_ca$col.sup <- list(
      coord = col_sup_coords[, 1:ncp],
      cos2 = col_sup_cos2[, 1:ncp]
    )
  }

  class(res_ca) <- c("CA", "list")

  return(res_ca)
}
