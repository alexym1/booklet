#' Data standardization for PCA
#'
#' Perform data standardization for multivariate exploratory data analysis.
#'
#' @param X matrix
#' @param center centering by the mean
#' @param scale scaling by the standard deviation
#' @param weighted_row row weights
#'
#' @details
#' Standardization depends on what you need to perform factor analysis. Two
#' methods are implemented:
#'
#' * \code{standardize}: standardization is performed by centering the
#' data matrix and dividing by the square root of the sum of squares of the
#' weights. This is the same method used in `FactoMineR::PCA()`.
#'
#' * \code{standardize_norm}: standardization is performed by centering and scaling
#' the data matrix. (X - µ) / S, where µ is the mean and S is the standard deviation.
#'
#' @examples
#' library(booklet)
#'
#' iris[, -5] |>
#'   pca_standardize_norm() |>
#'   head()
#' @export
pca_standardize_norm <- function(X, center = TRUE, scale = TRUE) {
  X_scaled <- scale(X, center = center, scale = scale)
  df_X <- as.data.frame(X_scaled)
  return(df_X)
}

#' @rdname pca_standardize_norm
#' @export
pca_standardize <- function(X, scale = TRUE, weighted_row = rep(1, nrow(X)) / nrow(X)) {
  weighted_ind <- weighted_row / sum(weighted_row)

  center <- crossprod(weighted_ind, as.matrix(X))
  X <- t(t(as.matrix(X)) - as.vector(center))

  if (scale) {
    std <- sqrt(as.vector(crossprod(weighted_ind, as.matrix(X^2))))
    std[std <= 1e-16] <- 1
    X <- t(t(X) / std)
  }

  X <- as.data.frame(X)

  return(X)
}
