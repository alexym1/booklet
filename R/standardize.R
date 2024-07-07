#' Data standardization
#'
#' Perform data standardization for multivariate exploratory data analysis.
#'
#' @param X matrix
#' @param center centering by the mean
#' @param scale scaling by the standard deviation
#'
#' @details
#' Standardization depends on what you need to perform factor analysis. We
#' implemented two types:
#'
#' * \code{standardize}: standardization is performed by centering the
#' data matrix and dividing by the square root of the sum of squares of the
#' weights. This is the default standardization in `FactoMineR::PCA()`.
#'
#' * \code{standardize_norm}: standardization is performed by centering and scaling
#' the data matrix. (X - µ) / σ, where µ is the mean and σ is the standard deviation.
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize_norm() |>
#'   head()
#' @export
standardize_norm <- function(X, center = TRUE, scale = TRUE) {
  X_scaled <- scale(X, center = center, scale = scale)
  return(X)
}

#' @rdname standardize_norm
#' @export
standardize <- function(X, scale = TRUE) {
  row.w <- rep(1, nrow(X))
  weights <- row.w / sum(row.w)

  center <- crossprod(weights / sum(weights), as.matrix(X))
  X <- t(t(as.matrix(X)) - as.vector(center))

  if (scale) {
    std <- sqrt(as.vector(crossprod(weights / sum(weights), as.matrix(X^2))))
    std[std <= 1e-16] <- 1
    X <- t(t(X) / std)
  }

  return(X)
}
