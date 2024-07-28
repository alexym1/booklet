#' Data standardization
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
  X <- scale(X, center = center, scale = scale)
  return(X)
}

#' @rdname standardize_norm
#' @export
standardize <- function(X, scale = TRUE, weighted_row = rep(1, nrow(X)) / nrow(X)) {
  weighted_ind <- weighted_row / sum(weighted_row)

  center <- crossprod(weighted_ind, as.matrix(X))
  X <- t(t(as.matrix(X)) - as.vector(center))

  if (scale) {
    std <- sqrt(as.vector(crossprod(weighted_ind, as.matrix(X^2))))
    std[std <= 1e-16] <- 1
    X <- t(t(X) / std)
  }

  return(X)
}
