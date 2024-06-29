utils::globalVariables(c("."))

#' Data standardization
#'
#' Perform data standardization for multivariate exploratory data analysis.
#'
#' @param X matrix
#' @param center centering by the mean
#' @param scale scaling by the standard deviation
#' @param row.w an optional row weights (by default, a vector of 1 for uniform row weights); the weights are given only for the active individuals
#'
#' @details
#' Standardization depends on what you need to perform factor analysis. We
#' implemented two types:
#'
#' * \code{standardize}: standardization is performed by centering the
#' data matrix and dividing by the square root of the sum of squares of the
#' weights. This is the default standardization in FactoMineR::PCA(..., scale.unit = TRUE).
#'
#' * \code{standardize_norm}: standardization is performed by centering and scaling
#' the data matrix. (X - µ) / σ, where µ is the mean and σ is the standard deviation.
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize_norm()
#'
#' @export
standardize_norm <- function(X, center = TRUE, scale = TRUE) {
  X <- scale(X, center = center, scale = scale)
  return(X)
}

#' @rdname standardize_norm
#' @export
standardize <- function(X, scale = TRUE, row.w = NULL) {
  if (is.null(row.w)) {
    row.w <- rep(1, nrow(X))
  }

  weights <- row.w / sum(row.w)

  center <- weights %>%
    `/`(sum(weights)) %>%
    crossprod(as.matrix(X)) %>%
    as.vector()

  X <- t(t(as.matrix(X)) - center)


  if (scale) {
    std <- sqrt(as.vector(crossprod(weights / sum(weights), as.matrix(X^2))))
    std[std <= 1e-16] <- 1
    X <- t(t(X) / std)
  }

  return(X)
}
