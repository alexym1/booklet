utils::globalVariables(c("."))

#' Data standardization
#'
#' Perform data standardization for multivariate exploratory data analysis.
#'
#' @param X matrix
#' @param type nature of standardization
#' @param ... additional arguments
#'
#' @details
#' Standardization depends on what you need to perform factor analysis. Actually,
#' there are only two types:
#'
#' * \code{type = "FactoMineR"}: standardization is performed by centering the
#' data matrix and dividing by the square root of the sum of squares of the
#' weights. This is the default standardization in FactoMineR::PCA(..., scale.unit = TRUE).
#'
#' * \code{type = "norm"}: standardization is performed by centering and scaling
#' the data matrix. This is the default standardization in R.
#'
#' @examples
#' library(FactoMineR2)
#'
#' iris[, -5] |>
#'   standardize(type = "norm")
#'
#' @export
standardize <- function(X, type = c("FactoMineR", "norm"), ...) {
  type <- match.arg(type)
  args <- list(...)

  if ("scale.unit" %in% names(args)) {
    if (!is.logical(args$scale.unit)) {
      stop("The argument `scale.unit` must be a logical value.")
    }
  } else {
    args$scale.unit <- TRUE
  }

  if (type == "norm") {
    X <- scale(X, center = TRUE, scale = TRUE)
  }

  if (type == "FactoMineR") {
    weights <- rep(1, times = nrow(X)) / sum(rep(1, times = nrow(X)))

    center <- weights %>%
      crossprod(as.matrix(X)) %>%
      as.vector()

    X <- t(t(as.matrix(X)) - center)

    if (args$scale.unit == TRUE) {
      std <- X^2 %>%
        as.matrix() %>%
        crossprod(weights / sum(weights), .) %>%
        as.vector() %>%
        sqrt()

      std[std <= 1e-16] <- 1

      X <- t(t(X) / std)
    }
  }

  return(X)
}
