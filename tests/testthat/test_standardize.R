# Principal Component Analysis (PCA)
expected_std_norm <- readRDS("data/standardize/expected_std_norm.rds")
expected_std_norm_no <- readRDS("data/standardize/expected_std_norm_no.rds")
expected_std <- readRDS("data/standardize/expected_std.rds")
expected_std_no <- readRDS("data/standardize/expected_std_no.rds")

# Correspondence analysis (CA)
expected_std_ca <- readRDS("data/standardize/expected_std_ca.rds")


test_that("pca_standardize_norm(X, center = TRUE, scale = TRUE)", {
  obs_std_norm <- pca_standardize_norm(iris[, -5], scale = TRUE)
  expect_equal(obs_std_norm, expected_std_norm)
})

test_that("pca_standardize_norm(X, center = TRUE, scale = FALSE)", {
  obs_std_norm_no <- pca_standardize_norm(iris[, -5], scale = FALSE)
  expect_equal(obs_std_norm_no, expected_std_norm_no)
})

test_that("pca_standardize(X, scale = TRUE)", {
  obs_std <- pca_standardize(iris[, -5], scale = TRUE)
  expect_equal(obs_std, expected_std)
})

test_that("pca_standardize(X, scale = FALSE)", {
  obs_std_no <- pca_standardize(iris[, -5], scale = FALSE)
  expect_equal(obs_std_no, expected_std_no)
})

test_that("ca_standardize(X, weighted_row = rep(1, nrow(X)))", {
  obs_std_ca <- ca_standardize(mtcars[, c(2, 8:11)])
  expect_equal(obs_std_ca, expected_std_ca)
})
