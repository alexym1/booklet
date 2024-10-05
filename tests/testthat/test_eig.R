# Principal Component Analysis (PCA)
expected_df_eigs <- readRDS("data/eigs/expected_df_eigs.rds")
expected_gf_eigs <- readRDS("data/eigs/expected_gf_eigs.rds")
expected_df_weighted_eigs <- readRDS("data/eigs/expected_df_weighted_eigs.rds")
expected_gf_weighted_eigs <- readRDS("data/eigs/expected_gf_weighted_eigs.rds")

# Correspondence Analysis (CA)
expected_hf_weighted_eigs <- readRDS("data/eigs/expected_hf_weighted_eigs.rds")

df <- pca_standardize_norm(iris[, -5])
gf <- pca_standardize(iris[, -5])
hf <- ca_standardize(mtcars[, c(2, 8:11)])

test_that("Testing pca_eigen()", {
  expect_equal(pca_eigen(df), expected_df_eigs)
  expect_equal(pca_eigen(gf), expected_gf_eigs)
})

test_that("Testing pca_weighted_eigen()", {
  expect_equal(pca_weighted_eigen(df), expected_df_weighted_eigs)
  expect_equal(pca_weighted_eigen(gf), expected_gf_weighted_eigs)
})

test_that("Testing ca_weighted_eigen()", {
  expect_equal(ca_weighted_eigen(hf), expected_hf_weighted_eigs)
})
