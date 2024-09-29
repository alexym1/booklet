# Observed results
df <- standardize_norm(iris[, -5])
gf <- standardize(iris[, -5])

df_eigs <- get_eigen(df)
gf_eigs <- get_weighted_eigen(gf)

df_coords <- pca_ind_coords(df_eigs)
gf_coords <- pca_ind_coords(gf_eigs)

df_var_coords <- pca_var_coords(df_eigs)
gf_var_coords <- pca_var_coords(gf_eigs)

df_var_cos2 <- pca_var_cos2(df_var_coords)
gf_var_cos2 <- pca_var_cos2(gf_var_coords)

# Expected results
expected_df_ind_coords <- readRDS("data/pca/expected_df_ind_coords.rds")
expected_gf_ind_coords <- readRDS("data/pca/expected_gf_ind_coords.rds")
expected_df_ind_cos2 <- readRDS("data/pca/expected_df_ind_cos2.rds")
expected_gf_ind_cos2 <- readRDS("data/pca/expected_gf_ind_cos2.rds")
expected_df_ind_contrib <- readRDS("data/pca/expected_df_ind_contrib.rds")
expected_gf_ind_contrib <- readRDS("data/pca/expected_gf_ind_contrib.rds")

expected_df_var_coords <- readRDS("data/pca/expected_df_var_coords.rds")
expected_gf_var_coords <- readRDS("data/pca/expected_gf_var_coords.rds")
expected_df_var_cos2 <- readRDS("data/pca/expected_df_var_cos2.rds")
expected_gf_var_cos2 <- readRDS("data/pca/expected_gf_var_cos2.rds")
expected_df_var_contrib <- readRDS("data/pca/expected_df_var_contrib.rds")
expected_gf_var_contrib <- readRDS("data/pca/expected_gf_var_contrib.rds")


test_that("Testing active individuals - coordinates", {
  expect_identical(df_coords, expected_df_ind_coords)
  expect_identical(gf_coords, expected_gf_ind_coords)
})

test_that("Testing active individuals - cos2", {
  expect_identical(pca_ind_cos2(df_coords), expected_df_ind_cos2)
  expect_identical(pca_ind_cos2(gf_coords), expected_gf_ind_cos2)
})

test_that("Testing active individuals - contribution", {
  expect_identical(pca_ind_contrib(df_coords, df_eigs), expected_df_ind_contrib)
  expect_identical(pca_ind_contrib(gf_coords, gf_eigs), expected_gf_ind_contrib)
})

test_that("Testing active variables - coordinates", {
  expect_identical(df_var_coords, expected_df_var_coords)
  expect_identical(gf_var_coords, expected_gf_var_coords)
})

test_that("Testing active variables - correlation", {
  expect_identical(pca_var_cor(df_eigs), -1 * expected_df_var_coords)
  expect_identical(pca_var_cor(gf_eigs), -1 * expected_gf_var_coords)
})

test_that("Testing active variables - cos2", {
  expect_identical(df_var_cos2, expected_df_var_cos2)
  expect_identical(gf_var_cos2, expected_gf_var_cos2)
})

test_that("Testing active variables - contrib", {
  expect_identical(pca_var_contrib(df_var_cos2, df_eigs), expected_df_var_contrib)
  expect_identical(pca_var_contrib(gf_var_cos2, gf_eigs), expected_gf_var_contrib)
})
