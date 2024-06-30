library(dplyr)
library(magrittr)

df <- iris |>
  select(-Species) |>
  standardize_norm()

gf <- iris |>
  select(-Species) |>
  standardize()

df_eigenvalues <- df |>
  get_eigen() |>
  extract2(1)

gf_eigenvalues <- gf |>
  get_eigen() |>
  extract2(1)

df_eigvectors <- df |>
  get_eigen() |>
  extract2(2)

gf_eigvectors <- gf |>
  get_weighted_eigen() |>
  extract2(2)

df_coords <- df |>
  pca_ind_coords(df_eigvectors)

gf_coords <- gf |>
  pca_ind_coords(gf_eigvectors)

test_that("Testing active individuals - coordinates", {
  expect_identical(is.matrix(df_coords), TRUE)
  expect_identical(colnames(df_coords), paste0("Dim.", 1:ncol(df)))
  expect_identical(nrow(df_coords), nrow(df))

  expect_identical(is.matrix(gf_coords), TRUE)
  expect_identical(colnames(gf_coords), paste0("Dim.", 1:ncol(gf)))
  expect_identical(nrow(gf_coords), nrow(gf))
})

test_that("Testing active individuals - cos2", {
  df_cos2 <- pca_ind_cos2(df_coords)
  gf_cos2 <- pca_ind_cos2(gf_coords)

  expect_identical(is.matrix(df_cos2), TRUE)
  expect_identical(colnames(df_cos2), paste0("Dim.", 1:ncol(df)))
  expect_identical(nrow(df_cos2), nrow(df))

  expect_identical(is.matrix(gf_cos2), TRUE)
  expect_identical(colnames(gf_cos2), paste0("Dim.", 1:ncol(gf)))
  expect_identical(nrow(gf_cos2), nrow(gf))
})

test_that("Testing active individuals - contribution", {
  df_contrib <- pca_ind_contrib(df_coords, df_eigenvalues)
  gf_contrib <- pca_ind_contrib(gf_coords, gf_eigenvalues)

  expect_identical(is.matrix(df_contrib), TRUE)
  expect_identical(colnames(df_contrib), paste0("Dim.", 1:ncol(df)))
  expect_identical(nrow(df_contrib), nrow(df))

  expect_identical(is.matrix(gf_contrib), TRUE)
  expect_identical(colnames(gf_contrib), paste0("Dim.", 1:ncol(gf)))
  expect_identical(nrow(gf_contrib), nrow(gf))
})


test_that("Testing active variables - coordinates", {
  df_coords <- pca_var_coords(df_eigenvalues, df_eigvectors)
  gf_coords <- pca_var_coords(gf_eigenvalues, gf_eigvectors)

  expect_identical(is.matrix(df_coords), TRUE)
  expect_identical(colnames(df_coords), paste0("Dim.", 1:ncol(df)))
  expect_identical(dim(df_coords), c(ncol(df), ncol(df)))
  expect_identical(dim(df_coords)[1], dim(df_coords)[2])

  expect_identical(is.matrix(gf_coords), TRUE)
  expect_identical(colnames(gf_coords), paste0("Dim.", 1:ncol(gf)))
  expect_identical(dim(gf_coords), c(ncol(gf), ncol(gf)))
  expect_identical(dim(gf_coords)[1], dim(gf_coords)[2])
})

test_that("Testing active variables - cos2", {
  df_cos2 <- df_eigenvalues |>
    pca_var_coords(df_eigvectors) |>
    pca_var_cos2()

  gf_cos2 <- gf_eigenvalues |>
    pca_var_coords(gf_eigvectors) |>
    pca_var_cos2()

  expect_identical(is.matrix(df_cos2), TRUE)
  expect_identical(colnames(df_cos2), paste0("Dim.", 1:ncol(df)))
  expect_identical(dim(df_cos2), c(ncol(df), ncol(df)))
  expect_identical(dim(df_cos2)[1], dim(df_cos2)[2])

  expect_identical(is.matrix(gf_cos2), TRUE)
  expect_identical(colnames(gf_cos2), paste0("Dim.", 1:ncol(gf)))
  expect_identical(dim(gf_cos2), c(ncol(gf), ncol(gf)))
  expect_identical(dim(gf_cos2)[1], dim(gf_cos2)[2])
})

test_that("Testing active variables - contrib", {
  df_contrib <- df_eigenvalues |>
    pca_var_coords(df_eigvectors) |>
    pca_var_contrib()

  gf_contrib <- gf_eigenvalues |>
    pca_var_coords(gf_eigvectors) |>
    pca_var_contrib()

  expect_identical(is.matrix(df_contrib), TRUE)
  expect_identical(colnames(df_contrib), paste0("Dim.", 1:ncol(df)))
  expect_identical(dim(df_contrib), c(ncol(df), ncol(df)))
  expect_identical(dim(df_contrib)[1], dim(df_contrib)[2])

  expect_identical(is.matrix(gf_contrib), TRUE)
  expect_identical(colnames(gf_contrib), paste0("Dim.", 1:ncol(gf)))
  expect_identical(dim(gf_contrib), c(ncol(gf), ncol(gf)))
  expect_identical(dim(gf_contrib)[1], dim(gf_contrib)[2])
})
