df <- standardize_norm(iris[, -5])
gf <- standardize(iris[, -5])

df_eigs <- get_eigen(df)
gf_eigs <- get_weighted_eigen(gf)

df_coords <- pca_ind_coords(df_eigs)
gf_coords <- pca_ind_coords(gf_eigs)

df_cos2 <- pca_ind_cos2(df_coords)
gf_cos2 <- pca_ind_cos2(gf_coords)

df_contrib <- pca_ind_contrib(df_cos2, df_eigs, weighted_row = rep(1, nrow(df_cos2)))
gf_contrib <- pca_ind_contrib(gf_cos2, gf_eigs)

df_var_coords <- pca_var_coords(df_eigs)
gf_var_coords <- pca_var_coords(gf_eigs)

df_var_cor <- pca_var_cor(df_eigs)
gf_var_cor <- pca_var_cor(gf_eigs)

df_var_cos2 <- pca_var_cos2(df_var_coords)
gf_var_cos2 <- pca_var_cos2(gf_var_coords)

df_var_contrib <- pca_var_contrib(df_var_cos2, eigs=df_eigs)
gf_var_contrib <- pca_var_contrib(gf_var_cos2, eigs=gf_eigs)


test_that("Testing active individuals - coordinates", {
  expect_identical(is.matrix(df_coords), TRUE)
  expect_identical(colnames(df_coords), paste0("Dim.", 1:ncol(df)))
  expect_identical(nrow(df_coords), nrow(df))

  expect_identical(is.matrix(gf_coords), TRUE)
  expect_identical(colnames(gf_coords), paste0("Dim.", 1:ncol(gf)))
  expect_identical(nrow(gf_coords), nrow(gf))
})

test_that("Testing active individuals - cos2", {
  expect_identical(is.matrix(df_cos2), TRUE)
  expect_identical(colnames(df_cos2), paste0("Dim.", 1:ncol(df)))
  expect_identical(nrow(df_cos2), nrow(df))

  expect_identical(is.matrix(gf_cos2), TRUE)
  expect_identical(colnames(gf_cos2), paste0("Dim.", 1:ncol(gf)))
  expect_identical(nrow(gf_cos2), nrow(gf))
})

test_that("Testing active individuals - contribution", {
  expect_identical(is.matrix(df_contrib), TRUE)
  expect_identical(colnames(df_contrib), paste0("Dim.", 1:ncol(df)))
  expect_identical(nrow(df_contrib), nrow(df))

  expect_identical(is.matrix(gf_contrib), TRUE)
  expect_identical(colnames(gf_contrib), paste0("Dim.", 1:ncol(gf)))
  expect_identical(nrow(gf_contrib), nrow(gf))
})

test_that("Testing active variables - coordinates", {
  expect_identical(is.matrix(df_var_coords), TRUE)
  expect_identical(colnames(df_var_coords), paste0("Dim.", 1:ncol(df)))
  expect_identical(dim(df_var_coords), c(ncol(df), ncol(df)))
  expect_identical(dim(df_var_coords)[1], dim(df_coords)[2])

  expect_identical(is.matrix(gf_var_coords), TRUE)
  expect_identical(colnames(gf_var_coords), paste0("Dim.", 1:ncol(gf)))
  expect_identical(dim(gf_var_coords), c(ncol(gf), ncol(gf)))
  expect_identical(dim(gf_var_coords)[1], dim(gf_coords)[2])
})

test_that("Testing active variables - correlation", {
  expect_identical(is.matrix(df_var_cor), TRUE)
  expect_identical(colnames(df_var_cor), paste0("Dim.", 1:ncol(df)))
  expect_identical(dim(df_var_cor), c(ncol(df), ncol(df)))

  expect_identical(is.matrix(gf_var_cor), TRUE)
  expect_identical(colnames(gf_var_cor), paste0("Dim.", 1:ncol(gf)))
  expect_identical(dim(gf_var_cor), c(ncol(gf), ncol(gf)))
})

test_that("Testing active variables - cos2", {
  expect_identical(is.matrix(df_var_cos2), TRUE)
  expect_identical(colnames(df_var_cos2), paste0("Dim.", 1:ncol(df)))
  expect_identical(dim(df_var_cos2), c(ncol(df), ncol(df)))
  expect_identical(dim(df_var_cos2)[1], dim(df_cos2)[2])

  expect_identical(is.matrix(gf_var_cos2), TRUE)
  expect_identical(colnames(gf_var_cos2), paste0("Dim.", 1:ncol(gf)))
  expect_identical(dim(gf_var_cos2), c(ncol(gf), ncol(gf)))
  expect_identical(dim(gf_var_cos2)[1], dim(gf_cos2)[2])
})

test_that("Testing active variables - contrib", {
  expect_identical(is.matrix(df_var_contrib), TRUE)
  expect_identical(colnames(df_var_contrib), paste0("Dim.", 1:ncol(df)))
  expect_identical(dim(df_var_contrib), c(ncol(df), ncol(df)))
  expect_identical(dim(df_var_contrib)[1], dim(df_contrib)[2])

  expect_identical(is.matrix(gf_var_contrib), TRUE)
  expect_identical(colnames(gf_var_contrib), paste0("Dim.", 1:ncol(gf)))
  expect_identical(dim(gf_var_contrib), c(ncol(gf), ncol(gf)))
  expect_identical(dim(gf_var_contrib)[1], dim(gf_contrib)[2])
})
