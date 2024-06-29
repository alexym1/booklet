library(dplyr)

df <- iris |>
  select(-Species) |>
  standardize_norm()

gf <- iris |>
  select(-Species) |>
  standardize()

test_that("Testing get_eigen()", {
  df_eigs <- get_eigen(df)
  gf_eigs <- get_eigen(gf)

  expect_identical(names(df_eigs), c("values", "vectors"))
  expect_identical(length(df_eigs[[1]]), ncol(df))
  expect_identical(dim(df_eigs[[2]]), c(ncol(df), ncol(df)))

  expect_identical(names(gf_eigs), c("values", "vectors"))
  expect_identical(length(gf_eigs[[1]]), ncol(gf))
  expect_identical(dim(gf_eigs[[2]]), c(ncol(gf), ncol(gf)))
})

test_that("Testing get_eigen() with ponderation", {
  row.w <- rep(1, nrow(df))
  weights <- row.w / sum(row.w)

  df_eigs <- get_eigen(df, weights)
  gf_eigs <- get_eigen(gf, weights)

  expect_identical(names(df_eigs), c("values", "vectors"))
  expect_identical(length(df_eigs[[1]]), ncol(df))
  expect_identical(dim(df_eigs[[2]]), c(ncol(df), ncol(df)))

  expect_identical(names(gf_eigs), c("values", "vectors"))
  expect_identical(length(gf_eigs[[1]]), ncol(gf))
  expect_identical(dim(gf_eigs[[2]]), c(ncol(gf), ncol(gf)))
})

test_that("Testing eigvalues()", {
  df_eigs <- eigvalues(df)
  gf_eigs <- eigvalues(gf)
  expect_identical(length(df_eigs), ncol(df))
  expect_identical(length(gf_eigs), ncol(df))
})

test_that("Testing eigvalues() with ponderation", {
  row.w <- rep(1, nrow(df))
  weights <- row.w / sum(row.w)

  df_eigs <- eigvalues(df, weights)
  gf_eigs <- eigvalues(gf, weights)
  expect_identical(length(df_eigs), ncol(df))
  expect_identical(length(gf_eigs), ncol(df))
})

test_that("Testing eigvectors()", {
  df_eigs <- eigvectors(df)
  gf_eigs <- eigvectors(gf)
  expect_identical(dim(df_eigs), c(ncol(df), ncol(df)))
  expect_identical(dim(gf_eigs), c(ncol(gf), ncol(gf)))
})

test_that("Testing eigvectors() with ponderation", {
  row.w <- rep(1, nrow(df))
  weights <- row.w / sum(row.w)

  df_eigs <- eigvectors(df, weights)
  gf_eigs <- eigvectors(gf, weights)
  expect_identical(dim(df_eigs), c(ncol(df), ncol(df)))
  expect_identical(dim(gf_eigs), c(ncol(gf), ncol(gf)))
})
