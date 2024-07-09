df <- standardize_norm(iris[, -5])
gf <- standardize(iris[, -5])

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

test_that("Testing get_weighted_eigen()", {
  df_eigs <- get_weighted_eigen(df)
  gf_eigs <- get_weighted_eigen(gf)

  expect_identical(names(df_eigs), c("values", "vectors"))
  expect_identical(length(df_eigs[[1]]), ncol(df))
  expect_identical(dim(df_eigs[[2]]), c(ncol(df), ncol(df)))

  expect_identical(names(gf_eigs), c("values", "vectors"))
  expect_identical(length(gf_eigs[[1]]), ncol(gf))
  expect_identical(dim(gf_eigs[[2]]), c(ncol(gf), ncol(gf)))
})
