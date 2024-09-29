expected_df_eigs <- readRDS("data/eigs/expected_df_eigs.rds")
expected_gf_eigs <- readRDS("data/eigs/expected_gf_eigs.rds")
expected_df_weighted_eigs <- readRDS("data/eigs/expected_df_weighted_eigs.rds")
expected_gf_weighted_eigs <- readRDS("data/eigs/expected_gf_weighted_eigs.rds")

df <- standardize_norm(iris[, -5])
gf <- standardize(iris[, -5])

test_that("Testing get_eigen()", {
  expect_equal(get_eigen(df), expected_df_eigs)
  expect_equal(get_eigen(gf), expected_gf_eigs)
})

test_that("Testing get_weighted_eigen()", {
  expect_equal(get_weighted_eigen(df), expected_df_weighted_eigs)
  expect_equal(get_weighted_eigen(gf), expected_gf_weighted_eigs)
})
