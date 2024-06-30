library(dplyr)

df <- iris |>
  select(-Species)

test_that("standardize_norm(X, center = TRUE, scale = TRUE)", {
  obs <- standardize_norm(df, scale = TRUE)
  expected <- scale(df, center = TRUE, scale = TRUE)
  expect_identical(obs, expected)
})

test_that("standardize_norm(X, center = TRUE, scale = FALSE)", {
  obs <- standardize_norm(df, scale = FALSE)
  expected <- scale(df, center = TRUE, scale = FALSE)
  expect_identical(obs, expected)
})

test_that("standardize(X, scale = TRUE)", {
  df_active <- standardize(df, scale = TRUE)
  expect_identical(dim(df_active), dim(df))
})

test_that("standardize(X, scale = FALSE)", {
  df_active <- standardize(df, scale = FALSE)
  expect_identical(dim(df_active), dim(df))
})
