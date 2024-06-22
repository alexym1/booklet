library(dplyr)

df <- iris |>
  select(-Species)

test_that("standardize(X, type = 'norm')", {
  df_active <- standardize(df, type = 'norm')
  expect_identical(dim(df_active), dim(df))
})

test_that("standardize(X, type = 'FactoMineR')", {
  df_active <- standardize(df, type = 'FactoMineR', scale.unit = TRUE)
  expect_identical(dim(df_active), dim(df))

})

test_that("standardize(X, type = 'FactoMineR', scale.unit = FALSE)", {
  df_active <- standardize(df, type = 'FactoMineR', scale.unit = FALSE)
  expect_identical(dim(df_active), dim(df))
})

test_that("standardize(..., scale.unit = FALSE) vs. standardize(..., scale.unit = TRUE)", {
  df_active <- standardize(df, type = 'FactoMineR', scale.unit = TRUE)
  df_active2 <- standardize(df, type = 'FactoMineR', scale.unit = FALSE)
  expect_true(all(df_active != df_active2))
})

test_that("Wrong additional arguments: standardize(X, type = 'FactoMineR', scale.unit = `FALSE`)", {
  expect_error(
    standardize(df, type = 'FactoMineR', scale.unit = "FALSE"),
    "The argument `scale.unit` must be a logical value.")
})
