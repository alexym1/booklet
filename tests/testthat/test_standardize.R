expected_std_norm <- readRDS("data/standardize/expected_std_norm.rds")
expected_std_norm_no <- readRDS("data/standardize/expected_std_norm_no.rds")
expected_std <- readRDS("data/standardize/expected_std.rds")
expected_std_no <- readRDS("data/standardize/expected_std_no.rds")


test_that("standardize_norm(X, center = TRUE, scale = TRUE)", {
  obs_std_norm <- standardize_norm(iris[,-5], scale = TRUE)
  expect_identical(obs_std_norm, expected_std_norm)
})

test_that("standardize_norm(X, center = TRUE, scale = FALSE)", {
  obs_std_norm_no <- standardize_norm(iris[,-5], scale = FALSE)
  expect_identical(obs_std_norm_no, expected_std_norm_no)
})

test_that("standardize(X, scale = TRUE)", {
  obs_std <- standardize(iris[,-5], scale = TRUE)
  expect_identical(obs_std, expected_std)
})

test_that("standardize(X, scale = FALSE)", {
  obs_std_no <- standardize(iris[,-5], scale = FALSE)
  expect_identical(obs_std_no, expected_std_no)
})
