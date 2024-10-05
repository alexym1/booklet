# Observed results
X_scaled <- ca_standardize(mtcars[, c(2, 8:11)])
ca_eigs <- ca_weighted_eigen(X_scaled)

row_coords <- ca_row_coords(ca_eigs)
row_cos2 <- ca_row_cos2(row_coords, X_scaled)
row_contrib <- ca_row_contrib(row_coords, X_scaled, ca_eigs)
row_inertia <- ca_row_inertia(X_scaled)

col_coords <- ca_col_coords(ca_eigs)
col_cos2 <- ca_col_cos2(col_coords, X_scaled)
col_contrib <- ca_col_contrib(col_coords, X_scaled, ca_eigs)
col_inertia <- ca_col_inertia(X_scaled)


# Expected results
expected_ca_row_coords <- readRDS("data/ca/expected_ca_row_coords.rds")
expected_ca_row_cos2 <- readRDS("data/ca/expected_ca_row_cos2.rds")
expected_ca_row_contrib <- readRDS("data/ca/expected_ca_row_contrib.rds")
expected_ca_row_inertia <- readRDS("data/ca/expected_ca_row_inertia.rds")

expected_ca_col_coords <- readRDS("data/ca/expected_ca_col_coords.rds")
expected_ca_col_cos2 <- readRDS("data/ca/expected_ca_col_cos2.rds")
expected_ca_col_contrib <- readRDS("data/ca/expected_ca_col_contrib.rds")
expected_ca_col_inertia <- readRDS("data/ca/expected_ca_col_inertia.rds")


test_that("Testing ca_row_coords()", {
  expect_equal(row_coords, expected_ca_row_coords)
})

test_that("Testing ca_row_cos2()", {
  expect_equal(row_cos2, expected_ca_row_cos2)
})

test_that("Testing ca_row_contrib()", {
  expect_equal(row_contrib, expected_ca_row_contrib)
})

test_that("Testing ca_row_inertia()", {
  expect_equal(row_inertia, expected_ca_row_inertia)
})

test_that("Testing ca_col_coords()", {
  expect_equal(col_coords, expected_ca_col_coords)
})

test_that("Testing ca_col_cos2()", {
  expect_equal(col_cos2, expected_ca_col_cos2)
})

test_that("Testing ca_col_contrib()", {
  expect_equal(col_contrib, expected_ca_col_contrib)
})

test_that("Testing ca_col_inertia()", {
  expect_equal(col_inertia, expected_ca_col_inertia)
})
