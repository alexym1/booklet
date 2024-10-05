# Observed results
X_scaled <- ca_standardize(mtcars[, c(2, 8:11)])
ca_eigs <- ca_weighted_eigen(X_scaled)
row_coords <- ca_row_coords(ca_eigs)
row_cos2 <- ca_row_cos2(row_coords, X_scaled)
row_contrib <- ca_row_contrib(row_coords, X_scaled, ca_eigs)
row_inertia <- ca_row_inertia(X_scaled)


# Expected results
expected_ca_row_coords <- readRDS("data/ca/expected_ca_row_coords.rds")
expected_ca_row_cos2 <- readRDS("data/ca/expected_ca_row_cos2.rds")
expected_ca_row_contrib <- readRDS("data/ca/expected_ca_row_contrib.rds")
expected_ca_row_inertia <- readRDS("data/ca/expected_ca_row_inertia.rds")


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
