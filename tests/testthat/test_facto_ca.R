expected_ca <- readRDS("data/ca/expected_ca.rds")

test_that("Testing facto_ca()", {
  obs_ca <- facto_ca(mtcars, row_sup = 5:8, col_sup = c(1,3:7))
  expect_equal(obs_ca, expected_ca)
})
