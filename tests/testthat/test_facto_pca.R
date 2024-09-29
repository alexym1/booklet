expected_pca <- readRDS("data/pca/expected_pca.rds")

test_that("Testing facto_pca()", {
  obs_pca <- facto_pca(iris[, -5], ncp = 3, ind_sup = 1, quanti_sup = 1)
  expect_equal(obs_pca, expected_pca)
})
