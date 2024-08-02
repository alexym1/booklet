X <- iris[, -5]
res_pca <- facto_pca(X, ncp = 3, ind_sup = 1, quanti_sup = 1)

test_that("Testing facto_pca(X, ncp = 3, ind_sup = 1, quanti_sup = 1)", {
  expect_identical(names(res_pca), c("eig", "var", "ind", "call", "ind.sup", "var.sup"))

  expect_equal(names(res_pca$eig), c("eigenvalue", "percentage.of.variance", "cumulative.percentage.of.variance"))
  expect_equal(nrow(res_pca$eig), 3)

  expect_identical(names(res_pca$var), c("coord", "cor", "cos2", "contrib"))
  expect_equal(nrow(res_pca$var$coord), 3)
  expect_equal(nrow(res_pca$var$cos2), 3)
  expect_equal(nrow(res_pca$var$contrib), 3)

  expect_identical(names(res_pca$ind), c("coord", "cos2", "contrib"))
  expect_equal(nrow(res_pca$ind$coord), 149)
  expect_equal(nrow(res_pca$ind$cos2), 149)
  expect_equal(nrow(res_pca$ind$contrib), 149)

  expect_identical(names(res_pca$call), c("row.w", "col.w", "scale.unit", "ncp"))

  expect_identical(names(res_pca$ind.sup), c("coord", "cos2"))
  expect_equal(length(res_pca$ind.sup$coord), 3)
  expect_equal(length(res_pca$ind.sup$coord), length(res_pca$ind.sup$cos2))

  expect_identical(names(res_pca$var.sup), c("coord", "cor", "cos2"))
  expect_identical(res_pca$var.sup$coord, res_pca$var.sup$cor)
  expect_equal(dim(res_pca$var.sup$coord), c(1, 3))
  expect_equal(dim(res_pca$var.sup$cos2), c(1, 3))
})
