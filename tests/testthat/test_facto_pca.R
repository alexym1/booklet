library(FactoMineR)

data("decathlon")
X <- decathlon[, -c(11:13)]

test_that("Testing facto_pca()", {
  observed_pca <- facto_pca(X, ind_sup = 1, quanti_sup = 10)
  expected_pca <- PCA(X, ind.sup = 1, quanti.sup = 10, graph = FALSE)

  # Eig
  expect_equal(observed_pca$eig[, 1], as.vector(expected_pca$eig[, 1]))
  expect_equal(observed_pca$eig[, 2], as.vector(expected_pca$eig[, 2]))
  expect_equal(observed_pca$eig[, 3], as.vector(expected_pca$eig[, 3]))

  # Active individuals
  expect_equivalent(observed_pca$ind$coord, as.data.frame(expected_pca$ind$coord))
  expect_equivalent(observed_pca$ind$cos2, as.data.frame(expected_pca$ind$cos2))
  expect_equivalent(observed_pca$ind$contrib, as.data.frame(expected_pca$ind$contrib))

  # Supplementary individuals
  expect_equal(observed_pca$ind.sup$coord, as.data.frame(expected_pca$ind.sup$coord))
  expect_equal(observed_pca$ind.sup$cos2, as.data.frame(expected_pca$ind.sup$cos2))

  # Active Variables
  expect_equivalent(observed_pca$var$coord, as.data.frame(expected_pca$var$coord))
  expect_equal(observed_pca$var$cor, as.data.frame(expected_pca$var$cor))
  expect_equivalent(observed_pca$var$cos2, as.data.frame(expected_pca$var$cos2))
  expect_equivalent(observed_pca$var$contrib, as.data.frame(expected_pca$var$contrib))

  # Supplementary Variables
  expect_equal(observed_pca$quanti.sup$coord, as.data.frame(expected_pca$quanti.sup$coord))
  expect_equal(observed_pca$var.sup$coord, expected_pca$var.sup$coord)

  # Call
  expect_equal(observed_pca$call$row.w, expected_pca$call$row.w)
  expect_equal(observed_pca$call$col.w, expected_pca$call$col.w)
  expect_equal(observed_pca$call$scale.unit, expected_pca$call$scale.unit)
  expect_equal(observed_pca$call$ncp, expected_pca$call$ncp)
  expect_equal(observed_pca$call$centre, expected_pca$call$centre)
  expect_equal(observed_pca$call$ecart.type, expected_pca$call$ecart.type)
  expect_equal(observed_pca$call$X, expected_pca$call$X)
  expect_equal(observed_pca$call$row.w.init, expected_pca$call$row.w.init)
  expect_equal(observed_pca$call$ind.sup, expected_pca$call$ind.sup)
  expect_equal(observed_pca$call$quanti.sup, expected_pca$call$quanti.sup)
})
