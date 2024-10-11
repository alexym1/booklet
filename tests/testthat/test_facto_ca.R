test_that("Testing facto_ca()", {
  observed_ca <- facto_ca(mtcars, row_sup = 5:8, col_sup = c(1,3:7), ncp = 5)
  expected_ca <- CA(mtcars, row.sup = 5:8, col.sup = c(1,3:7), graph = FALSE, ncp = 5)

  # Eig
  expect_equal(observed_ca$eig[,1], as.vector(expected_ca$eig[,1]))
  expect_equal(observed_ca$eig[,2], as.vector(expected_ca$eig[,2]))
  expect_equal(observed_ca$eig[,3], as.vector(expected_ca$eig[,3]))

  # Active individuals
  expect_equal(observed_ca$row$coord, expected_ca$row$coord)
  expect_equal(observed_ca$row$cos2, expected_ca$row$cos2)
  expect_equal(observed_ca$row$contrib, expected_ca$row$contrib)
  expect_equal(observed_ca$row$inertia, expected_ca$row$inertia)

  # Supplementary individuals
  expect_equal(observed_ca$row.sup$coord, expected_ca$row.sup$coord)
  expect_equal(observed_ca$row.sup$cos2, expected_ca$row.sup$cos2)

  # Active Variables
  expect_equal(observed_ca$col$coord, expected_ca$col$coord)
  expect_equal(observed_ca$col$cor, expected_ca$col$cor)
  expect_equal(observed_ca$col$cos2, expected_ca$col$cos2)
  expect_equal(observed_ca$col$contrib, expected_ca$col$contrib)
  expect_equal(observed_ca$col$inertia, expected_ca$col$inertia)

  # Supplementary Variables
  expect_equal(observed_ca$col.sup$coord, expected_ca$col.sup$coord)
  expect_equal(observed_ca$col.sup$cos2, expected_ca$col.sup$cos2)

  # Call
  expect_equal(observed_ca$call$X, expected_ca$call$X)
  expect_equal(observed_ca$call$marge.col, expected_ca$call$marge.col)
  expect_equal(observed_ca$call$marge.row, expected_ca$call$marge.row)
  expect_equal(observed_ca$call$ncp, expected_ca$call$ncp)
  expect_equal(observed_ca$call$row.w, expected_ca$call$row.w)
  expect_equal(observed_ca$call$excl, expected_ca$call$excl)
  expect_equal(observed_ca$call$Xtot, expected_ca$call$Xtot)
  expect_equal(observed_ca$call$N, expected_ca$call$N)
  expect_equal(observed_ca$call$row.sup, expected_ca$call$row.sup)
  expect_equal(observed_ca$call$col.sup, expected_ca$call$col.sup)
})
