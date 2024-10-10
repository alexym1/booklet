observed_mfa <- facto_mfa(iris[, -5], ncp = 4, groups = c(2, 2))
expected_mfa <- MFA(iris[, -5], group = c(2, 2), ncp = 4, graph = FALSE)

test_that("Testing facto_mfa() - separate.analyses", {
  observed_analysis <- observed_mfa$separate.analyses
  expected_analysis <- expected_mfa$separate.analyses

  # Eig
  expect_equal(observed_analysis$Gr1$eig$eigenvalue, as.vector(expected_analysis$Gr1$eig[, 1]))
  expect_equal(observed_analysis$Gr2$eig$eigenvalue, as.vector(expected_analysis$Gr2$eig[, 1]))

  # Active individuals
  expect_equal(as.vector(observed_analysis$Gr1$ind$coord), as.vector(expected_analysis$Gr1$ind$coord))
  expect_equal(as.vector(observed_analysis$Gr1$ind$cos2), as.vector(expected_analysis$Gr1$ind$cos2))
  expect_equal(as.vector(observed_analysis$Gr1$ind$contrib), as.vector(expected_analysis$Gr1$ind$contrib))

  expect_equal(as.vector(observed_analysis$Gr2$ind$coord), as.vector(expected_analysis$Gr2$ind$coord))
  expect_equal(as.vector(observed_analysis$Gr2$ind$cos2), as.vector(expected_analysis$Gr2$ind$cos2))
  expect_equal(as.vector(observed_analysis$Gr2$ind$contrib), as.vector(expected_analysis$Gr2$ind$contrib))

  # Active Variables
  expect_equal(as.vector(observed_analysis$Gr1$var$coord), as.vector(expected_analysis$Gr1$var$coord))
  expect_equal(as.vector(observed_analysis$Gr1$var$cor), as.vector(expected_analysis$Gr1$var$cor))
  expect_equal(as.vector(observed_analysis$Gr1$var$cos2), as.vector(expected_analysis$Gr1$var$cos2))
  expect_equal(as.vector(observed_analysis$Gr1$var$contrib), as.vector(expected_analysis$Gr1$var$contrib))

  expect_equal(as.vector(observed_analysis$Gr2$var$coord), as.vector(expected_analysis$Gr2$var$coord))
  expect_equal(as.vector(observed_analysis$Gr2$var$cor), as.vector(expected_analysis$Gr2$var$cor))
  expect_equal(as.vector(observed_analysis$Gr2$var$cos2), as.vector(expected_analysis$Gr2$var$cos2))
  expect_equal(as.vector(observed_analysis$Gr2$var$contrib), as.vector(expected_analysis$Gr2$var$contrib))

  # Call
  expect_equal(observed_analysis$Gr1$call$row.w, expected_analysis$Gr1$call$row.w)
  expect_equal(observed_analysis$Gr1$call$col.w, expected_analysis$Gr1$call$col.w)
  expect_equal(observed_analysis$Gr1$call$scale.unit, expected_analysis$Gr1$call$scale.unit)
  expect_equal(observed_analysis$Gr1$call$ncp, expected_analysis$Gr1$call$ncp)
  expect_equal(as.vector(observed_analysis$Gr1$call$centre), expected_analysis$Gr1$call$centre)
  # expect_equal(observed_analysis$Gr1$call$ecart.type, expected_analysis$Gr1$call$ecart.type)

  rownames(observed_analysis$Gr1$call$X) <- as.numeric(rownames(observed_analysis$Gr1$call$X))
  expect_equal(observed_analysis$Gr1$call$X, expected_analysis$Gr1$call$X)
  expect_equal(observed_analysis$Gr1$call$row.w.init, expected_analysis$Gr1$call$row.w.init)

  expect_equal(observed_analysis$Gr2$call$row.w, expected_analysis$Gr2$call$row.w)
  expect_equal(observed_analysis$Gr2$call$col.w, expected_analysis$Gr2$call$col.w)
  expect_equal(observed_analysis$Gr2$call$scale.unit, expected_analysis$Gr2$call$scale.unit)
  expect_equal(observed_analysis$Gr2$call$ncp, expected_analysis$Gr2$call$ncp)
  expect_equal(as.vector(observed_analysis$Gr2$call$centre), expected_analysis$Gr2$call$centre)
  # expect_equal(observed_analysis$Gr2$call$ecart.type, expected_analysis$Gr2$call$ecart.type)

  rownames(observed_analysis$Gr2$call$X) <- as.numeric(rownames(observed_analysis$Gr2$call$X))
  expect_equal(observed_analysis$Gr2$call$X, expected_analysis$Gr2$call$X)
  expect_equal(observed_analysis$Gr2$call$row.w.init, expected_analysis$Gr2$call$row.w.init)
})

test_that("Testing facto_mfa() - global", {

})
