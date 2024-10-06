# FactoMineR2 0.4.0

* Add all necessary functions to perform Correspondence Analysis (CA) (#9)

* Replace `standardize()` and `standardize_norm()` by `pca_standardize()` and `pca_standardize_norm()`

* Replace `get_eig()` and `get_weighted_eigen()` by `pca_eigen()` and `pca_weighted_eigen()`

* Update all unit tests for better testing and coverage

* Update `_pkgdown.yml`

* Update `renv.lock` making the package lightweight

* `facto_pca` was updated to return same outputs as `FactoMineR::PCA()`


# FactoMineR2 0.3.0

* Add `facto_mfa()` function to perform Multiple Factor Analysis (MFA) (#7)

* Update unit tests for better testing and coverage

* Update `_pkgdown.yml`


# FactoMineR2 0.2.1

* `get_weighted_eigen()` now returns U matrix as expected (#4)

* `pca_ind_coords()` & `pca_var_coords()` return same signs of the corresponding coordinates as FactoMineR.

* `facto_pca()` has been updated. Now, it returns the same output as `FactoMineR::PCA()`.


# FactoMineR2 0.2.0

* `facto_pca()` is a wrapper function that mimics `FactoMineR::PCA()`.

* `get_weighted_eigen()` calculates the same eigs as FactoMineR, whereas `get_eigen()` calculates the eigs in the unweighted case.

* `eigvalues()` and `eigvectors()` were deprecated.

* `pca_var_cos2()` now works as expected.

* `Comparison.Rmd` has been updated allowing to compute either supplementary individual coordinates or supplementary variable coordinates.


# FactoMineR2 0.1.1

* `standardize()` now works as expected with `scale = FALSE` (#1)

* A new argument called `weights` has been added to `get_eigen()`, `eigvalues()` and `eigvectors()` functions. This argument allows to weight the variables in the PCA.

* `standardize_norm()` has replaced `standardize(type = "norm", ...)`

* Codecov badge has been fixed and now use `master` instead of `main` branch for coding coverage.


# FactoMineR2 0.1.0

* `get_eig()` is a wrapper function that returns eigenvalues and eigenvectors.

* `standardize()` is a wrapper function that standardizes the data.

* `pca_ind_*()` allows to compute coordinates, cos2 and contribution for active individuals in PCA.

* `pca_var_*()` allows to compute coordinates, cos2 and contribution for active variables in PCA.

* Unit tests were designed for functions mentioned above.

* Website was built and deployed using `pkgdown`.
