# FactoMineR2 0.1.1

* `standardize()` now works as expected with `scale = FALSE` (#1)

* `standardize_norm()` has replaced `standardize(type = "norm", ...)`

* Codecov badge has been fixed and now use `master` instead of `main` branch for coding coverage.


# FactoMineR2 0.1.0

* `get_eig()` is a wrapper function that returns eigenvalues and eigenvectors.

* `standardize()` is a wrapper function that standardizes the data.

* `pca_ind_*()` allows to compute coordinates, cos2 and contribution for active individuals in PCA.

* `pca_var_*()` allows to compute coordinates, cos2 and contribution for active variables in PCA.

* Unit tests were designed for functions mentioned above.

* Website was built and deployed using `pkgdown`.
