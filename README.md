
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FactoMineR2 <a href=#><img src='man/figures/sticker.png' align="right" width="120" /></a>

<!-- badges: start -->

![](https://img.shields.io/badge/github%20version-0.1.0-orange.svg)
[![R-CMD-check](https://github.com/alexym1/FactoMineR2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/alexym1/FactoMineR2/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/alexym1/FactoMineR2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/alexym1/FactoMineR2?branch=main)
<!-- badges: end -->

> Multivariate exploratory data analysis in R

## Overview

`FactoMineR2` is a ground-up rewrite of
[FactoMineR](https://github.com/husson/FactoMineR/tree/master) that
provides a set of functions for multivariate exploratory data analysis.
It is designed to be a more user-friendly version of `FactoMineR`. The
main goal was to make the package more intuitive and easier to use. The
package is still under development, and some functions are not yet
implemented. However, the main functions are already available.

## Installation

`FactoMineR2` can be installed from CRAN as follows:

``` r
install.packages("fusionchartsR")
```

The latest version can be installed from GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github("alexym1/FactoMineR2")
```

## Example

``` r
library(FactoMineR2)

# Get active individuals
X_active <- iris[,-5] |> standardize(type = "norm")
X_active |> head()
#>      Sepal.Length Sepal.Width Petal.Length Petal.Width
#> [1,]   -0.8976739  1.01560199    -1.335752   -1.311052
#> [2,]   -1.1392005 -0.13153881    -1.335752   -1.311052
#> [3,]   -1.3807271  0.32731751    -1.392399   -1.311052
#> [4,]   -1.5014904  0.09788935    -1.279104   -1.311052
#> [5,]   -1.0184372  1.24503015    -1.335752   -1.311052
#> [6,]   -0.5353840  1.93331463    -1.165809   -1.048667
```

``` r
# Get eigenvectors
vectors <- X_active |> eigvectors()
vectors
#>                   Dim.1       Dim.2      Dim.3      Dim.4
#> Sepal.Length  0.5210659 -0.37741762  0.7195664  0.2612863
#> Sepal.Width  -0.2693474 -0.92329566 -0.2443818 -0.1235096
#> Petal.Length  0.5804131 -0.02449161 -0.1421264 -0.8014492
#> Petal.Width   0.5648565 -0.06694199 -0.6342727  0.5235971
```

``` r
# Get principal components
X_active |> pca_ind_coords(vectors) |> head()
#>         Dim.1      Dim.2       Dim.3        Dim.4
#> [1,] 2.257141  0.4784238 -0.12727962 -0.024087508
#> [2,] 2.074013 -0.6718827 -0.23382552 -0.102662845
#> [3,] 2.356335 -0.3407664  0.04405390 -0.028282305
#> [4,] 2.291707 -0.5953999  0.09098530  0.065735340
#> [5,] 2.381863  0.6446757  0.01568565  0.035802870
#> [6,] 2.068701  1.4842053  0.02687825 -0.006586116
```

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/alexym1/FactoMineR2/.github/CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
