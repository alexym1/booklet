# Contributing to FactoMineR2

This outlines how to propose a change to FactoMineR2.

### Fixing typos

Small typos or grammatical errors in documentation **CANNOT** be edited directly using
the GitHub web interface. Please, all changes were made with commit and pull request.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it's a problem. If you've found a
bug, create an associated issue and illustrate the bug with a minimal example.

### Pull request process

* 1. Create a git branch from the latest `master` or `main` branch.
* 2. Make changes in your branch.
* 3. Include unit tests with [testthat](https://cran.r-project.org/package=testthat).

    ```
    # Run this command to validate your tests
    testthat::test_dir("tests/testthat")
    ```

* 4. Run `devtools::check()` to ensure that your code meets the package's standards.
     Fix errors, warnings or notes that may appear.

* 5. (optional) New code should follow the tidyverse [style guide](http://style.tidyverse.org).
You can use the [styler](https://CRAN.R-project.org/package=styler) package to
apply these styles, but please don't restyle code that has nothing to do with 
your PR.  
 
*  6. For user-facing changes, add a bullet to the top of `NEWS.md` below the current
development version header describing the changes made followed by your GitHub
username, and links to relevant issue(s)/PR(s).
    ```
    # Update the existing NEWS.md file
    pkgdown::build_site()
    ```
    
* 7. (optional) Bump to version 
* 8. Create a pull request. Don't forget to include the latest bullet of `NEWS.md` in the description of the pull request. Solving issues should be mentioned with `(#<number_of_the_issue>)`


In any case, CI files were included in the repository to check the code.


### Code of Conduct

By participating in this project you agree to abide by its terms.
