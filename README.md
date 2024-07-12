
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjd3stl

<!-- badges: start -->
<!-- badges: end -->

R interface giving access to STL (Loess based time series decomposition)
as implemented in JDemetra+ v3.x.

## Installation

Running rjd3 packages requires **Java 17 or higher**. How to set up such
a configuration in R is explained
[here](https://jdemetra-new-documentation.netlify.app/#Rconfig)

### Latest release

To get the current stable version (from the latest release):

- From GitHub:

``` r
# install.packages("remotes")
remotes::install_github("rjdverse/rjd3toolkit@*release")
remotes::install_github("rjdverse/rjd3highfreq@*release")
remotes::install_github("rjdverse/rjd3stl@*release")
```

- From [r-universe](https://rjdverse.r-universe.dev/rjd3stl):

``` r
install.packages("rjd3stl", repos = c("https://rjdverse.r-universe.dev", "https://cloud.r-project.org"))
```

### Development version

You can install the development version of **rjd3stl** from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rjdverse/rjd3stl")
```

## Package Maintenance and contributing

Any contribution is welcome and should be done through pull requests
and/or issues. pull requests should include **updated tests** and
**updated documentation**. If functionality is changed, docstrings
should be added or updated.

## Licensing

The code of this project is licensed under the [European Union Public
Licence
(EUPL)](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12).
