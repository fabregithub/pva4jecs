
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# pva4jecs

<!-- badges: start -->

<!-- badges: end -->

`pva4jecs` provides R functions for polytopic vector analysis (PVA). It
was created as an R translation of Python code for PVA developed by
Dr. Yang Ju and includes helpers for data transformation, end-member
selection, oblique rotation, back-transformation, model diagnostics, and
plotting.

The package includes example datasets (`dataX`, `orgA0`, and `orgF0`)
that can be used to try the workflow.

## Installation

You can install the development version of `pva4jecs` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("fabregithub/pva4jecs")
```

## Example

Load the package and the example dataset:

``` r
library(pva4jecs)

data(dataX)
head(dataX)
```

Use a cumulative variance plot to inspect candidate numbers of
end-members:

``` r
CV_plot(dataX)
```

Fit a PVA model. Here `k` is the number of end-members and `N` is the
maximum number of trials/iterations:

``` r
fit <- PVA(dataX, k = 3, N = 10)

fit$A0
fit$F0
```

Create a coefficient of determination plot for a selected value of `k`:

``` r
CD_plot(dataX, k = 3)
```

## Main functions

- `PVA()` fits a polytopic vector analysis model and returns named
  matrices `A0` and `F0`.
- `CV_plot()` creates a cumulative variance plot to help choose the
  number of end-members.
- `CD_plot()` creates coefficient of determination plots comparing
  measured and back-calculated values.
- `estimate_X()` obtains an estimated data matrix for a selected number
  of end-members.
- `row_sum()`, `range_transform()`, and `evlt()` provide data
  transformations used by the PVA workflow.
- `scale_back()`, `resultant_oblique()`, `inspect_extreme()`,
  `negative_A0()`, `negative_F0()`, and `DENEG()` are helper functions
  used by the PVA algorithm.

## Example data

The package includes:

- `orgA0`: example loading matrix.
- `orgF0`: example score matrix.
- `dataX`: sample data generated from `orgA0` and `orgF0`.

``` r
data(orgA0)
data(orgF0)
data(dataX)

str(orgA0)
str(orgF0)
str(dataX)
```

## Development

Regenerate the package metadata, documentation, README, and checks with:

``` r
source("tools/update-description.R")
devtools::document()
devtools::build_readme()
devtools::test()
devtools::check()
```

Generated files such as `README.md`, `NAMESPACE`, and files in `man/`
should be regenerated from source rather than edited directly.

## License

MIT (c) Shoji F. Nakayama
