
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

You can install the development version of `pva4jecs` from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("fabregithub/pva4jecs")
```

If you are developing the package locally, clone the repository and load
it with `devtools`:

``` r
# install.packages("devtools")
devtools::load_all()
```

## Quick start

This example uses the included `dataX` dataset.

``` r
library(pva4jecs)

data(dataX)

# Inspect the example data
dim(dataX)
head(dataX)

# Choose a candidate number of end-members
CV_plot(dataX)

# Fit the model
fit <- PVA(dataX, k = 3, N = 10)

# Inspect the returned matrices
fit$A0
fit$F0

# Check measured vs. back-calculated values
CD_plot(dataX, k = 3)
```

## Step-by-step workflow

### Step 1. Prepare your input data

Your input should be a numeric matrix or data frame. In most workflows,
rows represent observations or samples and columns represent measured
variables.

Before running PVA, check that your data are numeric and do not contain
missing values:

``` r
# Example using the included dataset
data(dataX)

X <- dataX

# Check dimensions
dim(X)

# Check column names
colnames(X)

# Check for missing values
anyNA(X)

# Keep only numeric columns if your data frame contains ID or grouping columns
X <- X[, vapply(X, is.numeric, logical(1))]

# Convert to matrix if needed
X <- as.matrix(X)
```

If your dataset contains sample IDs, grouping variables, dates, or other
metadata, keep them in a separate object and pass only the numeric
measurement matrix to the PVA functions.

``` r
# Example structure for user data
metadata <- my_data[, c("sample_id", "group")]
X <- my_data[, !(names(my_data) %in% c("sample_id", "group"))]
X <- as.matrix(X)
```

### Step 2. Inspect candidate numbers of end-members

Use `CV_plot()` to inspect cumulative variance and decide which values
of `k` are worth testing.

``` r
CV_plot(X)
```

The value of `k` is the number of end-members. Start with a small range
of candidate values and compare the results. In practice, the final
choice of `k` should consider both model fit and whether the resulting
end-members are interpretable.

### Step 3. Fit a PVA model

Use `PVA()` to estimate end-member profiles and scores.

``` r
fit <- PVA(X, k = 3, N = 10)
```

Arguments:

- `X`: numeric input matrix or data frame.
- `k`: number of end-members.
- `N`: maximum number of trials or iterations.

The returned object is a list with two matrices:

``` r
names(fit)

A0 <- fit$A0
F0 <- fit$F0

dim(A0)
dim(F0)
```

`A0` and `F0` are the main outputs from the PVA model. Use them for
interpretation, downstream analysis, and export.

### Step 4. Compare observed and predicted values

Use `CD_plot()` to create coefficient of determination plots for a
selected value of `k`.

``` r
CD_plot(X, k = 3)
```

This plot compares measured values with back-calculated values. It is
useful for checking how well a chosen number of end-members reproduces
the observed data.

### Step 5. Compare multiple values of `k`

It is often helpful to run the model for several candidate values of
`k`.

``` r
candidate_k <- 2:5

fits <- lapply(candidate_k, function(k) {
  PVA(X, k = k, N = 10)
})

names(fits) <- paste0("k", candidate_k)

# Inspect one model
fits$k3$A0
fits$k3$F0
```

You can also create diagnostic plots for each candidate value:

``` r
for (k in candidate_k) {
  print(CD_plot(X, k = k))
}
```

### Step 6. Export model results

Save the model outputs as CSV files if you want to inspect them outside
R or share them with collaborators.

``` r
fit <- PVA(X, k = 3, N = 10)

write.csv(fit$A0, "pva_A0_k3.csv", row.names = TRUE)
write.csv(fit$F0, "pva_F0_k3.csv", row.names = TRUE)
```

You can also save the full fitted object as an RDS file:

``` r
saveRDS(fit, "pva_fit_k3.rds")

# Later:
fit <- readRDS("pva_fit_k3.rds")
```

## Example data

The package includes three example datasets:

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

## Troubleshooting

### `object '.data' not found` or dependency-related check errors

If you update plotting code that uses `.data`, make sure `rlang` is
listed in `Imports` and that documentation has been regenerated:

``` r
source("tools/update-description.R")
devtools::document()
devtools::check()
```

### `there is no package called ...`

Install missing development dependencies:

``` r
install.packages(c(
  "devtools",
  "dplyr",
  "ggplot2",
  "ggpubr",
  "rlang",
  "tidyr",
  "testthat",
  "knitr",
  "rmarkdown"
))
```

### Results change between runs

Some parts of the PVA workflow may depend on initialization or trial
settings. For reproducible examples, set a seed before fitting:

``` r
set.seed(123)
fit <- PVA(X, k = 3, N = 10)
```

## Development

`README.md` is generated from `README.Rmd`. Edit `README.Rmd`, then
rebuild `README.md`.

A typical development sequence is:

``` r
# 1. Regenerate DESCRIPTION from the package metadata script
source("tools/update-description.R")

# 2. Regenerate NAMESPACE and man/*.Rd files from roxygen comments
devtools::document()

# 3. Render README.Rmd into README.md
devtools::build_readme()

# 4. Run tests
devtools::test()

# 5. Run a package check
devtools::check()
```

Generated files such as `README.md`, `NAMESPACE`, and files in `man/`
should be regenerated from source rather than edited directly.

Before committing, check the files that changed:

``` bash
git status
```

Common files to commit after documentation updates include:

``` bash
git add DESCRIPTION NAMESPACE README.Rmd README.md R/ man/ tests/
git commit -m "Update documentation and README"
```

Do not commit local build/check artifacts such as:

``` text
pva4jecs.Rcheck/
pva4jecs_*.tar.gz
.DS_Store
.Rproj.user/
```

## License

MIT (c) Shoji F. Nakayama
