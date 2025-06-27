# DistrSeqEst: Distributed Sequential Estimation Methods for Large-Scale Data

<!-- badges: start -->
[![R-CMD-check](https://github.com/shanchiwu/DistrSeqEst/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shanchiwu/DistrSeqEst/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/shanchiwu/DistrSeqEst/graph/badge.svg)](https://app.codecov.io/gh/shanchiwu/DistrSeqEst)
<!-- badges: end -->


**DistrSeqEst** provides tools for distributed or parallel adaptive sequential estimation in generalized linear models. The package implements the methodology from Chen et al. (2024, *Canadian Journal of Statistics*) and Wang & Chang (2017, *Biometrika*) to efficiently integrate multiple sequential experiments across large-scale datasets.

## Installation

The development version can be installed from GitHub using `devtools`:

```r
# install.packages("devtools")  # if not already installed
devtools::install_github("shanchiwu/DistrSeqEst")
```

## Basic example

The package exposes two main functions: `seq_ana()` for a single sequential analysis and `distr_seq_ana()` for integrating several sequences. Below is a minimal example using `seq_ana`.

```r
library(DistrSeqEst)

# generate a simple logistic dataset
b <- c(-2, 2, 1, 1, 0)

gen_data <- function(N, b) {
  p <- length(b)
  X <- cbind(1, matrix(rnorm(N * (p - 1)), ncol = p - 1))
  p1 <- 1 / (1 + exp(-(X %*% b)))
  data.frame(X, Y = rbinom(N, 1, p1))
}

df <- gen_data(10000, b)

fit <- seq_ana(
  df,
  interest  = Y ~ X1 + X2 + X3 - 1,
  nuisance  = Y ~ . - 1,
  init_N    = 100,
  model     = "glm", 
  fit_args  = list(family = binomial()),
  d1        = 0.3,
  alpha     = 0.05,
  alternative = "two.sided",
  adaptive    = "D.opt",
  verbose     = 0,
  max_try     = 1000
)

summary(fit)
```

For a demonstration of distributed sequential analysis with multiple datasets, see `inst/examples/example_distr_seq_est.R` or the vignette `vignettes/Analysis.Rmd`.


