test_that("seq_ana_parallel runs and returns seq.fit", {
  set.seed(123)
  df <- data.frame(
    y = rbinom(10000, 1, 0.5),
    x1 = rnorm(10000),
    x2 = rnorm(10000),
    x3 = rnorm(10000)
  )

  res <- seq_ana_parallel(
    data = df,
    interest = y ~ x1 + x2 - 1,
    nuisance = ~ x3,
    init_N = 20,
    d1 = 0.3,
    d2 = 0.05,
    family = binomial(),
    alternative = "two.sided",
    adaptive = "random",
    verbose = 3,
    backend = "none",
    cores = 1
  )

  expect_type(res, "list")
  #expect_true(any(sapply(res[[1]], inherits, "seq.fit")))
})

test_that("seq_ana_parallel errors when backend is mismatched", {
  df <- data.frame(y = rbinom(100, 1, 0.5), x1 = rnorm(100), x2 = rnorm(100))

  expect_error(
    seq_ana_parallel(
      data = df, interest = y ~ x1 + x2 - 1,
      init_N = 10, d1 = 0.3,
      backend = "none", cores = 2
    ),
    "did not register a parallel backend"
  )
})

test_that("seq_ana_parallel errors when beta is missing for beta.protect", {
  df <- data.frame(y = rbinom(100, 1, 0.5), x1 = rnorm(100), x2 = rnorm(100))

  expect_error(seq_ana_parallel(data = df, interest = Y ~ x1 - 1,
                                init_N = 50, d1 = 0.3,
                                alternative = "beta.protect"),
               "`beta` should be provided if `alternative` is 'beta.protect'")
})


test_that("seq_ana_parallel errors on doMC in non-Unix", {
  skip_if(.Platform$OS.type == "unix")

  df <- data.frame(y = rbinom(100, 1, 0.5), x1 = rnorm(100), x2 = rnorm(100))
  expect_error(
    seq_ana_parallel(df, y ~ x1 + x2 - 1, init_N = 20, d1 = 0.3,
                     backend = "doMC", cores = parallel::detectCores() -1),
    "doMC is only available"
  )
})

test_that("seq_ana_parallel registers doParallel backend", {
  skip_on_ci()
  skip_if_not_installed("doParallel")

  df <- data.frame(y = rbinom(10000, 1, 0.5), x1 = rnorm(10000), x2 = rnorm(10000))

  expect_silent(
    seq_ana_parallel(df, y ~ x1 + x2 - 1,
                     init_N = 20, d1 = 0.3, family = binomial(),
                     backend = "doParallel", cores = parallel::detectCores() -1, verbose = 0)
  )
})

test_that("seq_ana_parallel registers doMC backend", {
  skip_on_ci()
  skip_if(.Platform$OS.type != "unix")
  skip_if_not_installed("doMC")

  df <- data.frame(y = rbinom(10000, 1, 0.5), x1 = rnorm(10000), x2 = rnorm(10000))

  expect_silent(
    seq_ana_parallel(df, y ~ x1 + x2 - 1,
                     init_N = 20, d1 = 0.3, family = binomial(),
                     backend = "doMC", cores = parallel::detectCores() -1, verbose = 0)
  )
})

test_that("seq_ana_parallel errors on unknown backend", {
  df <- data.frame(y = rbinom(100, 1, 0.5), x1 = rnorm(100), x2 = rnorm(100))
  expect_error(
    seq_ana_parallel(df, y ~ x1 + x2 - 1, init_N = 20, d1 = 0.3,
                     backend = "CUDA", cores = parallel::detectCores() -1),
    "'arg' should be one of"
  )
})
