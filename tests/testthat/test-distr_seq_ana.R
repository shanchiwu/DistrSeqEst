test_that("distr_seq_ana catches invalid input", {
  expect_error(distr_seq_ana(list(mtcars), interest = mpg ~ ., init_N = 10, d1 = 0.2),
               "must be a `data_list` object")

  dl <- gen_data_list(mtcars, mtcars)
  expect_error(distr_seq_ana(dl, interest = mpg ~ ., nuisance = list(NULL),
                             init_N = 10, d1 = 0.2),
               "`nuisance` must be a list")

  expect_error(distr_seq_ana(dl, interest = mpg ~ ., alternative = "beta.protect",
                             init_N = 10, d1 = 0.2),
               "`beta` should be provided")
})

test_that("distr_seq_ana runs on diverse data_list", {
  set.seed(1)

  df_list <- readRDS(test_path("fixtures/df_list.rds"))

  nuisance <- list(
    Y ~ . -1,
    NULL,
    ~ X4 + X5 + X6 -1,
    ~ I(X2^2) + X4:X5 -1,
    ~ log(X3^2) + poly(X4, 2) -1
  )

  gamma <- rep(1 / 5, 5)

  fit <- distr_seq_ana(
    data_list = df_list,
    interest = Y ~ X1 + X2 + X3 - 1,
    nuisance = nuisance,
    init_N = 50,
    model = "glm",
    fit_args = list(family = binomial()),
    gamma = gamma,
    d1 = 0.3,
    d2 = 0.05,
    alpha = 0.05,
    alternative = "two.sided",
    adaptive = "A.opt",
    verbose = 3,
    backend = "none"
  )

  expect_s3_class(fit, "distr.seq.fit")
  expect_equal(length(fit$fits), 5)
  expect_true(all(c("beta_est", "Sigma_est", "N_star") %in% names(fit)))
  expect_true(all(sapply(fit$fits, inherits, "seq.fit")))
})

test_that("doParallel backend works", {
  skip_on_ci()
  skip_if_not_installed("doParallel")

  lib_ok <- tryCatch({
    cl <- parallel::makeCluster(1)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    res <- parallel::clusterEvalQ(cl, requireNamespace("DistrSeqEst", quietly = TRUE))
    isTRUE(res[[1]])
  }, error = function(e) FALSE)

  if (!lib_ok) skip("DistrSeqEst not found in worker library. Skip parallel test.")

  df_list <- readRDS(test_path("fixtures/df_list.rds"))

  interest <- Y ~ X1 + X2 + X3 - 1
  nuisance <- list(
    Y ~ . -1,
    NULL,
    ~ X4 + X5 + X6 -1,
    ~ I(X2^2) + X4:X5 -1,
    ~ log(X3^2) + poly(X4, 2) -1
  )

  gamma <- rep(1 / 5, 5)

  fit <- distr_seq_ana(data_list = df_list, interest = interest, nuisance = nuisance,
                       init_N = 100, model = "glm", fit_args = list(family = binomial()),
                       gamma = gamma, d1 = 0.3, beta = 0.05,
                       alternative = "beta.protect", adaptive = "random",
                       verbose = 0, max_try = 100,
                       cores = 2, backend = "doParallel")
  expect_s3_class(fit, "distr.seq.fit")
})

test_that("doMC backend works", {
  skip_on_ci()
  skip_if(.Platform$OS.type != "unix", "doMC only supported on Unix")
  skip_if_not_installed("doMC")

  df_list <- readRDS(test_path("fixtures/df_list.rds"))

  interest <- Y ~ X1 + X2 + X3 - 1
  nuisance <- list(
    Y ~ . -1,
    NULL,
    ~ X4 + X5 + X6 -1,
    ~ I(X2^2) + X4:X5 -1,
    ~ log(X3^2) + poly(X4, 2) -1
  )

  gamma <- rep(1 / 5, 5)

  fit <- distr_seq_ana(df_list, interest = interest, nuisance = nuisance,
                       init_N = 100, model = "glm", fit_args = list(family = binomial()),
                       gamma = gamma, d1 = 0.3, beta = 0.05,
                       alternative = "beta.protect", adaptive = "random",
                       verbose = 0, max_try = 100,
                       cores = 2, backend = "doMC")
  expect_s3_class(fit, "distr.seq.fit")
})
