test_that("seq_ana runs correctly on simulated binomial data", {
  set.seed(1)

  b <- c(-2, 2, 1, 1, 0)
  gen_data <- function(N, b) {
    p <- length(b)
    X <- cbind(rep(1, N), matrix(rnorm(N * (p - 1)), ncol = p - 1))
    Xb <- X %*% b
    p1 <- 1 / (1 + exp(-Xb))
    Y <- rbinom(n = N, size = 1, prob = p1)
    data <- data.frame(X, Y)
  }

  df <- gen_data(5000, b)

  fit <- seq_ana(
    data = df,
    interest = Y ~ X1 + X2 + X3 - 1,
    nuisance = Y ~ . - 1,
    init_N = 50,
    d1 = 0.3,
    alpha = 0.05,
    family = binomial(),
    alternative = "two.sided",
    adaptive = "D.opt",
    verbose = 0,
    max_try = 1000
  )

  expect_s3_class(fit, "seq.fit")
  expect_true(fit$Nj >= 50)
  expect_true(all(c("mu", "Sigma", "coef_path") %in% names(fit)))
})
