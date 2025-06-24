test_that("check_stopped works for two.sided without d2", {
  Sigma <- diag(2)
  theta <- c(0.1, 0.2)
  out <- check_stopped(
    k = 50, N0 = 10, interest_term = 1:2, d1 = 0.1, theta = theta,
    Sigma = Sigma, s = 1, gamma = 1, alpha = 0.05, alternative = "two.sided"
  )
  expect_type(out, "list")
  expect_true(all(c("mu", "st1", "st2", "is.stop") %in% names(out)))
})

test_that("check_stopped works for beta.protect without d2", {
  Sigma <- diag(2)
  theta <- c(0.2, 0.3)
  out <- check_stopped(
    k = 80, N0 = 20, interest_term = 1:2, d1 = 0.05, theta = theta,
    Sigma = Sigma, s = 1, gamma = 1, alpha = 0.05, beta = 0.1, alternative = "beta.protect"
  )
  expect_type(out, "list")
  expect_true(is.logical(out$is.stop))
})

test_that("check_stopped handles auc_var and d2 properly", {
  Sigma <- diag(2)
  theta <- c(0.1, 0.2)
  out <- check_stopped(
    k = 80, N0 = 10, interest_term = 1:2, d1 = 0.1, d2 = 0.05,
    theta = theta, Sigma = Sigma, s = 1, gamma = 1,
    alpha = 0.05, auc_var = 0.0001, alpha2 = 0.05,
    alternative = "two.sided"
  )
  expect_false(is.null(out$st2))
  expect_type(out$is.stop, "logical")
})
