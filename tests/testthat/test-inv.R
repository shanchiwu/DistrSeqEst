library(MASS)  # for ginv()

test_that("inv() returns correct inverse for well-conditioned matrix", {
  A <- matrix(c(2, 1, 1, 2), nrow = 2)
  A_inv_expected <- solve(A)
  A_inv_computed <- inv(A)

  expect_equal(A_inv_computed, A_inv_expected)
})

test_that("inv() applies ridge regularization for mildly ill-conditioned matrix", {
  A <- diag(c(1, 1e-10))  # rcond ≈ 1e-10

  # Run and expect warning about ridge
  expect_warning({
    G <- inv(A)
    lambda_adaptive <- 1e-4 * mean(diag(A))
    A_stable <- A + diag(lambda_adaptive, 2)
    expect_equal(G, solve(A_stable), tolerance = 1e-6)
  }, regexp = "ridge")
})

test_that("inv() falls back to ginv() for ill-conditioned matrix", {
  # Make a nearly singular matrix
  A <- matrix(c(1, 1, 1, 1 + 1e-14), nrow = 2)
  expect_warning({
    G <- inv(A)
    expect_equal(G, ginv(A), tolerance = 1e-8)
  }, regexp = "generalized inverse")
})

test_that("inv() preserves dimnames", {
  A <- matrix(c(4, 7, 2, 6), nrow = 2,
              dimnames = list(c("r1", "r2"), c("c1", "c2")))
  A_inv <- inv(A)
  expect_equal(dimnames(A_inv), dimnames(A))
})

test_that("inv() throws error on non-matrix input", {
  x <- c(1, 2, 3)
  expect_error(inv(x), regexp = "requires a matrix")
})
