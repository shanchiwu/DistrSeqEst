library(MASS)  # for ginv()

test_that("inv() returns correct inverse for well-conditioned matrix", {
  A <- matrix(c(2, 1, 1, 2), nrow = 2)
  A_inv_expected <- solve(A)
  A_inv_computed <- inv(A)

  expect_equal(A_inv_computed, A_inv_expected)
})

test_that("inv() falls back to ginv() for ill-conditioned matrix", {
  # Make a nearly singular matrix
  A <- matrix(c(1, 1, 1, 1 + 1e-14), nrow = 2)
  expect_warning({
    G <- inv(A)
    expect_equal(G, ginv(A), tolerance = 1e-8)
  }, regexp = "ill-conditioned")
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
