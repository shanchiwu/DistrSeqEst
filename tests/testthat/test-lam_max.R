test_that("lam_max returns correct largest eigenvalue for symmetric matrix", {
  A <- matrix(c(2, -1, -1, 2), nrow = 2)
  expect_equal(lam_max(A), 3)
})

test_that("lam_max symmetrizes the input matrix", {
  A <- matrix(c(1, 2, 3, 4), nrow = 2)
  A_sym <- (A + t(A)) / 2
  expect_equal(lam_max(A), max(eigen(A_sym, only.values = TRUE)$values))
})

test_that("lam_max works for identity matrix", {
  A <- diag(5)
  expect_equal(lam_max(A), 1)
})

test_that("lam_max works for zero matrix", {
  A <- matrix(0, 3, 3)
  expect_equal(lam_max(A), 0)
})

test_that("lam_max handles small numerical perturbations", {
  A <- matrix(c(1, 1e-10, -1e-10, 1), nrow = 2)
  expect_equal(lam_max(A), 1, tolerance = 1e-8)
})

test_that("lam_max returns numeric scalar", {
  A <- matrix(runif(16), 4, 4)
  result <- lam_max(A)
  expect_type(result, "double")
  expect_length(result, 1)
})

#-------- Error Case --------
test_that("lam_max fails gracefully on non-square matrix", {
  A <- matrix(1:6, nrow = 2)
  expect_error(lam_max(A))
})

test_that("lam_max fails gracefully on non-numeric input", {
  A <- matrix(letters[1:9], nrow = 3)
  expect_error(lam_max(A))
})

test_that("lam_max fails on NA matrix", {
  A <- matrix(c(1, NA, 3, 4), 2)
  expect_error(lam_max(A))
})

