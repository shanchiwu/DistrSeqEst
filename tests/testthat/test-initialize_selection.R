test_that("initialize_selection returns correct structure and size", {
  set.seed(123)
  X <- matrix(rnorm(100), nrow = 20, ncol = 5)
  res <- initialize_selection(X, init_N = 5, verbose = 0)

  expect_type(res, "list")
  expect_named(res, c("labeled_id", "unlabeled_id"))
  expect_length(res$labeled_id, 5)
  expect_true(all(res$labeled_id %in% seq_len(nrow(X))))
  expect_true(length(intersect(res$labeled_id, res$unlabeled_id)) == 0)
  expect_equal(length(res$labeled_id) + length(res$unlabeled_id), nrow(X))
})

test_that("initialize_selection returns full rank matrix", {
  set.seed(456)
  X <- matrix(rnorm(200), nrow = 40, ncol = 5)
  res <- initialize_selection(X, init_N = 5, verbose = 0)
  X_init <- X[res$labeled_id, ]
  expect_equal(qr(X_init)$rank, ncol(X))
})

test_that("initialize_selection errors when no full-rank subset found", {
  set.seed(789)
  X <- matrix(1, nrow = 10, ncol = 3)  # All rows identical, rank = 1
  expect_error(initialize_selection(X, init_N = 3, max_try = 10, verbose = 0),
               "Failed to find full-rank initial design matrix")
})

test_that("initialize_selection works with init_N == nrow(X)", {
  set.seed(101)
  X <- diag(5)
  res <- initialize_selection(X, init_N = 5, verbose = 0)
  expect_length(res$unlabeled_id, 0)
  expect_equal(length(res$labeled_id), 5)
})

test_that("initialize_selection works with init_N == 1 and p == 1", {
  set.seed(202)
  X <- matrix(rnorm(10), ncol = 1)
  res <- initialize_selection(X, init_N = 1, verbose = 0)
  expect_equal(length(res$labeled_id), 1)
})
