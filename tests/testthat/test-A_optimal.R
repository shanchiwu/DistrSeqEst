test_that("A.opt selects more informative point", {
  X <- matrix(c(1, 0,   # labeled
                0, 1,
                10, 0,  # unlabeled A
                0, 1),  # unlabeled B
              ncol = 2, byrow = TRUE)
  w <- rep(1, 4)
  labeled_id <- 1:2
  unlabeled_id <- 3:4

  selected <- design_select.A.opt(x, X, w, labeled_id, unlabeled_id)

  expect_equal(selected, 3)
})

test_that("A.opt returns a valid unlabeled index", {
  set.seed(123)
  X <- matrix(rnorm(100), nrow = 10)      # 10 samples, 10 features
  w <- runif(10, 0.5, 1.5)                # sample weights
  labeled_id <- 1:5
  unlabeled_id <- 6:10

  result <- design_select.A.opt(x, X, w, labeled_id, unlabeled_id)

  expect_true(result %in% unlabeled_id)   # Should return an index from unlabeled_id
})

test_that("A.opt handles 1D feature space", {
  X <- matrix(seq(1, 10), ncol = 1)       # 10 samples, 1 feature
  w <- rep(1, 10)
  labeled_id <- 1:5
  unlabeled_id <- 6:10

  result <- design_select.A.opt(x, X, w, labeled_id, unlabeled_id)

  expect_true(result %in% unlabeled_id)
})

test_that("A.opt is deterministic with fixed input", {
  X <- matrix(rep(1:10, each = 2), ncol = 2)
  w <- rep(1, 10)
  labeled_id <- 1:5
  unlabeled_id <- 6:10

  r1 <- design_select.A.opt(x, X, w, labeled_id, unlabeled_id)
  r2 <- design_select.A.opt(x, X, w, labeled_id, unlabeled_id)

  expect_equal(r1, r2)  # Should be deterministic
})

test_that("A.opt avoids crash with minimal input", {
  X <- matrix(rnorm(6), ncol = 2)         # 3 samples, 2 features
  w <- rep(1, 3)
  labeled_id <- 1
  unlabeled_id <- 2:3

  expect_silent({
    res <- design_select.A.opt(x, X, w, labeled_id, unlabeled_id)
    expect_true(res %in% unlabeled_id)
  })
})
