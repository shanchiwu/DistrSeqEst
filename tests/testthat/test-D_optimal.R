test_that("D.opt selects more informative point", {
  X <- matrix(c(1, 0,   # labeled
                0, 1,
                10, 0,  # unlabeled A
                0, 1),  # unlabeled B
              ncol = 2, byrow = TRUE)
  w <- rep(1, 4)
  labeled_id <- 1:2
  unlabeled_id <- 3:4

  result_chol <- design_select.D.opt.chol(x, X, w, labeled_id, unlabeled_id)
  result_inv <- design_select.D.opt.inv(x, X, w, labeled_id, unlabeled_id)

  expect_equal(result_chol, 3)
  expect_equal(result_inv, 3)
  expect_equal(result_chol, result_inv)
})

test_that("D.opt returns a valid unlabeled index", {
  set.seed(123)
  X <- matrix(rnorm(100), nrow = 10)      # 10 samples, 10 features
  w <- runif(10, 0.5, 1.5)                # sample weights
  labeled_id <- 1:5
  unlabeled_id <- 6:10

  result_chol <- design_select.D.opt.chol(x, X, w, labeled_id, unlabeled_id)
  result_inv <- design_select.D.opt.inv(x, X, w, labeled_id, unlabeled_id)

  # Should return an index from unlabeled_id
  expect_true(result_chol %in% unlabeled_id)
  expect_true(result_inv %in% unlabeled_id)
  expect_equal(result_chol, result_inv)
})

test_that("D.opt handles 1D feature space", {
  X <- matrix(seq(1, 10), ncol = 1)       # 10 samples, 1 feature
  w <- rep(1, 10)
  labeled_id <- 1:5
  unlabeled_id <- 6:10

  result_chol <- design_select.D.opt.chol(x, X, w, labeled_id, unlabeled_id)
  result_inv <- design_select.D.opt.inv(x, X, w, labeled_id, unlabeled_id)

  expect_true(result_chol %in% unlabeled_id)
  expect_true(result_inv %in% unlabeled_id)
  expect_equal(result_chol, result_inv)
})

test_that("D.opt avoids crash with minimal input", {
  # 3 samples, 2 features
  X <- matrix(c(1, 0,
                0, 1,
                1, 1.5), ncol = 2, byrow = TRUE)
  w <- rep(1, 3)
  labeled_id <- 1
  unlabeled_id <- 2:3

  expect_silent({
    result_chol <- design_select.D.opt.chol(x, X, w, labeled_id, unlabeled_id)
    result_inv <- design_select.D.opt.inv(x, X, w, labeled_id, unlabeled_id)

    expect_true(result_chol %in% unlabeled_id)
    expect_true(result_inv %in% unlabeled_id)
  })

  expect_equal(result_chol, result_inv)
})
