test_that("design_select.random selects one unlabeled ID", {
  set.seed(42)
  X <- matrix(runif(100), nrow = 10)
  w <- rep(1, 10)
  labeled_id <- 1:5
  unlabeled_id <- 6:10

  selected <- design_select.random(method, X, w, labeled_id, unlabeled_id)

  expect_true(length(selected) == 1)
  expect_true(selected %in% unlabeled_id)
})

test_that("design_select.default throws an error for unknown method", {
  X <- matrix(runif(100), nrow = 10)
  w <- rep(1, 10)
  labeled_id <- 1:5
  unlabeled_id <- 6:10

  method_obj <- structure(list(), class = "entropy")

  expect_error(
    design_select(method_obj, X, w, labeled_id, unlabeled_id),
    "Unknown method 'entropy'"
  )
})
