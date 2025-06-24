test_that("progress_controller init returns progress_bar object when expected", {
  pb <- progress_controller(
    type = "init",
    verbose = 2,
    total = 10
  )
  expect_s3_class(pb, "progress_bar")
})

test_that("progress_controller init returns NULL when verbose = 0", {
  pb <- progress_controller(
    type = "init",
    verbose = 0,
    total = 10
  )
  expect_null(pb)
})

test_that("progress_controller tick does not error when pb_env is valid", {
  pb <- progress_controller(type = "init", verbose = 1, total = 1)
  expect_silent(progress_controller(type = "tick", verbose = 1, pb_env = pb, tokens = list()))
})

test_that("progress_controller done does not error when pb_env is valid", {
  pb <- progress_controller(type = "init", verbose = 1, total = 1)
  expect_silent(progress_controller(type = "done", pb_env = pb))
})
