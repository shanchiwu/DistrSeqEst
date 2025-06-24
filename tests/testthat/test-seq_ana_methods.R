test_that("print and summary for seq.fit work correctly", {
  fit <- readRDS(testthat::test_path("fixtures/fit_seq.rds"))

  expect_output(print(fit), "Object of class <seq.fit>")
  expect_s3_class(summary(fit), "summary.seq.fit")
  expect_output(print(summary(fit)), "Summary of Sequential Estimation")
})
